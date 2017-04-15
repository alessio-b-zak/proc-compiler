module ProcParser where
import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String
import Data.List (intercalate)
import Control.Monad (void)
import System.IO
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as Lexer


type Numeric = Integer
type Var = String
type Pname = String
type DecV = [(Var,Aexp)]
type DecP = [(Pname,Stm)]

data Aexp = N Numeric | V Var | Mult Aexp Aexp
          | Add Aexp Aexp | Sub Aexp Aexp deriving Show

data Bexp = TRUE | FALSE | Neg Bexp | And Bexp Bexp
          | Le Aexp Aexp | Eq Aexp Aexp deriving Show

data Stm = Skip | Ass Var Aexp | Comp Stm Stm
         | If Bexp Stm Stm | While Bexp Stm
         | Block DecV DecP Stm | Call Pname deriving Show


tok :: String -> Parser String
tok t = try (string t <* whitespace)

newLine :: Parser ()
newLine = many (oneOf "\n") *> pure ()

comment :: Parser ()
comment =  try(string "//" *> manyTill anyChar newline *> space *> pure ())
       <|>  try(string "/*" *> manyTill anyChar (string "*/") *> space *> pure ())

whitespace :: Parser ()
whitespace = Lexer.space (void spaceChar) lineComment blockComment
  where
    lineComment  = Lexer.skipLineComment "//"
    blockComment = Lexer.skipBlockComment "/*" "*/"

number :: Parser Numeric
number = (some (oneOf ['0' .. '9']) >>= return . read) <* whitespace


parens :: Parser a -> Parser a
parens p = between (tok "(") (tok ")") p

aexp :: Parser Aexp
aexp = makeExprParser aTerm aOperators

aOperators = [[ InfixL (Mult <$  tok "*") ]
  , [ InfixL (Add <$ tok "+")
    , InfixL (Sub <$ tok "-") ]
               ]

aTerm = parens aexp
     <|> N <$> number <* whitespace
     <|> var

-- aexpressions :: Parser Aexp
-- aexpressions = try(Add <$> aterm <* char '+' <*> aexp)
--      <|> try(Mult <$> aterm <* char '*' <*> aexp)
--      <|> try(Sub <$> aterm <* char '-' <*> aexp)
--
-- aterm :: Parser Aexp
-- aterm = N <$> number <* whitespace
--      <|> var
--
-- aexp :: Parser Aexp
-- aexp = parens aexps
--      <|> try(aterm)

-- bexp :: Parser Bexp
-- bexp = try(And <$> bterm <* char '&' <* whitespace <*> bexp) --And
--     <|> try(Eq <$> aTerm <* char '=' <* whitespace <*> aexp)
--     <|> try(Le <$> aTerm <* tok "<=" <*> aexp)
--     <|> try(bterm)                                           --bterminals

bTerm :: Parser Bexp
bTerm = parens bexpr
     <|> TRUE <$ tok "true" <* whitespace    --TRUE
     <|> FALSE <$ tok "false" <* whitespace --FALSE
     <|> rExpr   --Negative


rExpr :: Parser Bexp
rExpr = do
 a1 <- aexp
 op <- relation
 a2 <- aexp
 return (op a1 a2)


relation = try(tok "=" *> pure Eq)
 <|> try(tok "<" *> pure Le)

bexpr :: Parser Bexp
bexpr = makeExprParser bTerm bOperators


bOperators :: [[Operator Parser Bexp]]
bOperators =
  [ [Prefix (Neg <$ tok "!") ]
  , [InfixL (And <$ tok "&")]
  ]

vars :: Parser Var
vars = (some (oneOf ['a' .. 'z'])) <* whitespace

var :: Parser Aexp
var = V <$> vars

decv :: Parser DecV
decv = many decvclause

decvclause :: Parser (Var,Aexp)
decvclause = try((tok "var") *> ((,) <$> vars) <* try(tok ":=") <*> aexp <* try(tok ";"))

decp :: Parser DecP
decp = many decpclause

decpclause :: Parser (Pname,Stm)
decpclause = try((tok "proc") *> ((,) <$> vars) <* (tok "is") <*> bigstm' <* try(tok ";"))

blockParse :: Parser Stm
blockParse = (tok "begin") *> (Block <$> decv) <*> decp <*> bigstm <* (tok "end")

ifParse :: Parser Stm
ifParse = tok "if" *> ((If <$> bexpr) <* (tok "then") <*> bigstm <* (tok "else") <*> bigstm) <* whitespace

whileParse :: Parser Stm
whileParse = tok "while" *>  ((While <$> bexpr ) <* (tok "do") <*> bigstm) <* whitespace

assParse :: Parser Stm
assParse = Ass <$> vars <* tok ":=" <*> aexp <* whitespace

callParse :: Parser Stm
callParse = (tok "call") *> (Call <$> vars) <* whitespace

compParse :: Parser Stm
compParse =  try((Comp  <$> stmParens <* tok ";") <* whitespace <*> bigstm)

compParse' :: Parser Stm
compParse' =  try((Comp  <$> stmParens <* tok ";") <* whitespace <*> bigstm')

skipParse :: Parser Stm
skipParse = Skip <$ tok "skip" <* whitespace

stmParens :: Parser Stm
stmParens = try(whitespace *> parens compParse)
         <|> try(whitespace *> stm)
         <|> try(whitespace *> parens stm)

bigstm' :: Parser Stm
bigstm' = try(whitespace *> parens bigstm')
      <|> (whitespace *> stmTerm')

stmTerm' :: Parser Stm
stmTerm' = try(whitespace *> stm) <|> compParse'

stmTerm :: Parser Stm
stmTerm = compParse <|> try(whitespace *> stm)

bigstm :: Parser Stm
bigstm = try(whitespace *> parens bigstm)
      <|> (whitespace *> stmTerm)

-- stm :: Parser Stm
-- stm = Skip <$ tok "skip" <* whitespace
--    <|> try(Ass <$> vars <* tok ":=" <*> aexp <* whitespace)
--    <|> try(tok "while" *>  ((While <$> bexpr ) <* (tok "do") <*> comp) <* whitespace)
--    <|> try(tok "if" *> ((If <$> bexpr) <* (tok "then") <*> stm <* (tok "else") <*> comp) <* whitespace)
--    <|> block <* whitespace
--    <|> try((tok "call") *> (Call <$> vars) <* whitespace)

stm :: Parser Stm
stm = skipParse
   <|> ifParse
   <|> whileParse
   <|> blockParse
   <|> callParse
   <|> assParse

parseProc :: Parser a -> String -> Maybe a
parseProc x = parseMaybe x
