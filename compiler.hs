module ProcCompiler where
import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String
import Data.List (intercalate)

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

whitespace :: Parser ()
whitespace = many (oneOf " \n") *> pure ()

number :: Parser Numeric
number = (some (oneOf ['0' .. '9']) >>= return . read) <* whitespace

aterm :: Parser Aexp
aterm = N <$> number <* whitespace
     <|> var

bterm :: Parser Bexp
bterm = TRUE <$ tok "true" <* whitespace    --TRUE
     <|> FALSE <$ tok "false" <* whitespace --FALSE
     <|> tok "!" *> return Neg <*> bexp     --Negative

aexp :: Parser Aexp
aexp = try(Add <$> aterm <* char '+' <*> aexp)
     <|> try(Mult <$> aterm <* char '*' <*> aexp)
     <|> try(Sub <$> aterm <* char '-' <*> aexp)
     <|> try(aterm)


bexp :: Parser Bexp
bexp = try(And <$> bterm <* char '&' <* whitespace <*> bexp) --And
    <|> try(bterm)                                           --bterminals

vars :: Parser Var
vars = (some (oneOf ['a' .. 'z'])) <* whitespace

var :: Parser Aexp
var = V <$> vars

decv :: Parser DecV
decv = many decvclause

decvclause :: Parser (Var,Aexp)
decvclause = try((tok "var") *> ((,) <$> vars) <* (tok ":=") <*> aexp <* try(tok ";"))

decp :: Parser DecP
decp = many decpclause

decpclause :: Parser (Pname,Stm)
decpclause = try((tok "proc") *> ((,) <$> vars) <* (tok "is") <*> stm <* try(tok ";"))

block :: Parser Stm
block = try((tok "begin") *> try(Block <$> decv) <*> try(decp) <*> try(stm) <* (tok "end"))

comp :: Parser Stm
comp = try (Comp  <$> stm <* tok ";" <*> comp)
   <|> stm

stm :: Parser Stm
stm = Skip <$ tok "skip" <* whitespace
   <|> try(Ass <$> vars <* tok ":=" <*> aexp)
   <|> try(tok "while" *> ((While <$> bexp ) <* (tok "do") <*> comp))
   <|> try(tok "if" *> ((If <$> bexp) <* (tok "then") <*> stm <* (tok "else") <*> comp))
   <|> block
