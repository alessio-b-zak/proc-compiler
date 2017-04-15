module ProcCompiler where

import Control.Applicative
import Text.Megaparsec hiding (State)
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


type T = Bool
type Z = Integer
type State = Var -> Z
type EnvP = Pname -> Stm
data EnvPExt = Inter(Pname -> (Stm, EnvPExt)) | Final EnvP
type Config = (State, EnvP)

aexp_ns :: Aexp -> State -> Z
aexp_ns (N number) s = number
aexp_ns (V x) s      = s x
aexp_ns (Add x y) s  = (aexp_ns x s) + (aexp_ns y s)
aexp_ns (Mult x y) s = (aexp_ns x s) * (aexp_ns y s)
aexp_ns (Sub x y) s  = (aexp_ns x s) - (aexp_ns y s)

bexp_ns :: Bexp -> State -> T
bexp_ns TRUE s      = True
bexp_ns FALSE s     = False
bexp_ns (Neg x) s   = not (bexp_ns x s)
bexp_ns (And x y) s = (bexp_ns x s) && (bexp_ns y s)
bexp_ns (Le x y) s  = (aexp_ns x s) <= (aexp_ns y s)
bexp_ns (Eq x y) s  = (aexp_ns x s) == (aexp_ns y s)


-- updateState :: State -> Var -> AssignmentType -> State
-- updateState s v x =
--   (\l -> case x of
--           VarAssign x -> if l == v
--                           then s x
--                           else s l
--           IntAssign x -> if l == v
--                           then x
--                           else s l)


update_env_mixed ::  EnvPExt -> (Pname, Stm) -> EnvPExt
update_env_mixed a@(Inter envp) v = Inter(\l -> if l == fst v
                                                then (snd v, a)
                                                else (envp l))
update_env_mixed a@(Final envp) v = Inter(\l -> if l == fst v
                                              then (snd v, a)
                                              else (envp (fst v), a))

fold_proc_mixed :: DecP -> EnvPExt -> EnvPExt
fold_proc_mixed xs envp = foldl update_env_mixed envp xs

block_ns_mixed :: Stm -> EnvPExt -> State -> (State, EnvPExt)
block_ns_mixed (Block var_dec proc_dec stm) envp s  =
  stm_ns_mixed stm (fold_proc_mixed proc_dec envp) (fold_dec var_dec s)

update_state :: (Var, Aexp) -> State ->  State
update_state v s = (\l -> if l == (fst v)
                              then aexp_ns (snd v) s
                              else s l)

update_env ::  (Pname, Stm) -> EnvP -> EnvP
update_env v envp = (\l -> if l == fst v
                                then snd v
                                else envp l)

if_ns :: Bexp -> Stm -> Stm -> EnvP -> State -> (State, EnvP)
if_ns bool stm1 stm2 envp s = if (bexp_ns bool s)
                          then stm_ns stm1 envp s
                          else stm_ns stm2 envp s

if_ns_mixed :: Bexp -> Stm -> Stm -> EnvPExt -> State -> (State, EnvPExt)
if_ns_mixed bool stm1 stm2 envp s = if (bexp_ns bool s)
                          then stm_ns_mixed stm1 envp s
                          else stm_ns_mixed stm2 envp s

fold_dec :: DecV -> State -> State
fold_dec xs s = foldr update_state s xs

fold_proc :: DecP -> EnvP -> EnvP
fold_proc xs envp = foldr update_env envp xs


block_ns :: Stm -> EnvP -> State -> (State, EnvP)
block_ns (Block var_dec proc_dec stm) envp s  =
  stm_ns stm (fold_proc proc_dec envp) (fold_dec var_dec s)


stm_ns :: Stm -> EnvP -> State -> (State, EnvP)
stm_ns (Ass var val) envp s       = (update_state (var, val) s, envp)
stm_ns Skip envp s                = (s, envp)
stm_ns (If bool stm1 stm2) envp s = if_ns bool stm1 stm2 envp s
stm_ns a@(Block _ _ _) envp s     = block_ns a envp s
stm_ns (Call proc_name) envp s    = stm_ns (envp proc_name) envp s
stm_ns y@(While bool stm) envp s  =
  let (s', envp') = stm_ns stm envp s
  in if (bexp_ns bool s)
      then stm_ns y envp' s'
      else (s, envp)
stm_ns (Comp stm1 stm2) envp s    =
  stm_ns stm2 envp' s'
    where
      (s', envp') = stm_ns stm1 envp s


extract_state :: EnvPExt -> Pname -> (Stm, EnvPExt)
extract_state (Inter envp) name = envp name
extract_state a@(Final envp) name = (envp name, a)


call_ns_mixed :: EnvPExt -> Pname -> State -> (State, EnvPExt)
call_ns_mixed (Inter envp) name s = stm_ns_mixed stm' envp1 s
                              where
                                (stm', envp') = envp name
                                envp1 = update_env_mixed envp' (name, stm') 

stm_ns_mixed :: Stm -> EnvPExt -> State -> (State, EnvPExt)
stm_ns_mixed (Ass var val) envp s       = (update_state (var, val) s, envp)
stm_ns_mixed Skip envp s                = (s, envp)
stm_ns_mixed (If bool stm1 stm2) envp s = if_ns_mixed bool stm1 stm2 envp s
stm_ns_mixed a@(Block _ _ _) envp s     = block_ns_mixed a envp s
stm_ns_mixed (Call proc_name) envp s    = call_ns_mixed envp proc_name s
stm_ns_mixed y@(While bool stm) envp s  =
  let (s', envp') = stm_ns_mixed stm envp s
  in if (bexp_ns bool s)
      then stm_ns_mixed y envp' s'
      else (s, envp)
stm_ns_mixed (Comp stm1 stm2) envp s    =
  stm_ns_mixed stm2 envp' s'
    where
      (s', envp') = stm_ns_mixed stm1 envp s


state_init :: State
state_init "x" = 2
state_init _   = -1

state_error :: State
state_error _ = -1

envp_error :: EnvP
envp_error _ = Skip

envp_init :: EnvP
envp_init _ = Skip

--GHCI DEBUGGING FUNCTIONS BELOW
compile_stm :: State -> String -> State
compile_stm state string = case parseProc bigstm string of
                  Just x  -> fst (stm_ns x envp_init state)
                  Nothing -> state_error

compile_stm_mixed :: State -> String -> State
compile_stm_mixed state string = case parseProc bigstm string of
                  Just x  -> fst (stm_ns_mixed x (Final envp_init) state)
                  Nothing -> state_error


compile_bexp :: IO()
compile_bexp = do
  putStrLn "Input text to compile: "
  bexpToParse <- getLine
  case parseProc bexpr bexpToParse of
    Just x -> do
      evaluated_bexp <- return (bexp_ns x state_init)
      putStrLn (show evaluated_bexp)
    Nothing -> do
      putStrLn "Error Parsing"


compile_aexp :: IO()
compile_aexp = do
  putStrLn "Input text to compile: "
  aexpToParse <- getLine
  case parseProc aexp aexpToParse of
    Just x -> do
      evaluated_aexp <- return (aexp_ns x state_init)
      putStrLn (show evaluated_aexp)
    Nothing -> do
      putStrLn "Error Parsing"
