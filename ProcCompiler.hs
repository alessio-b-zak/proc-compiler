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
 <|> try(tok "<=" *> pure Le)

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
compParse =  try((Comp  <$> bigstm' <* tok ";") <* whitespace <*> bigstm)

skipParse :: Parser Stm
skipParse = Skip <$ tok "skip" <* whitespace

stmParens :: Parser Stm
stmParens = try(whitespace *> parens bigstm)
         <|> try(whitespace *> stm)
         <|> try(whitespace *> parens stm)

bigstm' :: Parser Stm
bigstm' = try(whitespace *> parens bigstm)
      <|> (whitespace *> stm)

stmTerm :: Parser Stm
stmTerm = try(whitespace *> compParse) <|> try(whitespace *> stm)

bigstm :: Parser Stm
bigstm = (whitespace *> stmTerm)
      <|> try(whitespace *> parens bigstm)


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

parse' :: String -> Stm
parse' string = case parseProc bigstm string of
                  Just x -> x
                  Nothing -> error "invalid parse"

type T = Bool
type Z = Integer
type State = Var -> Z
type EnvP = Pname -> Stm
-- data EnvPExt = Inter(Pname -> (Stm, EnvPExt)) | Final EnvP
data EnvPExt = Inter(Pname -> (Stm, EnvPExt, DecP)) | Final EnvP
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

update_envp_mixed :: DecP -> EnvPExt -> DecP -> EnvPExt
update_envp_mixed (x:xs) envp decp =
  update_envp_mixed xs envp' decp
    where
      envp' = Inter(\l -> if l == fst x
                              then (snd x, envp, decp)
                              else extract_envp' envp l)
update_envp_mixed [] envp decp = envp

update_prc_mixed :: DecP -> EnvPExt -> EnvPExt
update_prc_mixed decp envp = update_envp_mixed decp envp decp

-- update_env_mixed :: EnvPExt -> (Pname, Stm) -> EnvPExt
-- update_env_mixed a@(Inter envp) v = Inter(\l -> if l == fst v
--                                                 then (snd v, a)
--                                                 else (envp l))
-- update_env_mixed a@(Final envp) v = Inter(\l -> if l == fst v
--                                               then (snd v, a)
--                                               else (envp (fst v), a))

-- fold_proc_mixed :: DecP -> EnvPExt -> EnvPExt
-- fold_proc_mixed xs envp = foldl update_env_mixed envp xs

block_ns_mixed :: Stm -> EnvPExt -> State -> (State, EnvPExt)
block_ns_mixed (Block var_dec proc_dec stm) envp s  =
  (state''', envp)
    where
      envp' = (update_prc_mixed proc_dec envp)
      (state'',_) = stm_ns_mixed stm envp' state'
      state' = (fold_dec var_dec s)
      state''' = update_state_post_block var_dec s state''

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


update_state_post_block :: DecV -> State -> State -> State
update_state_post_block (x:xs) state_pre state_post =
  update_state_post_block xs state_pre state_post'
    where
      state_post' = update_state (fst x, (N (state_pre (fst x)))) state_post
update_state_post_block [] state_pre state_post = state_post

update_envp_post_block :: DecP -> EnvP -> EnvP -> EnvP
update_envp_post_block (x:xs) envp_pre envp_post =
  update_envp_post_block xs envp_pre envp_post'
    where
      envp_post' = update_env (fst x, envp_pre (fst x)) envp_post
update_envp_post_block [] envp_pre envp_post = envp_post

block_ns :: Stm -> EnvP -> State -> (State, EnvP)
block_ns (Block var_dec proc_dec stm) envp s  =
  (state''', envp)
    where
      envp' = (fold_proc proc_dec envp)
      state' = (fold_dec var_dec s)
      (state'', _) = stm_ns stm envp' state'
      state''' = update_state_post_block var_dec s state''


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

extract_envp' (Inter a) = a

call_ns_mixed' :: EnvPExt -> Pname -> State -> (State, EnvPExt)
call_ns_mixed' envp proc_name state = (state', envp)
  where
    (state', _) = stm_ns_mixed stm env' state
    (stm, envp, decp) = (extract_envp' (envp) proc_name)
    env' = update_prc_mixed decp envp


call_ns_mixed :: EnvPExt -> Pname -> State -> (State, EnvPExt)
call_ns_mixed env proc_name state = (state', env)
  where
    (state',_) = stm_ns_mixed stm1 env' state
    (stm1, envp, decp) = (extract_envp' env proc_name)
    env' = update_prc_mixed decp envp

-- call_ns_mixed :: EnvPExt -> Pname -> State -> (State, EnvPExt)
-- call_ns_mixed (Inter envp) name s = stm_ns_mixed stm' envp1 s
--                               where
--                                 (stm', envp') = envp name
--                                 envp1 = update_env_mixed envp' (name, stm')
-- call_ns_mixed a@(Final envp) name s = stm_ns_mixed (envp name) a s

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
state_init "x" = 5
state_init _   = 0

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


---Static Semantics Below

type Loc = Integer
type Next = Loc
type Store = (Loc -> Z, Next)
type EnvV = Var -> Loc
type State' = (EnvV, Store)
type Env = (EnvP_Static, EnvV)
data EnvP_Static = Inter'(Pname -> (Stm, EnvP_Static, EnvV, DecP)) | Final' EnvP

envp_init' :: EnvP_Static
envp_init' = Final' (\l -> Skip)

store_init :: Store
store_init = (store', 1)
  where
    store' _ = 0

envv_init :: EnvV
envv_init _ = -1

new :: Integer -> Integer
new = succ

update_envv :: Var -> EnvV -> Loc -> EnvV
update_envv var envv loc = (\l -> if l == var
                                    then loc
                                    else envv l)

update_store_var :: Z -> Loc -> Store -> Store
update_store_var val loc store  = (store', snd store)
  where store' = (\l -> if l == loc
                          then val
                          else (fst store) l)

update_store_new :: Store -> Store
update_store_new store = (fst store, new (snd store))

-- update_state_static :: State' -> (Var, Aexp) -> State'
-- update_state_static state decv =
--   (update_envv (fst decv) envv next, update_store val store next)
--     where next  = snd(snd state)
--           envv  = fst state
--           store = snd state
--           state' = ((fst store).envv) -- fst store :: Loc -> Z
--           val   = aexp_ns (snd decv) state' --

update_state_static :: State' -> (Var, Aexp) -> State'
update_state_static state decv =
  (update_envv (fst decv) envv next, update_store val next store)
    where
      envv  = fst state
      store = snd state
      next  = snd store
      val = aexp_ns (snd decv) ((fst store).envv)
      update_store x y z = update_store_new (update_store_var x y z)

fold_dec_static :: DecV -> State' -> State'
fold_dec_static decv state = foldl update_state_static state decv

-- update_envp_static :: EnvP_Static -> (Pname, Stm) -> EnvP_Static
-- update_envp_static b@(Inter' a) proc_name =
--   Inter'((\l -> if l == fst proc_name
--                   then (snd proc_name, b)
--                   else (fst a) l
--                   ), (snd a ))
-- update_envp_static b@(Final' a) proc_name =
--   Inter'((\l -> if l == fst proc_name
--                  then (snd proc_name, b)
--                  else ((fst a) l , b)), snd a)

extract_envp (Inter' envp) = envp


update_envp_static :: DecP -> EnvV -> EnvP_Static -> DecP -> EnvP_Static
update_envp_static (x:xs) envv envp decp =
  update_envp_static xs envv envp' decp
    where
      envp' = Inter'(\l -> if l == fst x
                      then (snd x, envp, envv, decp)
                      else (extract_envp envp) l)
update_envp_static [] envv envp decp = envp


update_prc_static :: DecP -> EnvV ->  EnvP_Static -> EnvP_Static
update_prc_static decp envv envp =
  update_envp_static decp envv envp decp


ass_ns_static :: Stm -> Env -> Store -> (Store, Env)
ass_ns_static (Ass var val) env sto = (update_store_var aval loc sto, env)
 where
   aval = aexp_ns val ((fst sto).(snd env))
   loc = (snd env) var

comp_ns_static :: Stm -> Env -> Store -> (Store, Env)
comp_ns_static (Comp stm1 stm2) env store =
  stm_ns_static stm2 env' store'
    where
      (store', env') = stm_ns_static stm1 env store

if_ns_static :: Stm -> Env -> Store -> (Store, Env)
if_ns_static (If b stm1 stm2) env sto =
  if (bexp_ns b state')
    then stm_ns_static stm1 env sto
    else stm_ns_static stm2 env sto
      where
        state' = (fst sto).(snd env)

while_ns_static :: Stm -> Env -> Store -> (Store, Env)
while_ns_static y@(While bool stm) env sto =
  let (sto', env') = stm_ns_static stm env sto
  in if(bexp_ns bool ((fst sto).(snd env)))
      then stm_ns_static y env' sto'
      else (sto, env)

block_ns_static :: Stm -> Env -> Store -> (Store, Env)
block_ns_static (Block decv decp stm) env store = (store'',  env)
    where
      (store'', env1) = stm_ns_static stm env' store'
      (envv', store') = fold_dec_static decv (snd env, store)
      env' = (update_prc_static decp envv' (fst env) , envv')

call_ns_static :: Pname -> Env -> Store -> (Store, Env)
call_ns_static proc_name env store = (store', env)
    where
      (store', _) = stm_ns_static stm1 env' store
      (stm1, envp, envv, decp) = (extract_envp (fst env) proc_name)
      envp' = update_prc_static decp envv envp
      env' = (envp', envv)

stm_ns_static :: Stm -> Env -> Store -> (Store, Env)
stm_ns_static a@(Ass _ _) env store = ass_ns_static a env store
stm_ns_static (Skip) env store = (store, env)
stm_ns_static a@(Comp _ _) env store = comp_ns_static a env store
stm_ns_static a@(If _ _ _) env store = if_ns_static a env store
stm_ns_static a@(While _ _) env store = while_ns_static a env store
stm_ns_static a@(Block _ _ _) env store = block_ns_static a env store
stm_ns_static (Call pname) env store = call_ns_static pname env store

extract_variables_decv ::  [String] -> (Var, Aexp) -> [String]
extract_variables_decv dec_vars var =
  if (elem (fst var) dec_vars)
    then dec_vars
    else (fst var):dec_vars


fold_variables_decv :: DecV -> [String] -> [String]
fold_variables_decv decv dec_vars =
  foldl extract_variables_decv dec_vars decv

extract_variables_decp :: [String] -> (Pname, Stm) -> [String]
extract_variables_decp dec_vars procs = extract_variables (snd procs) dec_vars

extract_arith :: Aexp -> [String] -> [String]
extract_arith (N _) string = string
extract_arith (V var) string = if elem var string
                                then string
                                else var:string
extract_arith (Mult a1 a2) string = extract_arith a2 (extract_arith a1 string)
extract_arith (Add a1 a2) string = extract_arith a2 (extract_arith a1 string)
extract_arith (Sub a1 a2) string = extract_arith a2 (extract_arith a1 string)


extract_bool :: Bexp -> [String] -> [String]
extract bool (TRUE) string = string
extract bool (FALSE) string = string
extract_bool (Neg b1) string = extract_bool b1 string
extract_bool (Le a1 a2) string = extract_arith a2 (extract_arith a1 string)
extract_bool (Eq a1 a2) string = extract_arith a2 (extract_arith a1 string)
extract_bool (And b1 b2) string = extract_bool b2 (extract_bool b1 string)


fold_variables_decp :: DecP -> [String] -> [String]
fold_variables_decp decp dec_vars = foldl extract_variables_decp dec_vars decp

extract_variables :: Stm -> [String] -> [String]
extract_variables (Ass var val) dec_vars =
  if (elem var dec_vars')
    then dec_vars'
    else var:dec_vars'
  where
    dec_vars' = extract_arith val dec_vars
extract_variables (Skip) dec_vars = dec_vars
extract_variables (Comp stm1 stm2) dec_vars =
   extract_variables stm2 (extract_variables stm1 dec_vars)
extract_variables (If bool stm1 stm2) dec_vars =
  extract_variables stm2 (extract_variables stm1 (extract_bool bool dec_vars))
extract_variables (While bool stm) dec_vars =
  extract_variables stm (extract_bool bool dec_vars)
extract_variables (Call _) dec_vars = dec_vars
extract_variables (Block decv decp stm) dec_vars =
  extract_variables stm dec_vars'
    where
      dec_vars' = fold_variables_decp decp (fold_variables_decv decv dec_vars)

extract_values_state :: [String] -> State -> (Store, EnvV)
extract_values_state dec_vars state =
  foldl update_sto_env (store_init, envv_init) dec_vars
    where
      update_sto_env sto_env dec_var = (store', envv')
        where
          (envv', store') = update_state_static ((snd sto_env), (fst sto_env)) (dec_var, N aval)
          aval = state dec_var

state_unwrapper :: State -> Stm -> (Store, Env)
state_unwrapper state stm = (store', env')
  where
    (store', envv') = extract_values_state (extract_variables stm []) state
    env' = (envp_init', envv')


state_rewrapper :: (Store, Env) -> State
state_rewrapper store_env = ((fst(fst store_env)).(snd(snd store_env)))

s_static :: State -> Stm -> State
s_static state stm = state_rewrapper result
  where
    (store, env) = state_unwrapper state stm
    result = stm_ns_static stm env store


help_parser_static :: State -> String -> State
help_parser_static state string = case parseProc bigstm string of
                        Just x -> s_static state x
                        Nothing -> state_error
