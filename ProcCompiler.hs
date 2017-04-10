module ProcCompiler where
import ProcParser

type T = Bool
type Z = Integer
type State = Var -> Z
type EnvP = Pname -> Stm

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


update_state :: (Var, Aexp) -> State ->  State
update_state v s = (\l -> if l == (fst v)
                              then aexp_ns (snd v) s
                              else s l)


update_env ::  (Pname, Stm) -> EnvP -> EnvP
update_env v envp = (\l -> if l == fst v
                                then snd v
                                else envp (fst v))

if_ns :: Bexp -> Stm -> Stm -> EnvP -> State -> (State, EnvP)
if_ns bool stm1 stm2 envp s = if (bexp_ns bool s)
                          then stm_ns stm1 envp s
                          else stm_ns stm2 envp s


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
stm_ns (Call proc_name) envp s = stm_ns (envp proc_name) envp s
stm_ns y@(While bool stm) envp s  =
  let (s', envp') = stm_ns stm envp s
  in if (bexp_ns bool s)
      then stm_ns y envp' s'
      else (s, envp)
stm_ns (Comp stm1 stm2) envp s =
  stm_ns stm2 envp' s'
    where
      (s', envp') = stm_ns stm1 envp s

state_init :: State
state_init "x" = 5
state_init "y" = 0
state_init _   = -1

state_error :: State
state_error _ = -1

envp_error :: EnvP
envp_error _ = Skip

envp_init :: EnvP
envp_init _ = undefined

--GHCI DEBUGGING FUNCTIONS BELOW
compile_stm :: String -> State -> State
compile_stm string state = case parseProc bigstm string of
                  Just x  -> fst (stm_ns x envp_init state)
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
