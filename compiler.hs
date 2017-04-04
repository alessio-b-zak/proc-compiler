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
          | Le Aexp Aexp | Eq Aexp Aexp

data Stm = Skip | Ass Var Aexp | Comp Stm Stm
         | If Bexp Stm Stm | While Bexp Stm
         | Block DecV DecP Stm | Call Pname

tok :: String -> Parser String
tok t = try (string t <* whitespace)

newLine :: Parser ()
newLine = many (oneOf "\n") *> pure ()

whitespace :: Parser ()
whitespace = many (oneOf " ") *> pure ()

number :: Parser Numeric
number = (some (oneOf ['0' .. '9']) >>= return . read) <* whitespace

aexp :: Parser Aexp
aexp = Add <$> aexp <* tok "+" <*> aexp
     <|> N <$> number
     <|> V <$> var


var :: Parser Var
var = (some (oneOf ['a' .. 'z'])) <* whitespace
