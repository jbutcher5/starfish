module Main where

import Text.Parsec
import Text.Parsec.Char
import Data.Maybe (fromMaybe)
import Data.Char (digitToInt)
import System.IO

import qualified Data.HashMap.Strict as Map (HashMap, empty, insert, lookup)

type Environment = Map.HashMap String Token

data Token = Ident String | Expr [Token] | Num Float | Nil | Cons Token Token

instance Show Token where
  show (Num x) = show x
  show (Ident x) = x
  show (Expr (x:xs)) = '(' : foldr (\a acc -> acc ++ " " ++ show a) (show x) xs ++ ")"
  show (Expr []) = "nil"
  show Nil = "nil"
  show (Cons a b) = '(' : show a ++ " . " ++ show b ++ ")"

value :: Parsec String st Token 
value = skipMany space *> choice [number, boolean, expr, nil, ident] <* skipMany space

ident :: Parsec String st Token 
ident = Ident <$> many1 (noneOf "()[]{} ")

expr :: Parsec String st Token
expr = char '(' *> (try cons <|> (Expr <$> values)) <* char ')'

decimal :: Parsec String st String 
decimal = (:) <$> char '.' <*> many1 digit

integer :: Parsec String st String
integer = many1 digit

number :: Parsec String st Token 
number = do
  neg <- maybe id (const negate) <$> optionMaybe (char '-')
  intial <- digit
  i <- optionMaybe integer
  d <- optionMaybe decimal
  return . Num . neg . read $ maybe [intial] (intial:) (i <> d)  

cons :: Parsec String st Token
cons = string "cons" *> (Cons <$> value <*> value)

true :: Parsec String st Token
true = string "#t" >> pure (Num 1.0)

false :: Parsec String st Token
false = string "#t" >> pure (Num 0.0)

boolean :: Parsec String st Token
boolean = choice [true, false]

nil :: Parsec String st Token
nil = string "nil" >> pure Nil

values :: Parsec String st [Token]
values = many value 

evalArgs :: Environment -> [Token] -> Either String [Token]
evalArgs env = mapM (evalNoEnv env)

evalNoEnv :: Environment -> Token -> Either String Token
evalNoEnv env x = fst <$> eval env x

eval :: Environment -> Token -> Either String (Token, Environment)

eval env (Expr ((Ident "define"):xs)) =
  case xs of
    [Ident label, x] -> do
      sexp <- evalNoEnv env x
      Right (Nil, Map.insert label sexp env)
    _ -> Left "Expected (define ident _)"

eval env (Ident x) =
  case Map.lookup x env of
    Just x -> Right (x, env)
    _ -> Left "Unbound variable"

eval env (Expr ((Ident "car"):xs)) =
  do ys <- evalArgs env xs 
     case xs of
       [Cons x _] -> Right (x, env)
       _ -> Left "Expected (car list)"
    
eval env (Expr ((Ident "cdr"):xs)) =
  do ys <- evalArgs env xs
     case ys of
       [Cons _ x] -> Right (x, env)
       _ -> Left "Expected (cdr list)"

eval env (Expr ((Ident "list"):xs)) =
  do ys <- evalArgs env xs
     case reverse ys of
       [] -> Right (Expr [], env)
       (z:zs) -> Right (foldr Cons (Cons z Nil) zs, env)
  
eval env (Expr ((Ident "+"):xs)) =
  do ys <- evalArgs env xs
     case ys of
       [Num x, Num y] -> Right (Num $ x + y, env)
       _ -> Left "Expected (+ num num)"

eval env x = Right (x, env)

repl :: Environment -> IO ()
repl env = do
  putStr "> "
  hFlush stdout
  s <- getLine
  case parse value "" s of
    Left err -> print err >> repl env
    Right x -> case eval env x of
                 Right (result, new_env) -> print result >> repl new_env
                 Left err -> print err >> repl env

main :: IO ()
main = repl Map.empty
