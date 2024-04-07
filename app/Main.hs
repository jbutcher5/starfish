module Main where

import Text.Parsec
import Text.Parsec.Char
import Data.Maybe (fromMaybe)
import Data.Char (digitToInt)
import Data.Bool
import System.IO

import qualified Data.HashMap.Strict as Map (HashMap, empty, insert, lookup)

type Environment = Map.HashMap String Token

data Token = Ident String | Str String | Expr [Token] | Num Float | Nil | Boolean Bool | Cons Token Token

instance Show Token where
  show (Num x) = show x
  show (Ident x) = x
  show (Expr (x:xs)) = '(' : foldr (\a acc -> acc ++ " " ++ show a) (show x) xs ++ ")"
  show (Expr []) = "nil"
  show Nil = "nil"
  show (Str x) = '"':x ++ "\""
  show (Boolean True) = "#t"
  show (Boolean False) = "#f"
  show (Cons a b) = '(' : show a ++ " . " ++ show b ++ ")"

value :: Parsec String st Token 
value = skipMany space *> choice [number, str, boolean, expr, nil, ident] <* skipMany space

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

str :: Parsec String st Token
str = char '"' *> (Str <$> many (noneOf "\"")) <* char '"'

cons :: Parsec String st Token
cons = string "cons" *> (Cons <$> value <*> value)

true :: Parsec String st Token
true = string "t" >> pure (Boolean True)

false :: Parsec String st Token
false = string "f" >> pure (Boolean False) 

boolean :: Parsec String st Token
boolean = char '#' >> choice [true, false]

nil :: Parsec String st Token
nil = string "nil" >> pure Nil

values :: Parsec String st [Token]
values = many value 

evalArgs :: Environment -> [Token] -> Either String ([Token], IO ())
evalArgs env xs = do
  result <- mapM (evalNoEnv env) xs
  return (fst <$> result, mapM_ snd result)

evalNoEnv :: Environment -> Token -> Either String (Token, IO ())
evalNoEnv env x = (\(token, io, _) -> (token, io)) <$> eval env x

eval :: Environment -> Token -> Either String (Token, IO (), Environment)

eval env (Expr ((Ident "define"):xs)) =
  case xs of
    [Ident label, x] -> do
      (sexp, io) <- evalNoEnv env x
      Right (Nil, io, Map.insert label sexp env)
    _ -> Left "Expected (define ident _)"

eval env (Ident x) =
  case Map.lookup x env of
    Just x -> Right (x, return (), env)
    _ -> Left "Unbound variable"

eval env (Expr ((Ident "car"):xs)) =
  do (tokens, io) <- evalArgs env xs 
     case tokens of
       [Cons x _] -> Right (x, io, env)
       _ -> Left "Expected (car list)"
    
eval env (Expr ((Ident "cdr"):xs)) =
  do (tokens, io) <- evalArgs env xs
     case tokens of
       [Cons _ x] -> Right (x, io, env)
       _ -> Left "Expected (cdr list)"

eval env (Expr ((Ident "list"):xs)) =
  do (tokens, io) <- evalArgs env xs
     case reverse tokens of
       [] -> Right (Expr [], io, env)
       (z:zs) -> Right (foldr Cons (Cons z Nil) zs, io, env)

eval env (Expr ((Ident "if"):xs)) =
  case xs of
    [pred, x, y] -> case evalNoEnv env pred of
      Right (Boolean True, io) -> f x io
      Right (Boolean False, io) -> f y io
      Left err -> Left err
      _ -> Left "Expected (if bool _ _)"
    _ -> Left "Expected (if bool _ _)"
  where
    f x io = do
      (tokens, io2) <- evalNoEnv env x
      Right (tokens, io >> io2, env)

eval env (Expr ((Ident "+"):xs)) =
  do (tokens, io) <- evalArgs env xs
     case tokens of
       [Num x, Num y] -> Right (Num $ x + y, io, env)
       _ -> Left "Expected (+ num num)"

eval env (Expr ((Ident "print"):xs)) =
  case xs of
    [x] -> do
      (result, io) <- evalNoEnv env x
      Right (Nil, io >> print result, env)
    _ -> Left "Expected (print _)"

eval env (Expr ((Ident "puts"):xs)) = do
  case xs of
    [x] -> do
      (result, io) <- evalNoEnv env x
      case result of
        Str x -> Right (Nil, io >> putStr x, env)
        _ -> Left "Expected (puts string)"
    _ -> Left "Expected (puts string)"

eval env (Expr ((Ident "puts-ln"):xs)) = do
  case xs of
    [x] -> do
      (result, io) <- evalNoEnv env x
      case result of
        Str x -> Right (Nil, io >> putStrLn x, env)
        _ -> Left "Expected (puts-ln string)"
    _ -> Left "Expected (puts-ln string)"
       
eval env x = Right (x, return (), env)

repl :: Environment -> IO ()
repl env = do
  putStr "> "
  hFlush stdout
  s <- getLine
  case parse value "" s of
    Left err -> print err >> repl env
    Right x -> case eval env x of
                 Right (result, io, new_env) -> io >> print result >> repl new_env
                 Left err -> print err >> repl env

main :: IO ()
main = repl Map.empty
