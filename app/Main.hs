module Main where

import Text.Parsec
import Text.Parsec.Char
import Data.Maybe (fromMaybe)
import Data.Char (digitToInt)
import Data.Bool
import System.IO

import qualified Data.HashMap.Strict as Map (HashMap, empty, insert, lookup)

type Environment = Map.HashMap String Token

data Token = Ident String | Expr [Token] | Num Float | Nil | Str String | Boolean Bool | Cons Token Token

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

str :: Parsec String st Token
str = char '"' *> (Str <$> (many $ noneOf ['"'])) <* char '"'

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

evalArgs :: Environment -> [Token] -> IO (Either String [Token])
evalArgs env xs = sequence <$> mapM (evalNoEnv env) xs

evalNoEnv :: Environment -> Token -> IO (Either String Token)
evalNoEnv env x = (fmap . fmap) fst (eval env x)

eval :: Environment -> Token -> IO (Either String (Token, Environment))

eval env (Expr ((Ident "define"):xs)) =
  case xs of
    [Ident label, x] -> do
      res <- evalNoEnv env x
      return $ do
        sexp <- res
        Right (Nil, Map.insert label sexp env)
    _ -> return $ Left "Expected (define ident _)"

eval env (Ident x) =
  return $ case Map.lookup x env of
    Just x -> Right (x, env)
    _ -> Left "Unbound variable"

eval env (Expr ((Ident "car"):xs)) =
  case xs of
    [x] -> do res <- evalNoEnv env x
              return $ do token <- res
                          case token of
                            (Cons x _) -> Right (x, env)
                            _ -> Left "Expected (car list)"
    _ -> return $ Left "Expected (car list)"
    
eval env (Expr ((Ident "cdr"):xs)) =
  case xs of
    [x] -> do res <- evalNoEnv env x
              return $ do token <- res
                          case token of
                            (Cons _ x) -> Right (x, env)
                            _ -> Left "Expected (cdr list)"
    _ -> return $ Left "Expected (cdr list)"

eval env (Expr ((Ident "list"):xs)) =
  do res <- evalArgs env xs
     return $ do token <- res
                 case reverse token of
                   [] -> Right (Expr [], env)
                   (z:zs) -> Right (foldr Cons (Cons z Nil) (reverse zs), env)

eval env (Expr ((Ident "if"):xs)) =
  case xs of
    [pred, x, y] ->
      do res <- evalNoEnv env pred
         case res of
           Right (Boolean True) -> eval env x 
           Right (Boolean False) -> eval env y 
           Left err -> return $ Left err
           _ -> return $ Left "Expected (if bool _ _)"
    _ -> return $ Left "Expected (if bool _ _)"

eval env (Expr ((Ident "+"):xs)) =
  do res <- evalArgs env xs
     return $ do ys <- res
                 case ys of
                   [Num x, Num y] -> Right (Num $ x + y, env)
                   _ -> Left "Expected (+ num num)"

eval env (Expr ((Ident "print"):xs)) =
  case xs of
    [x] ->
      do res <- evalNoEnv env x
         case res of
           Right x -> print x >> return (Right (Nil, env))
           Left x -> return $ Left x
    _ -> return $ Left "Expected (print _)"

eval env (Expr ((Ident "input"):xs)) =
  case xs of
    [] -> (\x -> Right (Str x, env)) <$> getLine
    _ -> return $ Left "Expected (input)"

eval env x = return $ Right (x, env)

repl :: Environment -> IO ()
repl env = do
  putStr "> "
  hFlush stdout
  s <- getLine
  case parse value "" s of
    Left err -> print err >> repl env
    Right x -> do
      result <- eval env x
      case result of
        Right (x, new_env) -> print x >> repl new_env
        Left err -> print err >> repl env

main :: IO ()
main = repl Map.empty
