module Main where

import Text.Parsec
import Text.Parsec.Char
import Data.Maybe (fromMaybe)
import Data.Char (digitToInt)
import Data.Bool
import System.IO

import qualified Data.HashMap.Strict as Map (HashMap, empty, insert, lookup)

type Environment = Map.HashMap String Token
type EnvStack = [Environment]

data Token = Ident String | Expr [Token] | Num Float | Nil | Str String | Pair Token Token | Lambda [String] [Token] | Boolean Bool

instance Show Token where
  show (Num x) = show x
  show (Ident x) = x
  show (Expr [Ident "quote", x]) = '\'' : show x
  show (Expr (x:xs)) = '(' : foldl (\acc a -> acc ++ " " ++ show a) (show x) xs ++ ")"
  show (Lambda _ _) = "#lambda"
  show (Expr []) = "nil"
  show (Pair x y) = '(':show x ++ " . " ++ show y ++ ")"
  show Nil = "nil"
  show (Str x) = '"':x ++ "\""
  show (Boolean True) = "#t"
  show (Boolean False) = "#f"

quote :: Parsec String st Token
quote = (*>) (char '\'') $ (\x -> Expr [Ident "quote", x]) <$> value

value :: Parsec String st Token 
value = skipMany space *> choice [quote, number, str, boolean, expr, nil, ident] <* skipMany space

ident :: Parsec String st Token 
ident = Ident <$> many1 (noneOf "()[]{} ")

expr :: Parsec String st Token
expr = char '(' *> (Expr <$> values) <* char ')'

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

searchEnv :: EnvStack -> String -> Maybe Token
searchEnv [] _ = Nothing
searchEnv (x:xs) ident =
  case Map.lookup ident x of
    Nothing -> searchEnv xs ident
    x -> x

evalArgs :: EnvStack -> [Token] -> IO (Either String [Token])
evalArgs env xs = sequence <$> mapM (evalNoEnv env) xs

evalNoEnv :: EnvStack -> Token -> IO (Either String Token)
evalNoEnv env x = (fmap . fmap) fst (eval env x)

evalLambda :: EnvStack -> [String] -> [Token] -> [Token] -> IO (Either String Token)
evalLambda env formals body args =
  if length formals == length args
     then evalBody new_env body (return $ Right Nil)
     else return $ Left $ "(not (eq? (length formals) (length args)))" ++ " " ++ show formals ++ " " ++ show args
  where
    new_env = bindFormals formals args Map.empty : env
    bindFormals :: [String] -> [Token] -> Environment -> Environment
    bindFormals [] [] acc = acc
    bindFormals (formal:xs) (arg:ys) acc = Map.insert formal arg (bindFormals xs ys acc)
    evalBody :: EnvStack -> [Token] -> IO (Either String Token) -> IO (Either String Token)
    evalBody envs [] acc = return $ Right Nil
    evalBody envs (x:xs) acc = do
      res <- eval envs x
      case res of
        Right (token, envs') -> evalBody envs' xs (pure $ Right token)
        Left err -> return $ Left err
    

eval :: EnvStack -> Token -> IO (Either String (Token, EnvStack))

eval env (Expr ((Ident "lambda"):xs)) =
  return $ case xs of
    (Expr formals:body) -> case reduceFormals formals [] of
      Just x -> Right (Lambda x body, env)
      _ -> Left "Expect (lambda (ident ...) _ ...)"
    _ -> Left "Expected (lambda sexp _ ...)"
    where
      reduceFormals :: [Token] -> [String] -> Maybe [String]
      reduceFormals [] acc = Just acc
      reduceFormals (Ident x : xs) acc = reduceFormals xs $ acc ++ [x]
      reduceFormals _ _ = Nothing

eval env (Expr ((Lambda formals args):xs)) =
  fmap (\x -> (x, env)) <$> evalLambda env formals args xs

eval env@(current:rest) (Expr ((Ident "define"):xs)) =
  case xs of
    [Ident label, x] -> do
      res <- evalNoEnv env x
      return $ do
        sexp <- res 
        Right (Nil, Map.insert label sexp current : rest)
    _ -> return $ Left "Expected (define ident _)"

eval env (Ident x) =
  return $ case searchEnv env x of
    Just x -> Right (x, env)
    _ -> Left "Unbound variable"

eval env (Expr ((Ident "car"):xs)) =
  case xs of
    [x] -> do res <- evalNoEnv env x
              return $ do token <- res
                          case token of
                            (Pair x _) -> Right (x, env)
                            (Expr (x:_)) -> Right (x, env)
                            _ -> Left "Expected (car list)"
    _ -> return $ Left "Expected (car list)"
    
eval env (Expr ((Ident "cdr"):xs)) =
  case xs of
    [x] -> do res <- evalNoEnv env x
              return $ do token <- res
                          case token of
                            (Pair _ x) -> Right (x, env)
                            (Expr (_:xs)) -> Right (Expr xs, env)
                            _ -> Left "Expected (cdr list)"
    _ -> return $ Left "Expected (cdr list)"

eval env (Expr ((Ident "quote"):xs)) =
  return $ case xs of
             [x] -> Right (x, env)
             _ -> Left "Expected (quote _)"

eval env (Expr ((Ident "cons"):xs)) =
  do maybe_args <- evalArgs env xs
     return $ case maybe_args of
       Right [x, Expr xs] -> Right (Expr (x:xs), env)
       Right [x, y] -> Right (Pair x y, env)
       Right _ -> Left "Expected (cons _ _)" 
       Left err -> Left err

eval env (Expr ((Ident "eval"):xs)) =
   case xs of
     [x] -> do y <- evalNoEnv env x
               case y of
                 Right z -> eval env z
                 Left z -> return $ Left z
     _ -> return $ Left "Expected (eval _)"
  
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

eval env (Expr ((Ident ident):xs)) =
  case searchEnv env ident of
    Just (Lambda formals args) -> fmap (\x -> (x, env)) <$> evalLambda env formals args xs
    _ -> return $ Left "Blahhhh"

eval env (Expr _) = return $ Left "Ill-formed expression"

eval env x = return $ Right (x, env)

repl :: EnvStack -> IO ()
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
main = repl [Map.empty]
