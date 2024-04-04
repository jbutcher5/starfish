module Main where

import Text.Parsec
import Text.Parsec.Char
import Data.Maybe (fromMaybe)
import Data.Char (digitToInt)
import System.IO

data Token = Ident String | Expr [Token] | Num Float | Nil | Cons Token Token

instance Show Token where
  show (Num x) = show x
  show (Ident x) = x
  show (Expr (x:xs)) = '(' : foldr (\a acc -> acc ++ " " ++ show a) (show x) xs ++ ")"
  show (Expr []) = "()"
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

main :: IO ()
main = do
  putStr "> "
  hFlush stdout
  s <- getLine
  case parse values "" s of
    Left err -> print err
    Right [] -> putStrLn "nil"
    Right (x:_) -> print x
  main
