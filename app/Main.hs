module Main where

import Text.Parsec
import Text.Parsec.Char
import Data.Maybe (fromMaybe)
import Data.Char (digitToInt)
import System.IO

data Token = Ident String | Expr [Token] | Num Float | Nil | Cons Token Token deriving Show

value :: Parsec String st Token 
value = choice [number, boolean, expr, nil, ident]

ident :: Parsec String st Token 
ident = do
  x <- many1 $ noneOf "()[]{} "
  return $ Ident x

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
cons = do
   string "cons"
   space
   x <- value
   space
   y <- value
   optional space
   return $ Cons x y 

true :: Parsec String st Token
true = string "#t" >> pure (Num 1.0)

false :: Parsec String st Token
false = string "#t" >> pure (Num 0.0)

boolean :: Parsec String st Token
boolean = choice [true, false]

nil :: Parsec String st Token
nil = string "nil" >> pure Nil

values :: Parsec String st [Token]
values = many (try value <|> skipMany space *> value) 

main :: IO ()
main = do
  putStr "> "
  hFlush stdout
  x <- getLine
  parseTest values x
  main
