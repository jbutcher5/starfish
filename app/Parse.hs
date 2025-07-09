module Parse where

import Text.Parsec
import Text.Parsec.Char

data Token = Ident String | Expr [Token] |
             Num Float | Nil |
             Str String | Pair Token Token |
             Lambda [String] [Token] | Boolean Bool |
             Macro [String] [Token] deriving Eq

showBody :: [Token] -> String
showBody (x:xs) = foldl (\acc a -> acc ++ " " ++ show a) (show x) xs
showBody [] = "nil"

instance Show Token where
  show (Num x) = show x
  show (Ident x) = x
  show (Expr (Ident "quote":xs)) = '\'' : showBody xs 
  show (Expr (x:xs)) = '(' : foldl (\acc a -> acc ++ " " ++ show a) (show x) xs ++ ")"
  show (Lambda formals body) = "(lambda " ++ (show . Expr $ map Ident formals) ++ " " ++ showBody body ++ ")"
  show (Expr []) = "nil"
  show (Pair x y) = '(':show x ++ " . " ++ show y ++ ")"
  show Nil = "nil"
  show (Str x) = '"':x ++ "\""
  show (Boolean True) = "#t"
  show (Boolean False) = "#f"
  show (Macro formals body) = "(defmacro " ++ (show . Expr $ map Ident formals) ++ " " ++ showBody body ++ ")"
    
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
