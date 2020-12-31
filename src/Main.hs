module Main where

import Data.Char
import Control.Applicative

main :: IO ()
main = undefined

type JsonMap = [(String, JsonValue)]

data JsonValue = JsonNull 
               | JsonBool Bool
               | JsonNumber Integer 
               | JsonString String
               | JsonArray [JsonValue]
               | JsonObject JsonMap
               deriving (Show, Eq)

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

charP :: Char -> Parser Char
charP x = Parser f
  where  f (y:ys) = if y == x then Just (ys,x) else Nothing
         f [] = Nothing

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do 
    (input', x  ) <- p input 
    return $ (input', f x)   
                             
instance Applicative Parser where
  pure x = Parser $ \input -> return (input, x)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (input', f) <- p1 input
    (input'', a) <- p2 input'
    return $ (input'', f a)

stringP :: String -> Parser String
stringP = sequenceA . map charP

jsonNull :: Parser JsonValue
jsonNull = (\_ -> JsonNull) <$> stringP "null"

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> 
    p1 input <|> p2 input 

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
  where f "true"  = JsonBool True
        f "false" = JsonBool False
        f _       = undefined

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
  let (token, rest) = span f input
    in return $ (rest, token) 

notNull :: Parser String -> Parser String
notNull (Parser p) = Parser $ \input -> do
  (input', xs) <- p input
  if null xs then Nothing
             else return (input', xs)

jsonNumber :: Parser JsonValue
jsonNumber = f <$> notNull (spanP isDigit)
  where f digits = JsonNumber $ read digits

stringLiteral :: Parser String
stringLiteral = (charP '"' *> spanP (/= '"') <* charP '"')

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

whitespaces :: Parser String
whitespaces = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (fmap (:) element) <*> many (sep *> element) <|> pure []

jsonArray :: Parser JsonValue
jsonArray =  JsonArray <$> (charP '[' *> whitespaces *> elements <* whitespaces <* charP ']')
  where
    elements = sepBy (whitespaces *> charP ',' <* whitespaces) jsonValue 

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charP '{' *> whitespaces *> object <* whitespaces <* charP '}')
  where object = sepBy (whitespaces *> charP ',' <* whitespaces) pair
        pair = (\key _ value -> (key,value)) <$> stringLiteral 
                           <*> (whitespaces *> charP ':' *> whitespaces) 
                           <*> jsonValue

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject
