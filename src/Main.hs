module Main where

import Data.Char
import Control.Applicative

clear = putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"

main :: IO ()
main = undefined

-- We define a map datatype to store a String and a JsonValue
type JsonMap = [(String, JsonValue)]

-- We define our language grammar
data JsonValue = JsonNull 
               | JsonBool Bool
               | JsonNumber Integer -- NOTE: No support for floats 
               | JsonString String
               | JsonArray [JsonValue]
               | JsonObject JsonMap
               deriving (Show, Eq)

-- NOTE: No proper error reporting (Could implement Either instead of Maybe)
newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

-- Parser :: (String -> Maybe (String, a)) -> Parser a
-- That is, a Parser is a function that takes in a String and maybe returns a tuple of a String and a parsed element
-- Example (A parser of characters (first element)):
-- parser 'n' :: String -> Maybe (String, a :: Char))
-- parser 'n' $ "nice" = ("ice",'n') -> since 

-- The following is such a function
-- It take a character and returns a parser that parses said character from a stream of characters
charP :: Char -> Parser Char
charP x = Parser f
  where  f (y:ys) = if y == x then Just (ys,x) else Nothing
         f [] = Nothing

-- If we want to access the value wrapped within the Parser datatype we need to implement functor
instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do -- f :: a -> b    e.g ord :: Char -> Int
    (input', x  ) <- p input -- Note we're not actually modifying the input, input' is just a promise that our parser will take in a string and return a Maybe (String, x),
    return $ (input', f x)   -- that is, p is already a parser that takes in a string and returns Maybe (String, x)
                             -- but instead of just returning x, we will return f(x)
{-
Example:

parser :: Parser Char   is a parser that parses characters
parser = charP 'n'

parser :: Parser Int    is now a parser that parses Ints
parser' = ord <$> parser 

<--<--<--<--<--<--<--<-->-->-->-->-->-->-->-->

charParse :: String -> Maybe (String, Char)
charParse = runParser parser

charParse "nice" = Just ("ice",'n')

<--<--<--<--<--<--<--<-->-->-->-->-->-->-->-->

intParse :: String -> Maybe (String, Int)
intParse = runParser parser'

charParse "nice" = Just ("ice",110)    ord 'n' == 110
-}

-- Now we want to chain parsers
-- i.e if we want to do the following: 
-- input -> parser1 -> parser2 -> ... -> parsedString
-- We con implement this functionality with the applicative instance

instance Applicative Parser where
  pure x = Parser $ \input -> return (input, x)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (input', f) <- p1 input
    (input'', a) <- p2 input'
    return $ (input'', f a)

{-
The first parser p1 returns a function f and the next string input -> input'

i.e (input', f) <- p1 input,

then we take input' and feed it into parser p2 which returns input'' and a
parsed item 'a'.

Finally we return input'' and f(a). i.e -> return $ (input'', f a)

This is precisely the chaining functionality we are looking for.

-}

-- We can now parse Strings sequencing char parsers
-- Example:
{-
Recall that "string" = ['s','t','r','i','n','g'],
therefore we can do the following: map charP "string" and this yields a new
list of char parsers:

parsers = map charP ['s','t','r','i','n','g'] = 
  [charP 's',charP 't',charP 'r',charP 'i',charP 'n',charP 'g'] =
:t = parsers :: [Parser Char]

However we want a parser of Strings, that is Parser [Char] = Parser String,
i.e we want to somehow invert our list of char parsers into a parser of a list of chars,
or a parser of strings.

This way we would have a parser that is capable of parsing an entire string and
not just a single character.


Note the type of sequenceA
:t sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)

lists are traversable and our parsers are applicatives! Thus, we are able to implement
this method and get out parser of strings.
-}

stringP :: String -> Parser String
stringP = sequenceA . map charP

-- YAY!!!

{-
we can now do the following:

> parser = stringP "hello"
> parse = runParser parser
> parse "hello" == Just ("","hello")

SUCCESS
-}

-- We are now ready to parse JsonValues

-- Ignore whetever input we receive and replace with JsonNull
-- (\_ -> JsonNull) <$> Parser $ \input -> (input', a)
-- ==  Parser $ \input -> (input', JsonNull)
jsonNull :: Parser JsonValue
jsonNull = (\_ -> JsonNull) <$> stringP "null"

-- Since we now need parse not only one, but two sequences of characters,
-- we must first try to parse true and if that fails try and parse false,
-- if that also fails, we return Nothing
-- i.e we want to combine two parsers into a single parser that tries
-- for "true" and "false" in sequence and picks the one that is
-- successful.

{-
We can use the Alternative (in Control.Applicative module) type class to acheive this:

class Applicative f => Alternative (f :: * -> *) where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]
  many :: f a -> f [a]
  {-# MINIMAL empty, (<|>) #-}
        -- Defined in `GHC.Base'

Our parser is already applicative, thus we need only implement the
alternative interface.
-}

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> 
    p1 input <|> p2 input -- We can take advantage of the fact that
                          -- Maybe is already an instance of Alternative
  some = undefined        
  many = undefined

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
  where f "true"  = JsonBool True
        f "false" = JsonBool False
        f _       = undefined

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
  let (token, rest) = span f input
    in return $ (rest, token) 

-- Parser combinator that parses a list of items and return the same parser
-- only if the initial parser is not null (not empty), otherwise it fails.
notNull :: Parser String -> Parser String
notNull (Parser p) = Parser $ \input -> do
  (input', xs) <- p input
  if null xs then Nothing
             else return (input', xs)

jsonNumber :: Parser JsonValue
jsonNumber = f <$> notNull (spanP isDigit)
  where f digits = JsonNumber $ read digits