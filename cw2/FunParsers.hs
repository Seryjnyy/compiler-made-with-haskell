module FunParsers(item, sat, char, string, int, nat, symbol, integer, natural, digit, space, token, identifierP, assignSymbolP) where

import Parsers(Parsers(..))
import Control.Applicative ( Alternative(..) )
import Data.Char ( isDigit, isSpace, isAlphaNum )

item :: Parsers Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

sat :: (Char -> Bool) -> Parsers Char
sat p = do x <- item
           if p x then return x else empty

char :: Char -> Parsers Char
char x = sat (== x)



string :: String -> Parsers String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

int :: Parsers Int
int = do 
         symbol "-"
         n <- nat
         return (-n)
       <|> nat

nat :: Parsers Int
nat = do xs <- some digit
         return (read xs)

symbol :: String -> Parsers String
symbol xs = token (string xs) 
integer = token nat 
natural = token nat

digit :: Parsers Char
digit = sat isDigit

space :: Parsers ()
space = do many (sat isSpace)
           return ()

token :: Parsers a -> Parsers a
token p = do space
             x <- p
             space
             return x 

alphaNumP = sat isAlphaNum

-- TODO - need to make sure first letter is capital
identifierP = do
   many alphaNumP

assignSymbolP = token $ string ":=" 