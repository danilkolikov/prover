module Util.Parser   (Parser(run), 
                sat, char, string, 
                takeMany, takeMany1, 
                sepBy, sepBy1, 
                chainl, chainr) where

-- Monadic parser
-- Inspired by "Functional pearls"  by Graham Hutton

import Control.Monad
import Control.Applicative

newtype Parser a = Parser {run :: String -> Maybe (a, String)}

instance Functor Parser where
        fmap = liftM

instance Applicative Parser where
        pure = return
        (<*>) = ap

instance Monad Parser where 
        return x = Parser $ \s -> Just (x, s)
        p >>= f = Parser $ \s -> run p s >>= \(e, r) -> run (f e)  r

instance Alternative Parser where
        empty = Parser $ \s -> Nothing
        p <|> l = Parser $ \s -> (run p s) <|> (run l s)
        
safe :: Parser [a] -> Parser [a]
safe p = p <|> return []

takeMany :: Parser a -> Parser [a]
takeMany = safe.takeMany1 

takeMany1 :: Parser a -> Parser [a]
takeMany1 p = do
        cur <- p
        rest <- takeMany p
        return (cur : rest)
           
sepBy :: Parser a -> Parser b -> Parser [a]        
p `sepBy` sep = safe (p `sepBy1` sep)

sepBy1 :: Parser a -> Parser b -> Parser [a]
p `sepBy1` sep = do     
        elt <- p
        rest <- takeMany (do {sep; p})
        return (elt : rest)

chainl p op operation = Parser $ \s -> 
                run (p `sepBy1` op) s >>= 
                \(tokens, rest) -> Just (foldl1 operation tokens, rest)
                
chainr p op operation = Parser $ \s -> 
                run (p `sepBy1` op) s >>= 
                \(tokens, rest) -> Just (foldr1 operation tokens, rest)

one :: Parser Char
one = Parser $ \s -> 
        case s of
                [] -> Nothing
                (c : rest) -> Just (c, rest)
 
sat :: (Char -> Bool) -> Parser Char
sat p = do
        c <- one
        if (p c) then return c else empty
                 
char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string "" = return ""
string (c : rest) = do
        c <- char c
        s <- string rest
        return (c : s)
