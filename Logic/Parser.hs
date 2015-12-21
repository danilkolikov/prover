module Logic.Parser (parse, parseHeader, parseTheorem) where


-- Parser for logic expresions

import Util.Parser
import Logic.Base (Expression(..), Header(Header), Theorem(Theorem))

import Data.Sequence(empty, (|>))
import Control.Applicative((<|>))
import Data.Char (isSpace, isLetter, isDigit)
import Data.Maybe (fromJust)

variable = do
        word <- takeMany1 (sat isLetter)
        number <- takeMany (sat isDigit)
        return (word ++ number)

neg = char '!'
lbr = char '('
rbr = char ')'
con = char '&'
dis = char '|'
impl = string "->"

expr = chainr disj impl (:->:)  
disj = chainl conj dis (:|:)
conj = chainl term con (:&:)
term =  (do { name <- variable; return (Variable name)})        <|>
        (do { neg; term <- term; return (Not term) })           <|>
        (do { lbr; expr <- expr; rbr; return expr })

header = do
        let comma = char ','
        assumptions <- expr `sepBy` comma
        string "|-"
        conclusion <- expr
        return $ Header assumptions conclusion

start parser = fst.fromJust.(run parser).(filter (not.isSpace))
        
parse = start expr
parseHeader = start header

parseTheorem s = let
        strings = filter (not.null) (lines s)
        header = parseHeader (head strings)
        proof = foldl (\a b -> a |> (parse b)) empty (tail strings)
                in Theorem header proof 
