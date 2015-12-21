module Logic.Expression (match, replace, isAxiom, getVariables, deconstruct) where

import Logic.Base
import Logic.Parser
import Logic.Proofs (axioms)

import Control.Monad((>>=))
import Control.Applicative((<|>))
import Data.Maybe (isJust, fromJust)
import Data.List (findIndex)
import qualified Data.Set as Set

-- Useful functions for expressions
 
-- Get operands of expression
deconstruct :: Expression -> [Expression]
deconstruct expr = case expr of
        (Variable s) -> []
        (Not e) -> [e]
        (a :->: b) -> [a, b]
        (a :|: b) -> [a, b]
        (a :&: b) -> [a, b]

-- Check that two expression has the same operation and get operands
deconstruct2 :: Expression -> Expression -> [(Expression, Expression)]
deconstruct2 (a :&: b) (c :&: d) = [(a, c), (b, d)]
deconstruct2 (a :|: b) (c :|: d) = [(a, c), (b, d)]
deconstruct2 (a :->: b) (c :->: d) = [(a, c), (b, d)]
deconstruct2 (Not a) (Not b) = [(a, b)]
deconstruct2 _ _ = []

-- Match two expresions
match :: Expression -> Expression -> Dictionary -> Maybe Dictionary
match x (Variable s) m
        | (Just v) <- found, v == x = Just m
        | Nothing <- found = Just $ ((s,x) : m)
        | otherwise = Nothing
        where found = lookup s m
match a b m
        | [(x, y)] <- dc = match x y m
        | [(x, y), (t, s)] <- dc = (match x y m) >>=  (match t s)
        | otherwise = Nothing
        where dc = deconstruct2 a b

match' a b = match a b []

-- Replace variables according to dictionary
replace :: Dictionary -> Expression -> Expression
replace d e = case e of 
        Variable s -> case lookup s d of
                Just c -> c
                Nothing -> Variable s
        Not e -> Not $ replace d e
        a :&: b -> (replace d a) :&: (replace d b)
        a :|: b -> (replace d a) :|: (replace d b)
        a :->: b -> (replace d a) :->: (replace d b)


getVariables :: Expression -> Set.Set String
getVariables (Variable s) = Set.singleton s
getVariables (Not e) = getVariables e
getVariables x = combine $ deconstruct x
        where   combine [a, b] = (getVariables a) `Set.union` (getVariables b)
        

isAxiom x = findIndex (isJust.(match' x).conclusion.header) axioms

