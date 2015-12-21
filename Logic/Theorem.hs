module Logic.Theorem ((|>), (|>>), (><), (>?<), (>??<),
                match, replace, replaceAll, shortify,
                deduceLast, prove) where

import Logic.Base
import qualified Logic.Expression as Expression
import Logic.Parser(parse, parseHeader)
import Logic.Proofs

import Control.Applicative ((<|>))
import Control.Monad (liftM2)
import Data.Maybe (isJust, fromJust, listToMaybe)

import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Util.MultiMap as MMap

-- Operations for easy construction of theorems
infixl 4 |>
infixl 4 |>>
infixl 4 ><
infixl 4 >?<
infixl 4 >??<

(Theorem (Header assumptions _) proof) |>> expr = Theorem (Header assumptions expr) (proof Seq.|> expr)
(|>) th = (th |>>).parse 
(Theorem (Header assumptions conclusion) proof) >< (Theorem _ add) = let
        newConclusion = if (not.null$add) 
                        then add `Seq.index` ((Seq.length add) - 1)
                        else conclusion
        in Theorem (Header assumptions newConclusion) (proof Seq.>< add)

theorem >??< (need, th) =
        case match need th of
                Just dict -> theorem >< replace dict th
                Nothing -> theorem  
theorem >?< (need, th) = theorem >??< (parseHeader need, th)

-- Check that theorem matches header        
match :: Header -> Theorem -> Maybe Dictionary
t `match` (Theorem s _) = let
        initial = Expression.match (conclusion t) (conclusion s) []
        firstMatch expr dict a b = a <|> (Expression.match b expr dict)
        
        include list expr dict = foldl (firstMatch expr dict) Nothing list
        extendDictionary a b = a >>= include (assumptions t) b
                in foldl extendDictionary initial (assumptions s) 

-- Replace variables in proof according to the dictionary
replace :: Dictionary -> Theorem -> Theorem
replace dict (Theorem header proof) = 
        Theorem header (fmap (Expression.replace dict) proof)

-- Replace variables in the whole theorem according to the dictionary
replaceAll :: Dictionary -> Theorem -> Theorem
replaceAll dict (Theorem header proof) = let
        repl = Expression.replace dict
        newAssumptions = map repl $ assumptions header
        newConclusion = repl $ conclusion header
        newProof = fmap repl proof
                in Theorem (Header newAssumptions newConclusion) newProof

-- Replace proof according to the header
prepareProof :: Header -> Theorem -> Seq.Seq Expression
prepareProof header th = let
        dict = fromJust $ match header th
        new = replace dict th
        in proof new

-- Remove duplicate statements from theorem
removeCopy :: Seq.Seq Expression -> Seq.Seq Expression
removeCopy list = let
        filtrate (res, pr) expr
                | expr `Set.member` pr = (res, pr)
                | otherwise = (res Seq.|> expr, Set.insert expr pr)
        in fst $ foldl filtrate (Seq.empty, Set.empty) list

-- Shortify proof
shortify :: Theorem -> Theorem
shortify (Theorem header proof) = let
        unique = removeCopy proof
        short = Seq.takeWhileL (/= conclusion header) unique
        in Theorem header (short Seq.|> conclusion header)

-- Deduce last statement in theorem's assumption
deduceLast :: Theorem -> Theorem
deduceLast (Theorem (Header assumptions conclusion) proof) = let
        alpha = last assumptions
        newAssumptions = init assumptions
        newHeader = Header newAssumptions (alpha :->: conclusion)
        assumptions' = Set.fromList newAssumptions
                
        transform (theorem, exprs, impls) expr 
                | (Just ind) <- Expression.isAxiom expr = res axiomCase [expr]
                | expr `Set.member` assumptions' = res axiomCase [expr]
                | expr == alpha = res reflexity []
                | j:rest <- MMap.lookup expr impls >>= 
                        \e -> if e `Set.member` exprs then [e] else [] = 
                        res mpCase [alpha :->: j, alpha :->: j :->: expr] 
                | otherwise = error $ (show expr) ++ (show assumptions)                
                where   res th assumes = let
                                need = Header assumes (alpha :->: expr)
                                in (theorem >??< (need, th), nexprs, nimpls)
                        
                        nexprs = Set.insert expr exprs
                        nimpls = case expr of 
                                (ant :->: cons) -> MMap.insert cons ant impls
                                otherwise -> impls
                                
        fst (x,_,_) = x
        in fst $ foldl transform (Theorem newHeader Seq.empty, Set.empty, MMap.empty) proof   

type Values = [(String, Expression)]

-- Prove the theorem
prove :: Expression -> Either Values Theorem
prove expr = let
        header = Header [] expr
        variables = Set.toList $ Expression.getVariables expr
        
        -- Find proof for operation in Proofs.hs
        findConnection :: Expression -> (Expression -> Header) -> (Theorem, Expression)
        findConnection expr header = case findProof expr of
                (x:xs) -> (x, expr)
                [] -> (head $ findProof (Not expr), Not expr)
                where findProof e = filter (isJust.match (header e)) connections
        
        -- Prove one case 
        prover :: Values -> Expression -> 
                (Seq.Seq Expression, Expression)
        prover dict (Variable s) = (Seq.empty, fromJust $ lookup s dict)
        prover dict expr@(Not e) = let
                (proof', proved') = prover dict e
                header e = Header [proved'] e
                (found, proved) = findConnection expr header
                in      (proof' Seq.>< 
                        prepareProof (header proved) found, proved)
                                                
        prover dict expr = let
                [a, b] = Expression.deconstruct expr
                (proof1, proved1) = prover dict a
                (proof2, proved2) = prover dict b
                header e = Header [proved1, proved2] e
                (found, proved) = findConnection expr header
                in      (proof1 Seq.>< 
                        proof2 Seq.>< 
                        prepareProof (header proved) found, proved)

        -- Merge cases into the final proof
        merger :: [String] -> Values -> 
                Either Values Theorem
        merger [] vals = let
                assumptions = snd $ unzip vals
                (proof, proved) = prover vals expr
                in if proved == expr 
                        then Right $ Theorem (Header assumptions expr) proof
                        else Left vals
                        
        merger (x:rest) pairs = let
                var = Variable x
                true = merger rest (pairs ++ [(x, var)])
                false = merger rest (pairs ++ [(x, Not var)])
                connect th1 th2 = 
                            (deduceLast th1)
                        ><  (deduceLast th2)
                        >??< (Header [var :->: expr, (Not var) :->:expr] expr, glue)
                in liftM2 connect true false        
        in merger variables [] >>= Right . shortify 
