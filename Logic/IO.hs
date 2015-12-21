module Logic.IO (printHeader, printTheorem, printAnnotatedTheorem) where

import Logic.Base
import Logic.Expression
import Logic.Proofs

import System.IO (hPutStr, hPutStrLn, Handle)
import qualified Data.Map as Map
import qualified Util.MultiMap as MMap

-- Input and output of theorems

-- Print header
printHeader :: Handle -> Header -> IO ()
printHeader output header = do
        let     assumptions' = assumptions header
                print = hPutStr output
                println = (hPutStrLn output).show
        if (null assumptions') then return () else do
                print (show.head $ assumptions')
                let     printer a b = a >> print (","++(show b))
                        rest = foldl printer (return ()) (tail assumptions')
                rest
        print "|-"
        println (conclusion header)

-- Print theorem
printTheorem :: Handle -> Theorem -> IO ()
printTheorem output (Theorem header proof) = let
        println = (hPutStrLn output).show
        printer a b = a >> println b
        printProof = foldl printer (return ()) proof
        
        in (printHeader output header) >> printProof
        
-- Annotate theorem and print it 
printAnnotatedTheorem :: Handle -> Theorem -> IO ()
printAnnotatedTheorem output (Theorem header proof) = let
        assumptions' = Map.fromList (zip (assumptions header) [1..])

        annotate (out, n, exprs, impls) expr 
                | (Just ind) <- isAxiom expr = 
                        res $ "(Axiom " ++ (show (ind + 1)) ++ ")"
                | (Just ind) <- Map.lookup expr assumptions' =
                        res $ "(Assumption " ++ (show ind) ++ ")"
                | (i,j):rest <- (MMap.lookup expr impls) >>= 
                        \(e, j) -> case Map.lookup e exprs of
                                Just i -> [(i, j)]
                                Nothing -> [] 
                        = res $ "(Modus Ponens " ++ (show i) ++ " " ++ (show j) ++ ")"
                | otherwise =
                        (out >> hPutStrLn output (beginning ++ "(Not proved)" ++ (show $ MMap.lookup expr impls)), 
                                n + 1, 
                                exprs, 
                                impls)
                        
                where   beginning = "(" ++ (show n) ++ ") " ++ (show expr) ++ " "
                        res annotation = 
                                (out >> hPutStrLn output (beginning ++ annotation), 
                                n + 1, 
                                nexprs, 
                                nimpls)
                        nexprs = Map.insert expr n exprs
                        nimpls = case expr of 
                                (ant :->: cons) -> MMap.insert cons (ant, n) impls
                                otherwise -> impls
        
        fst (x,_,_,_) = x                        
        printProof = fst $ 
                        foldl annotate (return (), 1, Map.empty, MMap.empty) proof
        in (printHeader output header) >> printProof
