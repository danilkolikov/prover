module Main where

import Logic.Parser
import Logic.Base
import Logic.Theorem
import Logic.Proofs
import Logic.IO

import Data.List(sort)
import System.IO
import System.Environment

main = do
        args <- getArgs
        
        let     
                -- Prove theorem or say that it's a lie
                proveTheorem content output = let 
                        expression = parse content
                        theorem = prove expression
                        print = hPutStr stdout
                        in do
                                case theorem of
                                        Right proof -> do
                                                printTheorem output proof
                                        Left dict -> let
                                                decide (name, var) 
                                                        | Variable _ <- var = name ++ " = T"
                                                        | Not _ <- var = name ++ " = F"

                                                initial = "Statement is false when " ++
                                                        decide (head dict)                        
                                                in      foldl (\o e -> 
                                                                o >> print (", " ++ (decide e))) 
                                                        (print initial) (tail dict) 
        
                -- Annotate theorem from input file
                annotate content output = let
                        theorem = parseTheorem content
                        in printAnnotatedTheorem output theorem
                
                -- Read arguments from command line
                findMode [mode] = case mode of 
                        "-p" -> Just 0
                        "-a" -> Just 1
                        otherwise -> Nothing
                findMode _ = Nothing
                        
                run args = let 
                                mode = findMode args
                                perform = withFiles stdin stdout
                        in case mode of
                                Just 0 -> perform proveTheorem
                                Just 1 -> perform annotate
                                Nothing -> do
                                        print $ "Wrong arguments \n" ++ (show args) ++ " Usage: prover [MODE]"
                                     
                withFiles input output f = do                        
                        content <- hGetContents input
                        
                        f content output

        run args


