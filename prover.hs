module Main where

import Logic.Parser
import Logic.Base
import Logic.Theorem
import Logic.Proofs
import Logic.IO

import Data.List(sort)
import System.IO
import System.Environment

data Argument = Mode Int | Input String | Output String | Unexpected deriving (Eq, Ord)

main = do
        args <- getArgs
        print "Welcome to the Prover v. 0.1"
        
        let     
                -- Prove theorem or say that it's a lie
                proveTheorem content output = let 
                        expression = parse content
                        theorem = prove expression
                        printF = hPutStr output
                        in do
                                print $ "Mode: proving " ++ (show expression)
                                case theorem of
                                        Right proof -> do
                                                print "Theorem proved"
                                                printTheorem output proof
                                        Left dict -> let
                                                decide (name, var) 
                                                        | Variable _ <- var = name ++ " = T"
                                                        | Not _ <- var = name ++ " = F"

                                                initial = "Statement is false when " ++
                                                        decide (head dict)                        
                                                in (print "Statement is false") >> 
                                                        foldl (\o e -> 
                                                                o >> printF (", " ++ (decide e))) 
                                                        (printF initial) (tail dict) 
        
                -- Annotate theorem from input file
                annotate content output = let
                        theorem = parseTheorem content
                        in do
                                print "Mode: create annotation of proof"
                                printAnnotatedTheorem output theorem
                
                -- Read arguments from command line
                readArguments [] = []
                readArguments (flag : rest) 
                        | flag == "-p" = (Mode 0) : (readArguments rest)
                        | flag == "-a" = (Mode 1) : (readArguments rest)
                        | flag == "-i" = case rest of
                                [] -> [Unexpected]
                                (file : rest) -> (Input file) : (readArguments rest)
                        | flag == "-o" = case rest of
                                [] -> [Unexpected]
                                (file : rest) -> (Output file) : (readArguments rest)
                        | otherwise = Unexpected : (readArguments rest)
                
                findMode [] = Nothing
                findMode ((Mode m) : rest) = Just m
                findMode (_ : rest) = findMode rest
                
                findInput [] = "input.txt"
                findInput ((Input f) : rest) = f
                findInput (_ : rest) = findInput rest
                
                findOutput [] = "output.txt"
                findOutput ((Output f) : rest) = f
                findOutput (_ : rest) = findOutput rest
  
                run args = let 
                                arguments = sort $ readArguments args
                                mode = findMode arguments
                                input = findInput arguments
                                output = findOutput arguments
                                perform action = withFiles input output action
                        in case mode of
                                Just 0 -> perform proveTheorem
                                Just 1 -> perform annotate
                                Nothing -> do
                                        print "Mode isn't specified"
                                     
                withFiles inputF outputF f = do
                        print $ "Input: " ++ inputF
                        print $ "Output: " ++ outputF
                        
                        input <- openFile inputF ReadMode
                        output <- openFile outputF WriteMode
                        
                        content <- hGetContents input
                        
                        f content output
                        
                        hClose input
                        hClose output
                        
        
        run args
        
        print "Done"
                


