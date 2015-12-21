module Main where

import Logic.Parser
import Logic.Base
import Logic.Theorem
import Logic.Proofs
import Logic.IO

import System.IO
import System.Environment

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
                                                        | Variable _ <- var = name ++ "=И"
                                                        | Not _ <- var = name ++ "=Л"
                                                        
                                                initial = "Высказывание ложно при " ++
                                                        decide (head dict)                        
                                                in (print "Statement is not a truth") >> 
                                                        foldl (\o e -> 
                                                                o >> printF (", " ++ (decide e))) 
                                                        (printF initial) (tail dict) 
        
                -- Annotate theorem from input file
                annotate content output = let
                        theorem = parseTheorem content
                        in do
                                print "Mode: create annotation of proof"
                                printAnnotatedTheorem output theorem
                
                -- Read IO arguments
                readIO [] = ("input.txt", "output.txt")
                readIO [mode, file] 
                        | mode == "-i" = (file, "output.txt")
                        | mode == "-o" = ("input.txt", file)

                readIO [_, file1, _, file2] = (file1, file2) 
                
                withFiles :: (String, String) -> (String -> Handle -> IO ()) -> IO ()        
                withFiles (inputF, outputF) f = do
                        input <- openFile inputF ReadMode
                        output <- openFile outputF WriteMode
                        
                        print $ "Input: " ++ inputF
                        print $ "Output: " ++ outputF
                        
                        content <- hGetContents input
                        
                        f content output
                        
                        
                        hClose input
                        hClose output
                        
                readArgs :: [String] -> IO ()        
                
                readArgs [] = print "Mode isn't specified"
                readArgs (mode : rest) 
                        | mode == "-p" = withFiles (readIO rest) proveTheorem
                        | mode == "-a" = withFiles (readIO rest) annotate
        
        readArgs args
        
        print "Stopping..."
                


