module Logic.Base (   Expression(..), 
                Header(..), 
                Theorem(..),
                Dictionary) where
                
-- Base types for Expression and Theorem

import qualified Data.Sequence as Seq

infixr 3 :->:
infixl 4 :|: 
infixl 5 :&:

data Expression =       Expression :->: Expression |
                        Expression :|: Expression |
                        Expression :&: Expression | 
                        Not Expression |  
                        Variable String 
                        deriving (Eq, Ord)

instance Show Expression where
        show expr = case expr of 
                Variable c -> c
                Not c -> "!" ++ (show c)
                a :&: b -> print "&" a b
                a :|: b -> print "|" a b
                a :->: b -> print "->" a b
                where   print c a b = 
                                "(" ++ (show a) ++ c ++ (show b) ++ ")"
                                where 

data Header = Header {  assumptions :: [Expression],
                        conclusion :: Expression }
                        
data Theorem = Theorem { header :: Header,
                        proof :: Seq.Seq Expression }
                        
type Dictionary = [(String, Expression)]



