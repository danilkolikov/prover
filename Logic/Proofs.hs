module Logic.Proofs where

import Logic.Parser
import {-# SOURCE #-} Logic.Theorem(deduceLast, (|>), (>?<), shortify)

-- Axioms and proofs for theorems

axioms = map (\s -> (parseTheorem ("|-" ++ s)) |> s) [
        "a->b->a",
        "(a->b)->(a->b->c)->(a->c)",
        "a->b->a&b",
        "a&b->a",
        "a&b->b",
        "a->a|b",
        "b->a|b",
        "(a->c)->(b->c)->(a|b->c)",
        "(a->b)->(a->!b)->!a",
        "!!a->a"]

-- |- a -> a
reflexity = 
        (parseTheorem "|- a-> a")           
        |> "a->(a->a)->a"                         
        |> "a->(a->a)"                            
        |> "(a->(a->a))->(a->(a->a)->a)->(a->a)"  
        |> "(a->(a->a)->a)->(a->a)"               
        |> "a->a"

-- Proofs for deductions        
axiomCase = 
        (parseTheorem "b |- a -> b") 
        |> "b->a->b" 
        |> "b" 
        |> "a -> b"

mpCase = 
        (parseTheorem "a->j, a->j->i |- a->i") 
        |> "(a->j)->(a->j->i)->(a->i)" 
        |> "a->j"
        |> "(a->j->i)->(a->i)" 
        |> "a->j->i"
        |> "a->i"

-- Proof for connection operations in prove algorithm
glue =
        (parseTheorem "a->c, !a->c |- c")
        |> "(a->c)->(!a->c)->(a|!a->c)"
        |> "a->c"
        |> "(!a->c)->(a|!a->c)"
        |> "!a->c"
        |> "a|!a->c"
        >?< ("|-a|!a", excludeMiddle)
        |> "c"

-- a -> b |- !b -> !a
contraposition = deduceLast $
        (parseTheorem "a->b, !b |- !a")
        |> "!b->a->!b"
        |> "!b"
        |> "a->!b"
        |> "a->b"
        |> "(a->b)->(a->!b)->!a"
        |> "(a->!b)->!a"
        |> "!a"

-- |- a | !a
excludeMiddle =  
        (parseTheorem "|- a | !a")
        |> "a->a|!a"
        >?< ("a->a|!a|-!(a|!a)->!a", contraposition)
        |> "!a->a|!a"
        >?< ("!a->a|!a|-!(a|!a)->!!a", contraposition)
        |> "(!(a|!a)->!a)->(!(a|!a)->!!a)->!!(a|!a)"
        |> "(!(a|!a)->!!a)->!!(a|!a)"
        |> "!!(a|!a)"
        |> "!!(a|!a)->(a|!a)"
        |> "a|!a"

-- Proofs for operations
connections = map shortify [
        (parseTheorem "!a|-!a")
        |> "!a",
        (parseTheorem "a|-!!a")
        |> "a->!a->a"
        |> "a"
        |> "!a->a"
        >?< ("|-!a->!a", reflexity)
        |> "(!a->a)->(!a->!a)->!!a"
        |> "(!a->!a)->!!a"
        |> "!!a",
        
        (parseTheorem "!a,!b|-!(a&b)") 
        |> "a&b->a"
        |> "!a->a&b->!a"
        |> "!a"
        |> "a&b->!a"
        |> "(a&b->a)->(a&b->!a)->!(a&b)"
        |> "(a&b->!a)->!(a&b)"
        |>"!(a&b)",
        (parseTheorem "!a,!b|-!(a|b)")
        |> "a&b->a"
        |> "!a->a&b->!a"
        |> "!a"
        |> "a&b->!a"
        |> "(a&b->a)->(a&b->!a)->!(a&b)"
        |> "(a&b->!a)->!(a&b)"
        |> "!(a&b)"
        >?< ("!a,!(a&b)|-a->a&b", connections !! 4)
        >?< ("!b,!(a&b)|-b->a&b", connections !! 4)
        |> "(a->a&b)->(b->a&b)->(a|b->a&b)"
        |> "(b->a&b)->(a|b->a&b)"
        |> "a|b->a&b"
        |> "!(a&b)->a|b->!(a&b)"
        |> "a|b->!(a&b)"
        |> "(a|b->a&b)->(a|b->!(a&b))->!(a|b)"
        |> "(a|b->!(a&b))->!(a|b)"
        |> "!(a|b)",
        deduceLast $ (parseTheorem "!a,!b,a|-b")
        |> "!a->!b->!a"
        |> "!a"
        |> "!b->!a"
        |> "a->!b->a"
        |> "a"
        |> "!b->a"
        |> "(!b->a)->(!b->!a)->!!b"
        |> "(!b->!a)->!!b"
        |> "!!b"
        |> "!!b->b"
        |> "b",
        (parseTheorem "!a,b|-!(a&b)")
        |> "a&b->a"
        |> "!a->a&b->!a"
        |> "!a"
        |> "a&b->!a"
        |> "(a&b->a)->(a&b->!a)->!(a&b)"
        |> "(a&b->!a)->!(a&b)"
        |> "!(a&b)",
        (parseTheorem "!a,b|-a|b")
        |> "b->a|b"
        |> "b"
        |> "a|b",
        (parseTheorem "!a,b|-a->b")
        |> "b->a->b"
        |> "b"
        |> "a->b",
        
        (parseTheorem "a,!b|-!(a&b)") 
        |> "a&b->b"
        |> "!b->a&b->!b"
        |> "!b"
        |> "a&b->!b"
        |> "(a&b->b)->(a&b->!b)->!(a&b)"
        |> "(a&b->!b)->!(a&b)"
        |> "!(a&b)",
        (parseTheorem "a,!b|-a|b")
        |> "a->a|b"
        |> "a"
        |> "a|b",
        (parseTheorem "a,!b|-!(a->b)")
        |> "!b->(a->b)->!b"
        |> "!b"
        |> "(a->b)->!b"
        |> "a->(a->b)->a"
        |> "a"
        |> "(a->b)->a"
        |> "((a->b)->a)->((a->b)->a->b)->((a->b)->b)"
        |> "((a->b)->a->b)->((a->b)->b)"
        >?< ("|-(a->b)->(a->b)", reflexity)
        |> "(a->b)->b"
        |> "((a->b)->b)->((a->b)->!b)->!(a->b)"
        |> "((a->b)->!b)->!(a->b)"
        |> "!(a->b)",
        
        (parseTheorem "a,b|-a&b")
        |> "a->b->a&b"
        |> "a"
        |> "b->a&b"
        |> "b"
        |> "a&b",
        (parseTheorem "a,b|-a|b")
        |> "a->a|b"
        |> "a"
        |> "a|b",
        (parseTheorem "a,b|-a->b")
        |> "b->a->b"
        |> "b"
        |> "a->b"]
