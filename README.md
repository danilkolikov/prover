Prover
------

Prover of logic theorems in propositional calculus (using Hilbert's axioms). 
Inspired by the course of mathematical logic.

Has two modes of work:

1. Prove statement: run with -p flag

2. Annotate proof: run with -a flag


By default reads statements from "input.txt" and writes output to "output.txt"
It's possible to specify custom io files with flags: 

-i "input file" -o "output file"


### Structure of statements:

* Propositional variables - (letters)(digits)
* And: &
* Or: |
* Implies: ->
* Brackets: ( )
* Header of a theorem: Assumption1, ..., AssumptionN |- Conclusion

### Example of proof:

**Input**:

A -> A


**Output** (-p flag):

|-(A->A)

((A->(A->A))->(A->(A->(A->A))))

(A->(A->A))

(A->(A->(A->A)))

(A->((A->A)->A))

((A->(A->A))->((A->((A->A)->A))->(A->A)))

((A->((A->A)->A))->(A->A))

(A->A)  



If we annotate this proof with -a flag:


|-(A->A)

(1) ((A->(A->A))->(A->(A->(A->A)))) (Axiom 1)

(2) (A->(A->A)) (Axiom 1)

(3) (A->(A->(A->A))) (Modus Ponens 2 1)

(4) (A->((A->A)->A)) (Axiom 1)

(5) ((A->(A->A))->((A->((A->A)->A))->(A->A))) (Axiom 2)

(6) ((A->((A->A)->A))->(A->A)) (Modus Ponens 2 5)

(7) (A->A) (Modus Ponens 4 6)
