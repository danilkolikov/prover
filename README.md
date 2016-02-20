Prover
------

Prover of logic theorems in propositional calculus (using Hilbert's system of axioms). 
Inspired by the course of mathematical logic.

Has two modes of work:

1. Prove statement: run with -p flag
2. Check and annotate proof: run with -a flag

By default Prover reads statements from standard input and writes to standard output. 
It's possible to specify custom io files with "<" and ">" operators


### Structure of statements:

* Propositional variables: (letters)(digits) : x123, y456
* Not: !x
* And: x & y
* Or: x | y
* Implies: x -> y
* Brackets: (expression)
* Header of a theorem: Assumption1, ..., AssumptionN |- Conclusion  

### List of axioms:

1. A -> B -> A 
2. (A -> B) -> (A -> B -> C) -> (A -> C)
3. A -> B -> A & b
4. A & B -> A
5. A & B -> B
6. A -> A | B
7. B -> A | B
8. (A -> C) -> (B -> C) -> (A | B -> C)
9. (A -> B) -> (A -> !B) -> !A
10. !!A -> A

**Modus Ponens**:

A -> B, A => B


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


### Example of annotation:

If we annotate this proof with -a flag:  

|-(A->A)

(1) ((A->(A->A))->(A->(A->(A->A)))) (Axiom 1)

(2) (A->(A->A)) (Axiom 1)

(3) (A->(A->(A->A))) (Modus Ponens 2 1)

(4) (A->((A->A)->A)) (Axiom 1)

(5) ((A->(A->A))->((A->((A->A)->A))->(A->A))) (Axiom 2)

(6) ((A->((A->A)->A))->(A->A)) (Modus Ponens 2 5)

(7) (A->A) (Modus Ponens 4 6)  



 
