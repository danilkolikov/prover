compilator = ghc -iD:\Projects\prover -O3
build: build_prover

build_prover:
	$(compilator) prover.hs
	
