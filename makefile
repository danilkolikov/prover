compilator = ghc -i. -O3
build: build_prover

build_prover:
	$(compilator) prover.hs
	
