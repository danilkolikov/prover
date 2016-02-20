compilator = ghc -i. -O3
build: build_prover

build_prover:
	$(compilator) prover.hs
	
clean:
	find . -name "*.o*" -type f -delete
	find . -name "*.hi*" -type f -delete