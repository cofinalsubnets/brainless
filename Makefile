brainless:
	ghc --make brainless.hs
clean:
	rm -f {Brainless,.}/*.{hi,o}
.PHONY: clean brainless
