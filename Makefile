brainless:
	ghc --make -Wall -Werror brainless.hs
clean:
	rm -f {Brainless,.}/*.{hi,o}
.PHONY: clean brainless
