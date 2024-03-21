.PHONY: format
format:
	git ls-files '*.hs' | xargs fourmolu --mode inplace --cabal-default-extensions
	cabal-fmt --inplace hie-readr.cabal


.PHONY: install
install:
	stack install

