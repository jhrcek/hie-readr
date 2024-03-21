.PHONY: format
format:
	git ls-files '*.hs' | xargs fourmolu -i --unsafe
	cabal-fmt --inplace hie-readr.cabal


.PHONY: install
install:
	stack install

