.PHONY: format
format:
	git ls-files '*.hs' | xargs fourmolu -i
	cabal-fmt --inplace hie-readr.cabal

.PHONY: install884
install884:
	stack install --resolver lts-16.31

.PHONY: install8107
install8107:
	stack install

