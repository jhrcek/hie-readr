.PHONY: format
format:
	git ls-files '*.hs' | xargs fourmolu -i
	cabal-fmt --inplace hie-readr.cabal

.PHONY: install884
install884:
	stack install --resolver lts-16.27

.PHONY: install8103
install8103:
	stack install

