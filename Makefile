.PHONY: format
format:
	git ls-files '*.hs' | xargs fourmolu -i

.PHONY: install884
install884:
	stack install --resolver lts-16.27

.PHONY: install8102
install8102:
	stack install

