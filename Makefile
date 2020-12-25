.PHONY: format
format:
	git ls-files '*.hs' | xargs fourmolu -i

.PHONY: install-884
install-884:
	stack install --resolver lts-16.27

.PHONY: install-8102
install-8102:
	stack install

