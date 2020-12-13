.PHONY: format
format:
	git ls-files '*.hs' | xargs stylish-haskell -i
