build:
	stack build

run: build
	stack exec -- break-unagi-chan-exe
