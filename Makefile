build: $(wildcard src/*.hs) app/Main.hs aoc2022.cabal
	cabal build

run: build
	cabal exec aoc2022

test: $(wildcard test/**/*.hs)
	cabal test