GHC_OPTS := -Wall

all: build

run:
	cabal v2-run -O0 --ghc-options='$(GHC_OPTS) -fprint-potential-instances'

build:
	cabal v2-build -O0 --ghc-options='$(GHC_OPTS) -fprint-potential-instances'

dump-derived-generics:
	cabal v2-build -O0 --ghc-options='$(GHC_OPTS) -ddump-deriv'

clean:
	rm -rf dist dist-newstyle

repl:
	cabal v2-repl

.PHONY: all run build dump-derived-generics clean repl
