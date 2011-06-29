GHC=ghc -fwarn-unused-imports -fwarn-incomplete-patterns --make

all: spc

spc: spc.hs Language/Pascal/*.hs
	$(GHC) $<

install:
	cabal install --global

clean:
	find . -name \*.hi -delete
	find . -name \*.o -delete
