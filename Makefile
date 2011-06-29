GHC=ghc -fwarn-unused-imports --make

all: spc

spc: spc.hs Language/Pascal/*.hs
	$(GHC) $<

install:
	cabal install --global

clean:
	find . -name \*.hi -delete
	find . -name \*.o -delete
