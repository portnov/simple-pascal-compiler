GHC=ghc -fwarn-unused-imports --make

all: spc

spc: spc.hs Language/Pascal/*.hs
	$(GHC) $<

clean:
	find . -name \*.hi -delete
	find . -name \*.o -delete
