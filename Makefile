GHC=ghc -fwarn-unused-imports --make

all: test

test: test.hs Language/Pascal/*.hs
	$(GHC) $<

clean:
	find . -name \*.hi -delete
	find . -name \*.o -delete
