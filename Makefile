GHC=ghc -fwarn-unused-imports --make

clean:
	find . -name \*.hi -delete
	find . -name \*.o -delete
