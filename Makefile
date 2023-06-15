Compiler : Compiler.hs Syntax.hs Checker.hs Generator.hs
	ghc --make Compiler

clean :
	rm -f *.hi *.o Compiler

tests : FORCE
	mkdir -p localtests
	rm -rf ./localtests/*
	cp -r tests/*.fun localtests
	for i in 1 2 3 4 5 6 7 8 9; \
         do runhaskell Compiler.hs localtests/ejemplo$$i; \
            runhaskell Compiler.hs -o localtests/ejemplo$$i; done
	for i in 1 2 3 4; \
         do runhaskell Compiler.hs localtests/ejemplo$${i}err > \
            localtests/ejemplo$${i}err.err ; done

FORCE: ;
