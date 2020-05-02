all:
	ghc Interpreter.hs
clean:
	rm -f Grammar/*.o Grammar/*.hi *.hi *.o Interpreter
