all:
	ghc Interpreter.hs -o interpreter
clean:
	rm -f Grammar/*.o Grammar/*.hi *.hi *.o Interpreter
