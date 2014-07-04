compiler: Main.hs
	ghc -o compiler.out Main.hs
	rm -rf *.o *.hi
	rm -rf Parser/*.o Parser/*.hi
	rm -rf Syntax/*.o Syntax/*.hi
	rm -rf SemanticChecker/*.o SemanticChecker/*.hi
	rm -rf CodeGenerator/*.o CodeGenerator/*.hi
