all: tetris4d.cabal Tetris4D.hs
	cabal configure	
	cabal build
	cp dist/build/tetris4d/tetris4d .

clean: 
	rm tetris4d
	rm dist
