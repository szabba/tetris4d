all: tetris4d.cabal Main.hs Logic.hs Events.hs
	cabal configure	
	cabal build
	cp dist/build/tetris4d/tetris4d .

clean: 
	rm tetris4d
	rm -rf dist
