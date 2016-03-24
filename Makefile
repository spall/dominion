all: main.hs parse.hs state.hs player_v1.hs
	ghc main.hs parse.hs state.hs player_v1.hs test.hs

clean:
	rm *.o *.hi main
