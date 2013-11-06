all: left4deadrl

left4deadrl: left4deadrl.hs hscharm.hs charm.c charm.h
	ghc -O2 -Wall --make -fforce-recomp -o left4deadrl left4deadrl.hs hscharm.hs charm.c charm.h -package MissingH -package random-fu

lint:
	hlint .
	splint *.c *.h

clean:
	-rm *.exe
	-rm *.o
	-rm *.hi
	-rm left4deadrl
