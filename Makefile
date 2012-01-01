all: left4deadrl

left4deadrl: left4deadrl.hs hscharm.hs charm.c charm.h
	ghc --make -fforce-recomp -o left4deadrl left4deadrl.hs hscharm.hs charm.c charm.h -package containers -package random-fu

clean:
	-rm left4deadrl
	-rm *.hi
	-rm *.o