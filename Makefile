all: left4deadrl

left4deadrl: left4deadrl.hs hscharm.hs charm.c charm.h
	ghc --make -fforce-recomp -o left4deadrl left4deadrl.hs hscharm.hs charm.c charm.h -package MissingH -package random-fu

clean:
	-rm left4deadrl
	-rm left4deadrl.exe
	-rm *.hi
	-rm *.o