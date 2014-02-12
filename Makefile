all: left4deadrl

left4deadrl: left4deadrl.hs hscharm/hscharm.hs hscharm/charm/charm.c hscharm/charm/charm.h
	ghc -O2 -Wall --make -fforce-recomp -o left4deadrl left4deadrl.hs hscharm/hscharm.hs hscharm/charm/charm.c hscharm/charm/charm.h -package MissingH -package random-fu

lint:
	hlint .

churn:
	bundle exec churn

clean:
	-rm *.exe
	-rm *.o
	-rm *.hi
	-rm left4deadrl
