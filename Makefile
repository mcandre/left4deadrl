BIN=left4deadrl

FLAGS=-O2 -Wall -fwarn-tabs --make -fforce-recomp -o $(BIN) -main-is Left4DeadRL

all: left4deadrl

left4deadrl: Left4DeadRL.hs hscharm/hscharm.hs hscharm/charm/charm.c hscharm/charm/charm.h
	ghc $(FLAGS) Left4DeadRL.hs hscharm/hscharm.hs hscharm/charm/charm.c hscharm/charm/charm.h -package MissingH -package random-fu

hlint:
	hlint .

lint: hlint

churn:
	bundle exec churn

clean:
	-rm left4deadrl
	-rm *.exe
	-rm *.o
	-rm *.hi
