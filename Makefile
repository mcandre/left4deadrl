BIN=bin/left4deadrl

FLAGS=-O2 -Wall -fwarn-tabs --make -fforce-recomp -o $(BIN) -main-is Left4DeadRL

all: $(BIN)

$(BIN): Left4DeadRL.hs hscharm/hscharm.hs hscharm/charm/charm.c hscharm/charm/charm.h
	mkdir -p bin/
	ghc $(FLAGS) Left4DeadRL.hs hscharm/hscharm.hs hscharm/charm/charm.c hscharm/charm/charm.h -package MissingH -package random-fu

hlint:
	hlint .

lint: hlint

churn:
	bundle exec churn

clean:
	-rm -rf bin/
	-rm *.o
	-rm *.hi
