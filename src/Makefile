INCLUDE = -L./mdp -L./lp-solve -L./laspack
LIBS = -lm -lmdp -llp-solve -llaspack


OBJ = alpha.o belief.o cmd-line.o common.o cross-sum.o enumeration.o finite-grid.o global.o inc-prune.o linear-support.o lp-interface.o neighbor.o pomdp-solve-options.o params.o parsimonious.o pg.o pomdp.o pomdp-solve.o projection.o random.o region.o signal-handler.o stats.o timing.o two-pass.o vertex-enum.o witness.o xalloc.o zlz_speedup.o sim.o mcgs.o associative-array.o command-line.o config-file.o program-options.o main.o

%.o: %.c
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -c -o $@ $<

../inst/pomdp-solve: $(OBJ)
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) $(INCLUDE) $(OBJ) -o pomdp-solve $(LIBS)
	cp pomdp-solve ../inst/
