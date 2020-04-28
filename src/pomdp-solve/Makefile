INCLUDE = -L./mdp -L./lp-solve -L./laspack
MY_LIBS = -lm -lmdp -llp-solve -llaspack

OBJ = alpha.o belief.o cmd-line.o common.o cross-sum.o enumeration.o \
      finite-grid.o global.o inc-prune.o linear-support.o lp-interface.o \
      neighbor.o pomdp-solve-options.o params.o parsimonious.o pg.o pomdp.o \
      pomdp-solve.o projection.o random.o region.o signal-handler.o stats.o timing.o \
      two-pass.o vertex-enum.o witness.o xalloc.o zlz_speedup.o sim.o mcgs.o \
      associative-array.o command-line.o config-file.o program-options.o main.o

SUBDIRS = mdp lp-solve laspack

all: pomdp-solve

mdp/libmdp.a:
	(cd mdp && CXX="$(CXX)" CC="$(CC)" CFLAGS="$(CFLAGS) $(CPICFLAGS)" AR="$(AR)" RANLIB="$(RANLIB)" $(MAKE))

lp-solve/liblp-solve.a:
	(cd lp-solve && CXX="$(CXX)" CC="$(CC)" CFLAGS="$(CFLAGS) $(CPICFLAGS)" AR="$(AR)" RANLIB="$(RANLIB)" $(MAKE))
  
laspack/liblaspack.a:
	(cd laspack && CXX="$(CXX)" CC="$(CC)" CFLAGS="$(CFLAGS) $(CPICFLAGS)" AR="$(AR)" RANLIB="$(RANLIB)" $(MAKE))

pomdp-solve: $(OBJ) mdp/libmdp.a lp-solve/liblp-solve.a laspack/liblaspack.a 
	$(CC) $(CFLAGS) $(INCLUDE) $(OBJ) -o pomdp-solve $(MY_LIBS)

.PHONY: clean

clean:
	rm -f *.o
	rm -f pomdp-solve
	rm -f pomdp-solve.exe
	for dir in $(SUBDIRS); do \
	  $(MAKE) -C $${dir} clean; \
	done

