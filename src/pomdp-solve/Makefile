INCLUDE = -L./mdp -L./lp-solve -L./laspack
MY_LIBS = -lm -lmdp -llp-solve -llaspack

OBJ = alpha.o belief.o cmd-line.o common.o cross-sum.o enumeration.o \
      finite-grid.o global.o inc-prune.o linear-support.o lp-interface.o \
      neighbor.o pomdp-solve-options.o params.o parsimonious.o pg.o pomdp.o \
      pomdp-solve.o projection.o random.o region.o signal-handler.o stats.o timing.o \
      two-pass.o vertex-enum.o witness.o xalloc.o zlz_speedup.o sim.o mcgs.o \
      associative-array.o command-line.o config-file.o program-options.o main.o

SUBDIRS = mdp lp-solve laspack
SUBLIBS = $(SUBDIRS:=.a)

all: pomdp-solve

pomdp-solve: sublibs $(OBJ)
	$(CC) $(CFLAGS) $(INCLUDE) $(OBJ) -o pomdp-solve $(MY_LIBS)

sublibs:
	@for d in $(SUBDIRS); do \
		(cd $${d} && CXX="$(CXX)" CC="$(CC)" CFLAGS="$(CFLAGS) $(CPICFLAGS)" AR="$(AR)" RANLIB="$(RANLIB)" $(MAKE)) || exit 1; \
	done

clean: subclean
	@rm -f *.o
	@rm -f pomdp-solve
	@rm -f pomdp-solve.exe

subclean:
	@for d in $(SUBDIRS); do \
	  (cd $${d} && $(MAKE) clean) || exit 1; \
	done