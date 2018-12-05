INCLUDES = -I$(top_srcdir)/src/mdp -I$(top_srcdir)/src/lp-solve  -I$(top_srcdir)/src/laspack

# Override this since it seems to want to do -g -O2 all the time whch
# seems to be the default and which I do not yet know how to change.
#
CFLAGS =

# Make sure to generate the header file, since the scanner will need it.
YFLAGS = -d

# Assumed location of dmalloc.h
#
DMALLOC_BASE = /usr/local

LDFLAGS = -L$(top_builddir)/src/mdp -L$(top_builddir)/src/lp-solve -L$(top_builddir)/src/laspack

COMMON_LIBS = $(top_builddir)/src/mdp/libmdp.a $(top_builddir)/src/lp-solve/liblpk.a $(top_builddir)/src/laspack/liblaspack.a @LEXLIB@

SUBDIRS = mdp lp-solve laspack

bin_PROGRAMS = pomdp-solve pomdp-tools pomdp-fg

if DEBUG

  ## DEBUG options
  ##
  pomdp_solve_CFLAGS = -g
  pomdp_solve_LDADD = $(COMMON_LIBS)

  pomdp_tools_CFLAGS = -g
  pomdp_tools_LDADD = $(COMMON_LIBS)

  pomdp_fg_CFLAGS = -g
  pomdp_fg_LDADD = $(COMMON_LIBS)

else

if PROFILE

  ## DEBUG options
  ##
  pomdp_solve_CFLAGS = -pg
  pomdp_solve_LDADD = $(COMMON_LIBS)
  pomdp_solve_LDFLAGS = -pg

  pomdp_tools_CFLAGS = -pg
  pomdp_tools_LDADD = $(COMMON_LIBS)
  pomdp_tools_LDFLAGS = -pg

  pomdp_fg_CFLAGS = -pg
  pomdp_fg_LDADD = $(COMMON_LIBS)
  pomdp_fg_LDFLAGS = -pg

else

if DMALLOC

  ## DMALLOC options
  ##
  pomdp_solve_CFLAGS = -g -DDMALLOC
  pomdp_solve_INCLUDES = -I$(DMALLOC_BASE)/include
  pomdp_solve_LDFLAGS = -L$(DMALLOC_BASE)/lib
  pomdp_solve_LDADD = $(COMMON_LIBS) -ldmalloc

  pomdp_tools_CFLAGS = -g -DDMALLOC
  pomdp_tools_INCLUDES = -I$(DMALLOC_BASE)/include
  pomdp_tools_LDFLAGS = -L$(DMALLOC_BASE)/lib
  pomdp_tools_LDADD = $(COMMON_LIBS) -ldmalloc

  pomdp_fg_CFLAGS = -g -DDMALLOC
  pomdp_fg_INCLUDES = -I$(DMALLOC_BASE)/include
  pomdp_fg_LDFLAGS = -L$(DMALLOC_BASE)/lib
  pomdp_fg_LDADD = $(COMMON_LIBS) -ldmalloc

else

  ## Default options
  ##
  pomdp_solve_CFLAGS = -O3
  pomdp_solve_LDADD = $(COMMON_LIBS)

  pomdp_tools_CFLAGS = -O3
  pomdp_tools_LDADD = $(COMMON_LIBS)

  pomdp_fg_CFLAGS = -O3
  pomdp_fg_LDADD = $(COMMON_LIBS)

endif
endif
endif

COMMON_SRC = alpha.h alpha.c belief.h belief.c cmd-line.h cmd-line.c common.h common.c cross-sum.h cross-sum.c enumeration.h enumeration.c finite-grid.h finite-grid.c global.h global.c inc-prune.h inc-prune.c linear-support.h linear-support.c lp-interface.h lp-interface.c neighbor.h neighbor.c pomdp-solve-options.h pomdp-solve-options.c params.h params.c parsimonious.h parsimonious.c pg.h pg.c pomdp.h pomdp.c pomdp-solve.h pomdp-solve.c projection.h projection.c random.h random.c region.h region.c signal-handler.h signal-handler.c stats.h stats.c timing.h timing.c two-pass.h two-pass.c vertex-enum.h vertex-enum.c witness.h witness.c xalloc.h xalloc.c zlz_speedup.h zlz_speedup.c sim.h sim.c mcgs.h mcgs.c associative-array.h associative-array.c command-line.h command-line.c config-file.h config-file.c program-options.h program-options.c 

POMDP_SOLVE_SRC = $(COMMON_SRC) main.c

POMDP_TOOLS_SRC = $(COMMON_SRC) utils.h utils.c pg-eval.h pg-eval.c laspack-util.h laspack-util.c pomdp-tools-options.h pomdp-tools-options.c pomdp-tools-main.c
POMDP_FG_SRC = $(COMMON_SRC) pomdp-fg-main.c pomdp-fg.h pomdp-fg.c index-list.h index-list.h pomdp-fg-options.h pomdp-fg-options.c fg-params.h fg-params.c value-function.h value-function.c belief-state.h belief-state.c double-vector.h double-vector.c 

############################################################
#####
##### pomdp-solve
#####

pomdp_solve_SOURCES =  $(POMDP_SOLVE_SRC)

############################################################
#####
##### pomdp-tools
#####

pomdp_tools_SOURCES = $(POMDP_TOOLS_SRC)


############################################################
#####
##### pomdp-fg
#####

pomdp_fg_SOURCES = $(POMDP_FG_SRC)


# For testing via 'make check'
##check_PROGRAMS = test-pomdp-solve
##test_pomdp_solve_SOURCES = ...
