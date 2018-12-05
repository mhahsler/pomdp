# To be defined ...

# Override this since it seems to want to do -g -O2 all the time whch
# seems to be the default and which I do not yet know how to change.
#
CFLAGS = 

# Assumed location of dmalloc.h
#
DMALLOC_BASE = /usr/local

# Need this to have yacc generate a header file which I include in the
# scanner.
YFLAGS = -d

lib_LIBRARIES = liblpk.a

if DEBUG
  liblpk_a_CFLAGS = -g
else
if DMALLOC

  liblpk_a_CFLAGS = -g -DDMALLOC
  liblpk_a_INCLUDES = -I$(DMALLOC_BASE)/include

else
  liblpk_a_CFLAGS = -O3

endif
endif

LP_SOLVE_SRC = debug.c debug.h hash.c hash.h lp.y lex.l lpglob.h lpkit.c lpkit.h presolve.c read.c read.h solve.c

############################################################
#####
##### liblpk
#####

liblpk_a_SOURCES = $(LP_SOLVE_SRC)

