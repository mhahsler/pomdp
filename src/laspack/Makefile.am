# Override this since it seems to want to do -g -O2 all the time whch
# seems to be the default and which I do not yet know how to change.
#
CFLAGS = 

INCLUDES = -I$(top_srcdir)/src

# Assumed location of dmalloc.h
#
DMALLOC_BASE = /usr/local

# Need this to have yacc generate a header file which I include in the
# scanner.
YFLAGS = -d

lib_LIBRARIES = liblaspack.a

if DEBUG
  liblaspack_a_CFLAGS = -g
else
if DMALLOC

  liblaspack_a_CFLAGS = -g -DDMALLOC
  liblaspack_a_INCLUDES = -I$(DMALLOC_BASE)/include

else
  liblaspack_a_CFLAGS = -O3

endif
endif

LASPACK_HDRS	= copyrght.h \
	eigenval.h \
	elcmp.h \
	errhandl.h \
	factor.h \
	itersolv.h \
	lastypes.h \
	matrix.h \
	mlsolv.h \
	operats.h \
	precond.h \
	qmatrix.h \
	rtc.h \
	vector.h \
	version.h

LASPACK_SRCS	= eigenval.c \
	errhandl.c \
	factor.c \
	itersolv.c \
	matrix.c \
	mlsolv.c \
	operats.c \
	precond.c \
	qmatrix.c \
	rtc.c \
	vector.c


############################################################
#####
##### liblaspack
#####

liblaspack_a_SOURCES = $(LASPACK_HDRS) $(LASPACK_SRCS)

