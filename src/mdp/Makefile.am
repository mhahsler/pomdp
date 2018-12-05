# To be filled out ...

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

lib_LIBRARIES = libmdp.a

if DEBUG
  libmdp_a_CFLAGS = -g
else
if DMALLOC

  libmdp_a_CFLAGS = -g -DDMALLOC
  libmdp_a_INCLUDES = -I$(DMALLOC_BASE)/include

else
  libmdp_a_CFLAGS = -O3

endif
endif

COMMON_SRC = mdp-common.h imm-reward.c imm-reward.h mdp.c mdp.h parse_constant.h parse_err.c parse_err.h parse_hash.c parse_hash.h parser.y scanner.l sparse-matrix.c sparse-matrix.h

############################################################
#####
##### libmdp
#####

libmdp_a_SOURCES = $(COMMON_SRC)

