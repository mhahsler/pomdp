LIB = ./liblp-solve.a
OBJ = debug.o hash.o lp.o lpkit.o presolve.o read.o solve.o

default: $(LIB)

$(LIB): $(OBJ)
	$(AR) -rcs $(LIB) $(OBJ)

clean:
	@-rm -rf .libs _libs $(LIB)
	@-rm -f *.o
