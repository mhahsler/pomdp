LIB = ./liblaspack.a
OBJ = eigenval.o matrix.o qmatrix.o errhandl.o mlsolv.o rtc.o factor.o operats.o vector.o itersolv.o precond.o

default: $(LIB)

$(LIB): $(OBJ)
	$(AR) -rcs $(LIB) $(OBJ)

clean:
	@-rm -rf .libs _libs $(LIB)
	@-rm -f *.o