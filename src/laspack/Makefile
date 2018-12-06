INCLUDES = 
#FLAGS = -Wall -Wunused-variable
FLAGS =
  
LIB = ./liblaspack.a

OBJ = eigenval.o matrix.o qmatrix.o errhandl.o mlsolv.o rtc.o factor.o operats.o vector.o itersolv.o precond.o
#OBJ = liblaspack_a-eigenval.o  liblaspack_a-matrix.o   liblaspack_a-qmatrix.o liblaspack_a-errhandl.o  liblaspack_a-mlsolv.o   liblaspack_a-rtc.o liblaspack_a-factor.o    liblaspack_a-operats.o  liblaspack_a-vector.o liblaspack_a-itersolv.o  liblaspack_a-precond.o

lib: $(LIB)

%.o: %.c
	$(CC) $(INCLUDES) $(FLAGS) -c -o $@ $<

$(LIB): $(OBJ)
	$(AR) -rcs $(LIB) $(OBJ)

mostlyclean: clean

clean:
	@-rm -rf .libs _libs $(LIB)
	@-rm -f *.o

