FLAGS  = 
#FLAGS  = -Wall -Wunused-variable
#FLAGS  = -Wall -Wpedantic -Wunused-variable

LIB = ./liblp-solve.a

OBJ = debug.o hash.o lex.o lp.o lpkit.o presolve.o read.o solve.o

lib: $(LIB)

%.o: %.c
	$(CC) $(FLAGS) -c -o $@ $<

$(LIB): $(OBJ)
	$(AR) -rcs $(LIB) $(OBJ)

clean:
	@-rm -rf .libs _libs $(LIB)
	@-rm -f *.o

