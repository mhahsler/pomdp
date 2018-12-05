PKG_CPPFLAGS = -I../..
  
LIB = ./liblp-solve.a

OBJ = debug.o hash.o lex.o lp.o lpkit.o presolve.o read.o solve.o

lib: $(LIB)

%.o: %.c
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -c -o $@ $<

$(LIB): $(OBJ)
	$(AR) -rucs $(LIB) $(OBJ)

mostlyclean: clean

clean:
	@-rm -rf .libs _libs $(LIB)
	@-rm -f *.o

