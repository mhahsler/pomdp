PKG_CPPFLAGS = -I../..
  
LIB = ./libmdp.a

OBJ = imm-reward.o parse_err.o parser.o sparse-matrix.o mdp.o parse_hash.o  scanner.o 

lib: $(LIB)

%.o: %.c
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -c -o $@ $<

$(LIB): $(OBJ)
	$(AR) -rucs $(LIB) $(OBJ)

mostlyclean: clean

clean:
	@-rm -rf .libs _libs $(LIB)
	@-rm -f *.o

