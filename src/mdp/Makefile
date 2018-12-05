PKG_CPPFLAGS = -I../..
CPPFLAGS =

LIB = ./libmdp.a

OBJ = imm-reward.o parse_err.o parser.o sparse-matrix.o mdp.o parse_hash.o  scanner.o 

lib: $(LIB)

%.o: %.c
	$(CC) $(CPPFLAGS) -c -o $@ $<

$(LIB): $(OBJ)
	$(AR) -rcs $(LIB) $(OBJ)

clean:
	@-rm -rf .libs _libs $(LIB)
	@-rm -f *.o

