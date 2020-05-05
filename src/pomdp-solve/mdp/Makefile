LIB = libmdp.a
OBJ = parser.o scanner.o imm-reward.o parse_err.o sparse-matrix.o mdp.o parse_hash.o

default: $(LIB)

# don't check sources so Windows does not try to rebuild
#scanner.c: scanner.l
scanner.c:
	lex -P yymdp scanner.l
	mv lex.yymdp.c scanner.c

#parser.c parser.h: parser.y
parser.c parser.h:
	yacc -p yymdp -d parser.y
	mv y.tab.c parser.c
	mv y.tab.h parser.h

$(LIB): $(OBJ)
	$(AR) -rcs $(LIB) $(OBJ)

.PHONY: clean deepclean

clean:
	@-rm -rf .libs _libs $(LIB)
	@-rm -f *.o

deepclean: clean
	@-rm -f scanner.c parser.c parser.h
