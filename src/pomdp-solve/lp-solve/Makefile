LIB = ./liblp-solve.a
OBJ = y.tab.o lex.yy.o debug.o hash.o lpkit.o presolve.o read.o solve.o

default: $(LIB)

# don't check the files so Windows does not try to rebuild them
#lex.yy.c: lex.l y.tab.c
lex.yy.c:
	lex lex.l

#y.tab.c: lp.y
y.tab.c:
	yacc -d lp.y

$(LIB): $(OBJ)
	$(AR) -rcs $(LIB) $(OBJ)

clean:
	@-rm -rf .libs _libs $(LIB)
	@-rm -f *.o

deepclean: clean
	@-rm -f y.tab.c lex.yy.c

.PHONY: clean deepclean