#ifndef BISON_LP_H
# define BISON_LP_H

# ifndef YYSTYPE
#  define YYSTYPE int
#  define YYSTYPE_IS_TRIVIAL 1
# endif
# define	VAR	257
# define	CONS	258
# define	SIGN	259
# define	AR_M_OP	260
# define	RE_OP	261
# define	END_C	262
# define	COMMA	263
# define	COLON	264
# define	MINIMISE	265
# define	MAXIMISE	266


extern YYSTYPE yylval;

#endif /* not BISON_LP_H */
