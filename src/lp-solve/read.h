#ifndef LP_SOLVE_READ
#define LP_SOLVE_READ 1

/* prototypes of functions used in the parser */

void init_read(void);
void add_constraint_name(char *name, int row);
void store_re_op(void);
void null_tmp_store(void);
void store_bounds(void);
void add_int_var(char *name);
void rhs_store(REAL value);
void var_store(char *var, int row, REAL value);

/* This one seems to have been forgotten in the original code.  Added
   4/15/2003 by arc */
void check_decl(int within_int_decl);

#endif
