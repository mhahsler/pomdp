/*
  pg-eval.h

  *****
  Copyright 1998,2004 Anthony R. Cassandra

                           All Rights Reserved
                           
  Permission to use, copy, modify, and distribute this software and its
  documentation for any purpose other than its incorporation into a
  commercial product is hereby granted without fee, provided that the
  above copyright notice appear in all copies and that both that
  copyright notice and this permission notice appear in supporting
  documentation.
  
  ANTHONY CASSANDRA DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
  INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR ANY
  PARTICULAR PURPOSE.  IN NO EVENT SHALL ANTHONY CASSANDRA BE LIABLE FOR
  ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  *****

  This program searches in the space of deterministic finite state
  controllers 
*/
#ifndef PG_EVAL_H
#define PG_EVAL_H

#include <laspack/qmatrix.h>
#include <laspack/vector.h>
#include "pg.h"

/* When checking for zero entries in sparse equation solving of policy
   graph equations, use this value as the check against zero
   tolerance. */
#define DEFAULT_PG_CONTEXT_EPSILON    1e-8

typedef struct PgEvalContextStruct *PgEvalContext;
struct PgEvalContextStruct {

  /* It is more efficient to write out the row before entering it into
     the sparse array, since we need to know the number of non-zeroes.
     We keep a row of coeffients that is used over an over, so long as
     the policy graph size does not change.
  */
  int num_states;
  int num_nodes;
  int num_obs;
  int num_vars;
  double *row_coef;

  double zero_epsilon;

  /* The matrices used to solve are a function of the number of nodes
     and states. In  and can thus be allocated once. */ 
  QMatrix A;
  Vector x;
  Vector b;

  /* After a policy graph is evaluated, this will contain the solution
     as a 2-D array indexed by state-node pairs.
     */
  double **soln;

};


extern PgEvalContext PGEC_Constructor( );
extern void PGEC_Destructor( PgEvalContext pgec );
extern double PGEC_val( PgEvalContext pgec, 
                        double *b, 
                        int *best_node);
 
extern int PGE_evaluate( PG pg, PgEvalContext pgec );
extern void PGE_solutionPrint( PgEvalContext pgec, FILE *file );
extern void PGE_solutionDisplay( PgEvalContext pgec );
     
#endif

