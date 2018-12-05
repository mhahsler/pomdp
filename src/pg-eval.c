/*
  pg-eval.c

  Routines to evaluate a policy graph.

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

  This module has routines to evaluate a POMDP policy graph by setting
  up a system of equations using the policy graph and the POMDP model
  and solving (using the laspack library). 

*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

/* My own libraries */
#include <mdp/mdp.h>
#include <mdp/sparse-matrix.h>

/* The LAPACK modules */
#include <laspack/errhandl.h>
#include <laspack/itersolv.h>
#include <laspack/lastypes.h>
#include <laspack/operats.h>
#include <laspack/precond.h>
#include <laspack/qmatrix.h>
#include <laspack/rtc.h>
#include <laspack/vector.h>

#include "global.h"
#include "laspack-util.h"
#include "pg.h"
#include "pg-eval.h"

#undef DEBUG
#undef VERBOSE


/**********************************************************************/
/*****   Routines for evaluation context structure                *****/
/**********************************************************************/

/**********************************************************************/
void 
PGEC_freeMemory( PgEvalContext pgec ) {
  int s;

  assert( pgec != NULL );

  if ( pgec->row_coef != NULL )
    XFREE( pgec->row_coef );

  if ( pgec->soln != NULL ) {

    for( s = 0; s < gNumStates; s++ )
      XFREE( pgec->soln[s] );
    XFREE( pgec->soln );

/*
    Q_Destr( &(pgec->A) );
    V_Destr( &(pgec->b) );
    V_Destr( &(pgec->x) );
*/

  } /* if soln not null */

  pgec->num_states = 0;
  pgec->num_nodes = 0;
  pgec->num_obs = 0;
  pgec->num_vars = 0;

  pgec->zero_epsilon = DEFAULT_PG_CONTEXT_EPSILON;

  pgec->row_coef = NULL;
  pgec->soln = NULL;

}  /* PGEC_freeMemory */
/**********************************************************************/
void 
PGEC_allocMemory( PgEvalContext pgec, PG pg ) {
  int s;

  pgec->num_states = gNumStates;
  pgec->num_nodes = pg->num_nodes;
  pgec->num_obs = pg->num_obs;
  pgec->num_vars = pgec->num_nodes * pgec->num_states;

  pgec->row_coef
    = (double *) XMALLOC( pgec->num_vars * sizeof ( double ));

/*
  Q_Constr( &(pgec->A), "A", pgec->num_vars, False, Rowws, Normal, True );
  V_Constr( &(pgec->b), "b", pgec->num_vars, Normal, True );
  V_Constr( &(pgec->x), "x", pgec->num_vars, Normal, True );
*/

    /* Allocate the memory */
  pgec->soln 
    = (double **) XMALLOC( pgec->num_states * sizeof( *(pgec->soln) ));
  for( s = 0; s < pgec->num_states; s++ )
    pgec->soln[s] = (double *) XMALLOC( pgec->num_nodes * sizeof( double ));

}  /* PGEC_allocMemory */
/**********************************************************************/
PgEvalContext 
PGEC_Constructor( ) {
/*
  To evaluate a policy graph you need one of these objects.  Once
  created it will automatically make sure it is in synch with the
  policy graph being evaluated.
*/
  PgEvalContext pgec;

  pgec = (PgEvalContext) XMALLOC( sizeof( *pgec ));
  
  pgec->num_states = 0;
  pgec->num_nodes = 0;
  pgec->num_obs = 0;
  pgec->num_vars = 0;

  pgec->row_coef = NULL;
  pgec->soln = NULL;

  return( pgec );

}  /* PGEC_Constructor */
/**********************************************************************/
void 
PGEC_Destructor( PgEvalContext pgec ) {

  if ( pgec == NULL )
    return;

  PGEC_freeMemory( pgec );
  XFREE( pgec );

} /* PGEC_Destructor */
/**********************************************************************/
void 
PGEC_update( PgEvalContext pgec, PG pg ) {
/*
  Checks the context sizes to the policy graph size and adjusts the
  context's arrays if needed.
*/
  
  if (( pg->num_states == pgec->num_states )
      && ( pg->num_nodes == pgec->num_nodes )
      && ( pg->num_obs == pgec->num_obs ))
    return;

  PGEC_freeMemory( pgec );
  PGEC_allocMemory( pgec, pg );
  
}  /* PGEC_update */
/**********************************************************************/
double 
PGEC_val( PgEvalContext pgec, double *b, int *best_node ) {
/*
  Evaluates the policy graph for the given belief state byt finding
  the node of the graph that yields the highers value
*/
  double max_sum = -1.0 * HUGE_VAL;
  double cur_sum;
  int cur_node, state;

  *best_node = -1;

  for ( cur_node = 0; cur_node < pgec->num_nodes; cur_node++ ) {

    cur_sum = 0.0;

    for ( state = 0; state < pgec->num_states; state++ )
      cur_sum += pgec->soln[state][cur_node] * b[state];

    if ( cur_sum > max_sum ) {
      max_sum = cur_sum;
      *best_node = cur_node;
    } /* if new best value */

  } /* for cur_node */

  return ( max_sum );

}  /* PGEC_val */
/**********************************************************************/

/**********************************************************************/
/*****   Routines for evaluating policy graphs                    *****/
/**********************************************************************/

/**********************************************************************/
int 
numNonZero( double *val, int length, double epsilon ) {
/*
  Returns the number of non-zero entries in the array.
*/
  int i;
  int count = 0;

  for ( i = 0; i < length; i++ ) {

    if ( Equal( val[i], 0.0, epsilon ))
      continue;

    count++;
  } /* for i */

  return ( count );
}  /* numNonZero */
/**********************************************************************/
void 
PGE_setRowCoefs( PG pg, int state, int node, double *coef ) {
/*
  Sets up the coefficients for a single row of the policy graph
  evaluation system of equations.  
*/
  int i, j, next_state, a, z, col;

  /* Zero out all the values first since we may need to accumlate
     values; e.g., when the next node specified in the policy graph is
     the same for more than one observation. */
  for ( i = 0; i < (pg->num_nodes * gNumStates); i++ )
    coef[i] = 0.0;

  /* This is the main variable of the row. */
  coef[node + state * pg->num_nodes] = 1.0;

  a = pg->action[node];
      
  /* Loop over all next states with non-zero transition probability */
  for( j = P[a]->row_start[state]; 
	  j < P[a]->row_start[state] +  P[a]->row_length[state];
	  j++ ) {
    
    next_state = P[a]->col[j];

    /* Loop over all the possible next policy graph nodes.  Note 
       that they might not all be unique, which is why we must be
       careful to accumulate the coefficients, instead of
       overwriting them. */
    for( z = 0; z < gNumObservations; z++ ) {

      /* This converts from the 2D naming to the 1D naming
         for the current variable under consideration in this
         row. */
      col = pg->next[node][z] + next_state * pg->num_nodes;
      
      /* Accumulate, not just asign value! */
      coef[col] -= gDiscount * P[a]->mat_val[j] 
        * getEntryMatrix( R[a], next_state, z );
      
    } /* for z */
  }  /* for j */
  
}  /* PGE_setRowCoefs */
/**********************************************************************/
void 
PGE_setPGCoefs( PG pg, PgEvalContext pgec ) {
/*
   Sets up the coefficients for the system of equations which
   evaluates the value of following a particular policy graph.

   There is a variable for each MDP state and Policy graph node pair.

   The tricky part here is to map the 2D indexed variables into the 1D
   variables for the equation solving routines.  
*/
  int  n, cur_state, non_zero_element;
  int row, col;

  for( cur_state = 0; cur_state < gNumStates; cur_state++ ) {
    
    /* 'n' will be the node_number of the policy graph, assuming we 
       number the nodes starting with '0' at the first element of the
       list. */
    for ( n = 0; n < pg->num_nodes; n++ ) {

      row = n + cur_state * pg->num_nodes;

      /* Set the RHS of this row. */
      V_SetCmp( &(pgec->b), row+1, 
                getEntryMatrix( Q, pg->action[n], cur_state ) );
      
      PGE_setRowCoefs( pg, cur_state, n, pgec->row_coef );
      
      /* Only store non-zeroes and set the row length before adding
         anything to it. */
      Q_SetLen( &(pgec->A), row+1, numNonZero( pgec->row_coef, 
                                               pgec->num_vars,
                                               pgec->zero_epsilon ) );

      non_zero_element = 0;
      for ( col = 0; col < pgec->num_vars; col++ ) {
        
        if ( Equal( pgec->row_coef[col], 0.0, pgec->zero_epsilon ))
          continue;
        
        /* LASPACK provide two sets of accessor functions to its data
           structures.  One is a functional interface and the other is
           a macro expansion

           Q_SetEntry() - function
           Q__SetEntry() - macro

        */
        Q_SetEntry( &(pgec->A), row+1, non_zero_element, col+1, 
                     pgec->row_coef[col] );
        
        non_zero_element++;

      } /* for i */

    }  /* for n */
  }  /* for cur_state */

}  /* PGE_setPGCoefs */
/**********************************************************************/
int 
PGE_evaluate( PG pg, PgEvalContext pgec ) {
/*
   This routine will return a 2D array of doubles that represents the
   value of executing the policy graph.  The value function is on
   pairs of the form: soln[MDP state][Policy Graph Node] 
*/
  int s, n;

  assert( pg != NULL && pgec != NULL );

  /* This just makes sure the evaluation context is in synch with the
     size of the policy graph and if not it adjusts the evaluation
     context arrays. */
  PGEC_update( pgec, pg );

  Q_Constr( &(pgec->A), "A", pgec->num_vars, False, Rowws, Normal, True );
  V_Constr( &(pgec->b), "b", pgec->num_vars, Normal, True );
  V_Constr( &(pgec->x), "x", pgec->num_vars, Normal, True );

  /* Set the coefficients in equation-solve in preparation to solving
     the system of equations. */
  PGE_setPGCoefs( pg, pgec );

#ifdef DEBUG
  showQMatrix( &(pgec->A) );
  showVector( &(pgec->b) );
#endif

#ifdef VERBOSE
  printf( "[Evaluating policy graph]\n" );
#endif

  SetRTCAccuracy( ITER_SOLVE_ACCURACY ); 
  V_SetAllCmp( &(pgec->x), 0.0 );
  JacobiIter( &(pgec->A), &(pgec->x), &(pgec->b), 
              MAX_JACOBI_ITERATIONS, JacobiPrecond, JACOBI_PRECON_OMEGA );
  if ( LASResult() != LASOK)  {
    Warning( "evaluatePG() Trouble trying to solve the system of equations." );
    WriteLASErrDescr(stdout);
    Q_Destr( &(pgec->A) );
    V_Destr( &(pgec->b) );
    V_Destr( &(pgec->x) );
    return( FALSE );
  } /* if trouble solving system of equations */
#ifdef DEBUG
  showVector( &(pgec->x) );
#endif
  
  /* Set the variables in the 2D represntation. */
  for( s = 0; s < pgec->num_states; s++ )
    for( n = 0; n < pgec->num_nodes; n++ )      
      pgec->soln[s][n] = V_GetCmp( &(pgec->x),
                                   n + s * pgec->num_nodes + 1 );
  
  Q_Destr( &(pgec->A) );
  V_Destr( &(pgec->b) );
  V_Destr( &(pgec->x) );
  return ( TRUE );
}  /* PGE_evaluate */
/**********************************************************************/
void 
PGE_solutionPrint( PgEvalContext pgec, FILE *file ) {
  int s, n;

  for ( n = 0; n < pgec->num_nodes; n++ ) {

    for ( s = 0; s < pgec->num_states; s++ )
      fprintf( file, "%6.2lf ", pgec->soln[s][n] );
    
    fprintf( file, "\n" );
    
  } /* for n */
  
}  /* PGE_solutionPrint */
/**********************************************************************/
void 
PGE_solutionDisplay( PgEvalContext pgec ) {
  PGE_solutionPrint( pgec, stdout );
}  /* PGE_solutionDisplay */

/**********************************************************************/
void 
PGE_main( char *pg_filename ) {
  LinkedPG lpg;
  PG pg;
  int best_node;

  PgEvalContext pgec;

  lpg = LPG_read( pg_filename, TRUE );

  if ( lpg == NULL )
    Abort( "Problem reading the policy graph file.");

  pg = PG_convertLPGToPG( lpg );

  pgec = PGEC_Constructor();
  
  if ( PGE_evaluate( pg, pgec )) {
    
    printf( "Solution found is:\n" );
    PGE_solutionDisplay( pgec );
    
    if ( gInitialBelief == NULL ) {
	 printf( "No initial belief state. Cannot give a single value.\n" );
    }
    else {
	 printf( "Initial belief state value: %.2lf at node %d\n",
		    PGEC_val( pgec, gInitialBelief, &best_node ),
		    best_node );
	 
    } /* if solution found */
  }

  else {
    Abort( "No solution found." );
  } /* else no solution found */
  
  PGEC_Destructor( pgec );
  PG_Destructor( pg );
  LPG_destroy( lpg );
	 
}  /* PGE_main */


