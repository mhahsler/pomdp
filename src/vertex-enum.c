
/*
 *<SOURCE_HEADER>
 *
 *  <NAME>
 *    vertex-enum.c
 *  </NAME>
 *  <AUTHOR>
 *    Anthony R. Cassandra
 *  </AUTHOR>
 *  <CREATE_DATE>
 *    July, 1998
 *  </CREATE_DATE>
 *
 *  <RCS_KEYWORD>
 *    $RCSfile: vertex-enum.c,v $
 *    $Source: /u/cvs/proj/pomdp-solve/src/vertex-enum.c,v $
 *    $Revision: 1.2 $
 *    $Date: 2004/10/10 03:44:54 $
 *  </RCS_KEYWORD>
 *
 *  <COPYRIGHT>
 *
 *    1994-1997, Brown University
 *    1998-2003, Anthony R. Cassandra
 *
 *    All Rights Reserved
 *                          
 *    Permission to use, copy, modify, and distribute this software and its
 *    documentation for any purpose other than its incorporation into a
 *    commercial product is hereby granted without fee, provided that the
 *    above copyright notice appear in all copies and that both that
 *    copyright notice and this permission notice appear in supporting
 *    documentation.
 * 
 *    ANTHONY CASSANDRA DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 *    INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR ANY
 *    PARTICULAR PURPOSE.  IN NO EVENT SHALL ANTHONY CASSANDRA BE LIABLE FOR
 *    ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 *    WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 *    ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 *    OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *  </COPYRIGHT>
 *
 *</SOURCE_HEADER>
 */

/*
 *   Implements the vertex enumeration algorithm by T. Mattheiss.  This
 *   algorithm finds all vertices of a convex polytope as expressed by a
 *   series of linear contraints.  It requires the CPLEX linear programming
 *   package and its C-callable librrary.  It is customized to handle Cheng's
 *   Linear support finite horizon POMDP algorithm.
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "mdp/mdp.h"

#include "global.h"
#include "pomdp.h"
#include "alpha.h"
#include "lp-interface.h"
#include "vertex-enum.h"

/**********************************************************************/
/* ALGORITHM STATE VARIABLES
 
   Because we only want to generate the vertices one by one as each is
   requested, we need to maintain a bunch of global variables that will 
   encode the current state of the enumeration algorithm.  These are
   the ones we need.
*/
int gVertexInit = 0;   /* Keeps track of whether we have set up a
                          vertex enumeration problem yet to prevent
                          the getVertex() routine from accessing bad
                          info.  */

int gNumVariables = 0;  /* Keeps track of the current number of
                           variables in the problem.  This does not
                           include the extra variable that is added to
                           embed the polytope in a higher dimensional
                           space.  Therefore, this will be equal to
                           (tm_cols - 1) */

int gNumConstraints = 0;  /* Keeps the number of constraints in the
                             currently defined problem.  This will be
                             exactly the same as the variable tm_rows
                             and also corresponds to how many slack
                             variables there will need to be.  */

int gTotalColumns = 0;   /* Keeps the number of actual columns in the
                            LP including the original variables, the
                            added variable and the slack variables.
                            It will always be equal to
                            (gNumVariables+gNumConstraints+1) but will
                            we keep it handy to avoid having to do
                            this calculation explicitly when needed */

int gBasicSlackBytes = 0;  /* Since we will represent the basic slacks
                             as a bit vector for quicker comparisons,
                             we need to know how many bytes are needed
                             to specify 'gNumConstraints' bits.
                             Generally, it will be: ceil(
                             gNumConstraints/(sizeof(unsigned int
                             )*8)) */

int gCurCol;            /* Keeps track of the current non-basic column
                           that we are considering for the current
                           tableau.  For each tableau, we need to look
                           at all the non-basic slack variable
                           columns.  Note that the column here will
                           represent the actual column in the CPLEX
                           representation.  It will only ever
                           correspond to the slack variable columns
                           and thus for slack variable 'i', it will
                           take on the value: (gNumVariables + i + 1).
                           Since the algorithm will be periodically
                           suspended (after each time it returns a
                           vertex) the semantics of this variable are
                           that it is the last non-basic column that
                           was checked or BIG_COL_NUM if the algorithm
                           has enumerated no vertices yet.  */

int *gCurSlackBasis = NULL;   /* This will contain the long form (one
                                 integer for each slack variable) of
                                 the slack basis for the current
                                 tableau.  The bit form will be stored
                                 in the bit_basis field of gCurRecord.  */

int *gColBasis = NULL;        /* Although we will only ever consider
                                 LP basis' where all the original
                                 variables will be in the basis, we
                                 still need to tell CPLEX that they
                                 are.  This will contain a vector of
                                 all BASIC values which will not
                                 change, except for the very last
                                 element.  The last element will
                                 actually correspond to the added
                                 variable, which may or may not be in
                                 the basis */

Record_List gRecordList = NULL;  /* This is the master data structure
                                    used to store the records of
                                    Mattheiss' algorithm.  It is a
                                    linked list that is ordered by the
                                    'y' field */

Record_List gCurRecord = NULL;  /* Each record will correspond to a
                                   tableau which we need to construct.
                                   A record is removed from the list,
                                   the tableau created and then the
                                   non-basic slacks examined.  This
                                   variable will hold a point to the
                                   record which gave rise to the
                                   current tableau or NULL if no
                                   tableau exists.  */

/* end of ALGORITHM STATE VARIABLES */
/**********************************************************************/
/* LINEAR PROGRAMMING VARIABLES */


LP gVertexEnumLp;

/**********************************************************************/
/* TEMPORARY VARIABLES

   There are many times where temporary arrays need to be utilized.
   Since we do not know the size of the problem before hand, they
   must be dynamically allocated, to avoid excessive dynamic
   allocations and freeing of memory, we will keep this pool
   of temporary variables which we will allocate and deallocate
   only as the problem size changes.
*/
unsigned int *bit_basis;
int *col_basis, *row_basis;
double *vertex;
double *tableaux_col, *tableaux_rhs;  /* used when checking pivots */
double *tableaux_row;
int *tableaux_bv;
double *x_vals, *slack_vals;
int *row_ratio_winners;

/* There are a few CPLEX routines where we do not want some of the 
   results, and which we cannot just specify NULL for there arguments.
   In order to avoid conflicts with other variables in use, these
   should be used.  They will have a size equal to the sum of the
   number of columns and rows of the LP, so they can be used in 
   accessing anything from the LP without being too small
   */
double *dummy_double;
int *dummy_int;

/* END TEMPORARY VARIABLES */
/**********************************************************************/


/**********************************************************************/
/***************   Utility/Miscellaneous Routines     *****************/
/**********************************************************************/

/**********************************************************************/
int 
initVertexEnum( int num_orig_variables ) 
{
  /*
    This routine allocates the memory that will not change from
    one problem to the next.  These variables only depend upon
    the number of variables in the problem, which we assume will
    be the same for all instances that the algorithm is used.
  */
  LP lp;
  int i;
  
  lp =  (LP) XMALLOC( sizeof( *lp ));
  
  strcpy( lp->name, "vertex-enum" );
  
  /* Start with no LP loaded. */
  lp->lp = NULL;

  /* Unused variables */
  lp->x = NULL;
  lp->pi = NULL;
  lp->slack = NULL;
  lp->dj = NULL;

  /* Default value to use when considering whether to include a
     coefficient in a sparse representation. Note that we don't really
     want to tie this to the precision that is being used to solve the
     problem, because this value can change the problem being solved.
     Thus this should just be fixed for all time at the minimum
     precision. */
  lp->sparse_epsilon = SMALLEST_PRECISION;
  
  /* Set this sop we get the optimize() call instead of the dualopt()
     call in LP_solveLP() */ 
  lp->lp_algorithm = primal_simplex;

   
  lp->objsen = MAXIMIZE;
  lp->cols = lp->colspace = num_orig_variables + 1;  
  /* remember, the algorithm adds a variable */
  
  lp->obj = (double *) XMALLOC ( sizeof( double ) * lp->cols );
  lp->lowbnd = (double *) XMALLOC ( sizeof( double ) * lp->cols );
  lp->upbnd = (double *) XMALLOC ( sizeof( double ) * lp->cols );
  lp->matbeg = (int *) XMALLOC ( sizeof( int ) * lp->cols );
  lp->matcnt = (int *) XMALLOC ( sizeof( int ) * lp->cols );
  
  /* the objective function is always the same, so we might as well 
     set it here */
  for( i = 0; i < lp->cols; i++ ) {
    lp->obj[i] = 0.0;
    lp->lowbnd[i] = 0.0;
    lp->upbnd[i] = INFBOUND;
  }
  lp->obj[lp->cols-1] = 1.0;  /* maximize 'y', the added variable */
  
  /* Set the global variable that we will use. */
  gVertexEnumLp = lp;

}  /* initVertexEnum */
/**********************************************************************/
int 
cleanUpVertexEnum( ) 
{
  /*
    This frees the memory that was static for all the vertex enumeration
    constraint sets.  It undoes the allocation that the routine
    initMattheis() set up.
  */
  LP lp;

  lp = gVertexEnumLp;

  XFREE( lp->obj );
  XFREE( lp->lowbnd );
  XFREE( lp->upbnd );
  XFREE( lp->matbeg );
  XFREE( lp->matcnt );

  gVertexEnumLp = NULL;

}  /* cleanUpVertexEnum */
/**********************************************************************/

/**********************************************************************/
/******************   Routines for Basic Slacks     *******************/
/**********************************************************************/
int 
basisToBits( unsigned int *basis_bits, int *basis ) 
{
  /*
    This routine converts an integer array of size gNumConstraints
    (representing which slacks variables are in the basis of an LP
    tableau) and converts it into a more compact bit representation
    for subsequent comparisons and storage in a list.  This routine
    assumes that the appropriate memory has been allocated, namely
    gNumConstraints for 'basis' and gBasicSlackBytes for 'basis_bits'.
    If there is a problem the rouine will return 0, otherwise it
    returns 1.  The input 'basis' will be CPLEX's representation for
    the basis status of slack variable, namely 0 or 3 - non-basic and
    1 or 2 for basic.
  */
   int byte_index;
   int bit_index;
   int basis_index;
   unsigned int mask;

   if(( basis_bits == NULL) || ( basis == NULL ))
      return 0;

   for( basis_index = 0, byte_index = 0; 
       basis_index < gNumConstraints; 
       basis_index += (8 * sizeof( unsigned int)), byte_index++ ) {
      
      basis_bits[byte_index] = 0;
      mask = 01;

      for( bit_index = 0; 
          bit_index < (8 * sizeof( unsigned int ));
          bit_index++, mask *= 2 ) {

         if( (basis_index + bit_index) >= gNumConstraints )
            return( 1 );

         if(( basis[ basis_index + bit_index ] == BASIC )
            || ( basis[ basis_index + bit_index ] == BASIC_AT_UPPER_BOUND ))
            basis_bits[byte_index] |= mask;

      }  /* for bit_index */
   }  /* for basis_index */

   return( 1 );
}  /* basisToBits */
/**********************************************************************/
int 
bitsToBasis( int *basis, unsigned int *basis_bits ) 
{
  /*
    This routine is the exact inverse of the basisToBits routine
    and all of the assumptions there hold.  The only differerence
    is that the resulkting basis uses only 0 for non-basic and
    1 for basic.
  */
   int byte_index;
   int bit_index;
   int basis_index;
   int mask;

   if(( basis_bits == NULL) || ( basis == NULL ))
      return 0;

   for( basis_index = 0, byte_index = 0; 
       basis_index < gNumConstraints; 
       basis_index += (8 * sizeof( unsigned int)), byte_index++ ) {
      
      mask = 1;

      for( bit_index = 0; 
          bit_index < (8 * sizeof( unsigned int ));
          bit_index++, mask *= 2 ) {

         if( (basis_index + bit_index) >= gNumConstraints )
            return( 1 );

         if( basis_bits[byte_index] & mask )
            basis[ basis_index + bit_index] = BASIC;
         else
            basis[ basis_index + bit_index] = NON_BASIC;

      }  /* for bit_index */
   }  /* for basis_index */

   return( 1 );
}  /* bitsToBasis */
/**********************************************************************/
int 
basisEqual( unsigned int *basis1, unsigned int *basis2 ) 
{
  /*
    Compares to slack basis' to see if they are exactly the same and if
    they are it returns 1.  Zero is returned for inequality and -1 if
    an error ocurrs.
  */
   int i;

   if (( basis2 == NULL) || ( basis2 == NULL))
      return( -1 );

   for( i = 0; i < gBasicSlackBytes; i++ )
      if( basis1[i] != basis2[i] )
         return( 0 );

   return( 1 );
}  /* basisEqual */
/**********************************************************************/

/**********************************************************************/
/******************      Record List Routines      *******************/
/**********************************************************************/
Record_List 
createRecordNode( double y, unsigned int *basic_slacks ) 
{
  /*
    Allocates memory for a new record node, including space for the
    basic slack array.  It will copy the array sent in.
  */   
   Record_List node;
   int i;

   /* allocate memory */
   node = (Record_List) XMALLOC( sizeof( *node ) );
   node->basic_slacks 
      = (unsigned int *) XMALLOC( gBasicSlackBytes * sizeof( unsigned int ));

   /* set fields */
   node->y = y;
   for( i = 0; i < gBasicSlackBytes; i++ )
      node->basic_slacks[i] = basic_slacks[i];
   node->used = 0;
   node->next = NULL;

   return( node );
   
}  /* createRecordNode */
/**********************************************************************/
Record_List 
addRecordUnique( Record_List list,
		 double y,
		 unsigned int *basic_slacks,
		 int *result ) 
{
  /*
    Takes a record list and will create and add a new record, if the
    'basic_slacks' list is not already in the list.  It will return the
    resulting list (with or without the added element) and the parameter
    'result' will return 1 if record added or 0 if it already exists.
    If the record will be added, new memory for the basic_slacks will
    be created with those values copied into the new area, so you can
    reuse the memory sent in.  The list is ordered by the 'y' value from
    greatest to least.
    
    This routine will walk down the entire list to compare ther basis
    against everything in the list (note the inefficiency).  However,
    as it walks down the list it will also look at the 'y' values to 
    see where it should be inserted and will save the record pointer
    just before the point of insertion.  If the record is to be inserted 
    at the front, then the insertion point will be NULL;
    
    We must insert duplicate 'y' values *after* all records that
    have the same 'y' value.  If we are working on a record with
    the same 'y' value and we insert it before that record, then 
    advancing the current record by ->next will miss this new record.
  */

   Record_List insert_point = NULL;
   Record_List walk_ptr;
   Record_List new_node;

   *result = 0;
   walk_ptr = list;
   while( walk_ptr != NULL ) {

      if( walk_ptr->y > y )
         insert_point = walk_ptr;

      if( basisEqual( walk_ptr->basic_slacks, basic_slacks ) )
         return( list );

      walk_ptr = walk_ptr->next;
   }  /* while */

   /* We only get here if we decided we need to insert this record */
   *result = 1;

   /* Make the node */
   new_node = createRecordNode( y, basic_slacks );

   /* add in proper place */
   if( insert_point == NULL ) {
      new_node->next = list;
      return( new_node );
   }

   new_node->next = insert_point->next;
   insert_point->next = new_node;
   
   return( list );

}  /* addRecordUnique */
/**********************************************************************/
Record_List 
dequeueRecord( Record_List *list ) 
{
  /*
    Takes a pointer to a record list pointer and removes the element
    of the list with the highest 'y' value.  The list will will
    updated accordingly.  If the list is empty, then NULL will be
    returned.
  */
   Record_List node;

   if( *list == NULL )
      return NULL;

   node = *list;
   *list = (*list)->next;
   node->next = NULL;

   return( node );

}  /* dequeueRecord */
/**********************************************************************/
Record_List 
getFirstUnusedRecord( Record_List list ) 
{
   
   while( (list != NULL) && list->used ) 
      list = list->next;

   return( list );
}  /* getFirstUnusedRecord */
/**********************************************************************/
void 
destroyRecordNode( Record_List node ) 
{

   if( node == NULL )
      return;

   XFREE( node->basic_slacks );
   XFREE( node );

}  /* destroyRecordNode */
/**********************************************************************/
Record_List 
cleanRecordList( Record_List list, double y ) 
{
  /*
    With Mattheiss' algorithm, we cannot delete elements from the
    sorted record list until we start to work on a record with a
    strictly less 'y' value.  This routine will remove all records
    from the front of the list with 'y' value strictly greater
    than the 'y' value sent in.  Note that this assumes that
    the list is sorted correctly from greatest to least.  It might
    very well be the case that this routine will do nothing
    other than return the current list.
  */
   Record_List temp;

   while(( list != NULL) && (list->y > y )) {
      temp = list;
      list = list->next;

      if( temp->used == 0 )
         fprintf( gStdErrFile,
                  "WARNING! Removing an unused record from list.\n");

      destroyRecordNode( temp );
   }  /* while */

   return( list );
}  /* cleanRecordList */
/**********************************************************************/
void 
destroyRecordList( Record_List list ) 
{
  /*
    Free all memory associated with the record list.
  */
    Record_List temp;
  
    while( list != NULL ) {
       temp = list;
       list = list->next;

      if( temp->used == 0 )
         fprintf( gStdErrFile,
                  "WARNING! Removing an unused record from list.\n");

      destroyRecordNode( temp );
    } /* while */

}  /* destroyRecordList */
/**********************************************************************/

/**********************************************************************/
/*****************   Memory Allocation/Deallocation  ******************/
/**********************************************************************/
/**********************************************************************/
void 
allocateVertexEnumLP( LP lp, int num_orig_vars, 
		      int num_constraints ) 
{
  /* Allocate variables needed by CPLEX that will change from one
     LP to the next.  Note that there is a bunch of memory locations
     that will not change from one LP to the next.  For instance, they
     will all have the same number of variables or columns.  These are
     all allocated once at the initial program startup.
  */

   /* We will need a row for each constraint plus a row for each variable,
      since we need to explicitly put in the >= constraint to properly
      embed the polytope in a higher dimensional space.
      */
   lp->rows = lp->rowspace = num_orig_vars + num_constraints;

   /* We will assume the coef matrix for the original constraints
      will be dense, but that the extra variable constraints are
      sparse.  Therefore, for the original constraints we will have 
      col * constraints.  For the added constraints there will
      be two non-zero coefficients per added row (we have to add
      as many rows as there are variables in the original problem.
      */
   lp->matspace = lp->cols * num_constraints + 2 * num_orig_vars;

   lp->rhs = (double *) XMALLOC ( sizeof( double ) * lp->rows );
   lp->sense = (char *) XMALLOC ( sizeof( char ) * lp->rows );
   lp->matind = (int *) XMALLOC ( sizeof( int ) * lp->matspace );
   lp->matval = (double *) XMALLOC ( sizeof( double ) * lp->matspace );

}  /* allocateVertexEnumLP */
/**********************************************************************/
void 
freeVertexEnumLP( LP lp ) 
{

   /* free variables needed by CPLEX */
   lp->rows = lp->rowspace = lp->matspace = 0;
   XFREE( lp->rhs );
   XFREE( lp->sense );
   XFREE( lp->matind );
   XFREE( lp->matval );

}  /* freeVertexEnumLP */
/**********************************************************************/
void 
allocateTempMemory( LP lp ) 
{
   int i;

   /* allocate temporary variables to be used throughout enumeration */
   vertex = (double *)XMALLOC( gNumVariables * sizeof( double ));
   row_basis = (int *) XMALLOC( gNumConstraints * sizeof( int ));
   col_basis = (int *) XMALLOC( (gNumVariables+1) * sizeof( int ));
   gCurSlackBasis = (int *) XMALLOC( gNumConstraints * sizeof( int ));

   /* This array will remain constant except for the last element
      (which is not even initialized).  This will just be used to
      tell CPLEX that all the original variables (and possible the 
      added variable) should be included in the basis. 
      */
   gColBasis = (int *) XMALLOC( (gNumVariables + 1) * sizeof( int ));
   for( i = 0; i < gNumVariables; i++ )
      gColBasis[i] = BASIC;

   bit_basis = (unsigned int *) XCALLOC( gBasicSlackBytes,
                                      sizeof( unsigned int ));
   /* note that we must use calloc here, because if there are any unused
      bits (should only in the upper part of the last byte), then in order to 
      compare the last one, we have to ensure that the unsed bits are
      the same for all the bit_basis'.
      */

   x_vals = (double *)XMALLOC( (gNumVariables + 1) * sizeof( double ));
   slack_vals = (double *)XMALLOC( gNumConstraints * sizeof( double ));
   tableaux_col = (double *)XMALLOC( gNumConstraints * sizeof( double ));
   tableaux_row = (double *)XMALLOC( gNumConstraints * sizeof( double ));
   tableaux_rhs = (double *)XMALLOC( gNumConstraints * sizeof( double ));
   tableaux_bv = (int *) XMALLOC( gNumConstraints * sizeof( int ));

   row_ratio_winners = (int *) XMALLOC( gNumConstraints * sizeof( int ));

   dummy_double = (double *) XMALLOC( (lp->cols + lp->rows) * sizeof( double ));
   dummy_int = (int *) XMALLOC( (lp->cols + lp->rows) * sizeof( int ));

} /* allocateTempMemory */
/**********************************************************************/
void 
freeTempMemory() 
{
   
   /* Undoes the allocation done by allocateTempMemory() */
   XFREE( vertex );
   XFREE( gCurSlackBasis );
   XFREE( gColBasis );
   XFREE( row_basis );
   XFREE( col_basis );
   XFREE( bit_basis );
   XFREE( x_vals );
   XFREE( slack_vals );
   XFREE( tableaux_col );
   XFREE( tableaux_row );
   XFREE( tableaux_rhs );
   XFREE( tableaux_bv );
   XFREE( row_ratio_winners );
   XFREE( dummy_double );
   XFREE( dummy_int );

}  /* freeTempMemory */
/**********************************************************************/
/******************   Algorithm Specific Routines   *******************/
/**********************************************************************/
int 
cplexCallback( CPXLPptr lpinfo, int wherefrom ) 
{
  /*
    This routine will be called by CPLEX after each LP iteration.
    If it returns 1, then CPLEX stops optimization, which is what we 
    want it to do when we are trying to set the basis and get the 
    corresponding tableaux.  Just setting the basis in CPLEX does 
    not change the current solution, we need to call optimize()
    for it to actually start at this basis, immediately halting
    CPLEX (via this routine returning 1) gives the tableaux for
    the basis we set.
  */
   return( 1 );  /* stop optimization */

} /* cplexCallback */
/**********************************************************************/
int 
checkLPStatus( LP lp ) 
{
   int status;
   int result;

   result = LP_getSolution( lp );

   if( result )
      fprintf( gStdErrFile, "** WARN ** No LP solution exists\n");

   if( gVerbose[V_VERTEX_ENUM] && ( status == CPX_ABORT_INFEAS ))
      fprintf( gStdErrFile,
               "** WARN ** LP status is infeasible.\n" );
   
   return( status );
}  /* checkLPStatus */
/**********************************************************************/
int 
setTableaux( LP lp, int *slack_basis, int y_basis ) 
{
/*
   Takes a basis vector for slack variables and a boolean
   valued parameter indicating whether or not the added variable
   'y' is to be in the basis or not and sets the current tableaux
   (or whatever CPLEX uses to represent the current state of the 
   LP solution).  It returns the status returned from the optimize()
   routine..  Note that this routine assume that all the original 
   variables will always be in the basis.
*/
   int status;

   if( !gVertexInit || ( slack_basis == NULL ))
      return( 0 );

   /*  We will only ever change the last element of gColBasis since
       all tableaux will necessarily have all the original variables
       int the basis.  So we only need to check if the added variable
       is to be included in the basis or not
       */       
   if( y_basis )
      gColBasis[gNumVariables] = BASIC;
   else
      gColBasis[gNumVariables] = NON_BASIC;

   /* This doesn't really optimize, since the callback routine will
      stop CPLEX just as soon as it starts.  However, since it will
      start at the currently set basis, it has the desired effect
      of getting the current state of the solution to correspond
      to the basis that we set.
      */
   LP_loadbase( lp, gColBasis, slack_basis );

   status = LP_optimizeLP( lp );

   return( checkLPStatus( lp ));
}  /* setTableaux */
/**********************************************************************/
int 
getTableauxColumn( LP lp, int col, double *coefs ) 
{
  /*
    Constructs the 'col' column of the current tableaux.  It uses the
    cryptically named getgrad() CPLEX routine to get the column for
    the current tableaux.
  */   
   
   if( LP_getgrad( lp, col, dummy_int, coefs )) {
      fprintf( gStdErrFile, "CPLEX calling problem: getgrad().\n");
      exit( -1 );
   }

}  /* getTableauxColumn */
/**********************************************************************/
int 
getTableauxRHS( LP lp, double *coefs ) 
{
  /*
    Constructs the right hand side values of the current tableaux.
    There doesn't seem to be a direct way to get at this from
    CPLEX.  However, I can indirectly do this by getting the solution
    values for the original and slack variables and then examining
    the basic variable for each row.  The tableau is such that the
    RHS is always equal to the value of the variable that is basic
    in that row.
  */
   int row;

   /* Get solution values for original (and added) variables */
   if( LP_getx( lp, x_vals, 0, gNumVariables )) {
      fprintf( gStdErrFile, "CPLEX calling problem: getx().\n");
      exit( -1 );
   }

   /* Get solution values for slack variables */
   if( LP_getslack( lp, slack_vals, 0, gNumConstraints - 1 )) {
      fprintf( gStdErrFile, "CPLEX calling problem: getslack().\n");
      exit( -1 );
   }

   /* Get the list of basic variables for each row. */
   if( LP_getgrad( lp, 0, tableaux_bv, dummy_double )) {
      fprintf( gStdErrFile, "CPLEX calling problem: getgrad().\n");
      exit( -1 );
   }

   /* For each row set the RHS depending on the basic variable of that
      row.  Note that CPLEX stores original variables as positive
      numbers (the actual index) and slack variables as one less than
      the negative of its index.  
      */
   for( row = 0; row < gNumConstraints; row++ ) 
      if( tableaux_bv[row] >= 0 )
         coefs[row] = x_vals[ tableaux_bv[row]];

      else 
         coefs[row] = slack_vals[ (tableaux_bv[row] + 1 ) * -1 ];

   return( 1 );

}  /* getTableauxRHS */
/**********************************************************************/
int 
findPivotRow( LP lp, int col, int *rows_to_pivot ) 
{
  /*
    This routine assumes that we have determined that the variable
    corresponding to the tableaux column 'col' is to be pivotted in.
    It will do a ratio test on each row that meets certain criteria to
    determine which row it will become basic in.  It will return this
    row or if no rows meet the conditions, it will return -1.  It also
    returns -1 if the objective row coef of the current column is not
    less than zero.  Note that this indirectly determines the leaving
    variable, since the variable that is currently basic in this row
    will leave.  However, this routine does not attempt to determine
    what that variable is.  Naturally, it assume that a valid tableaux
    exists.  

    Since we need to be concerned with rows that tie in the mimimum ratio
    test, we will return a vector of boolean values, rows_to_pivot,
    that will have a 1 for the rows to pivot and 0 for those that are losers
    in the ratio test.  If no minimum ratio is found (either the objective
    row coef is <= 0 or all the colun coefs are <= 0) then the routine
    will return 0, otherwise the routine retruns the number of rows
    that meet the minimum.
    
    We can probably do the whole thing in a single loop, but I'll wait to 
    do any optimizations of that sort.
  */

   double min_ratio = HUGE_VAL;
   double cur_ratio;
   int row;
   double obj_coef;
   int num_min_rows = 0;

   /* First we need to determine if this is an original variable or
      slack column in order to know where to get the objective row
      coefficient from.
      */
   if( col < lp->cols ) {
      if( LP_getdj( lp, &obj_coef, col, col )) {
         fprintf( gStdErrFile, "CPLEX calling problem: getdj().\n");
         exit( -1 );
      }
   }
   else {  /* it is a slack column */
      if( LP_getpi( lp, &obj_coef, col - lp->cols, col - lp->cols )) {
         fprintf( gStdErrFile, "CPLEX calling problem: getpi().\n");
         exit( -1 );
      }
   }

   /* If the objective row coef of this column is < 0 then we
      don't consider it for a pivot, since this just leads us 
      closer to the optimal solution.  Our search started at 
      the optimal and works backwards, so we never need to work
      toward optimal.  Note that this is opposite of what 
      Mattheiss' algorithm says to do (he says > 0), but note
      that his objective row is also '-z', whereas we use just
      plain 'z'.  Hence, the sign is reversed.
      */
   if( obj_coef < NEG_PRECISION_TOLERANCE )
      return( 0 );
   
   /* Get the column of current tableau */
   getTableauxColumn( lp, col, tableaux_col );

   /* get the RHS for current tableau */
   getTableauxRHS( lp, tableaux_rhs );

   /* First find the minimum value for the ratio with this 
      loop.
      */
   for( row = 0; row < lp->rows; row++ ) {
      /* only consider ratios where coef is positive */
      if( tableaux_col[row] <= PRECISION_TOLERANCE )
         continue;

      cur_ratio = tableaux_rhs[row] / tableaux_col[row];
      
      if( cur_ratio < min_ratio ) 
         min_ratio = cur_ratio;
      
   } /* for row */

   /* Now we check every rows' ratio to this minimum and keep track
    with this loop.
    */
   for( row = 0; row < lp->rows; row++ ) {
      /* only consider ratios where coef is positive */
      if( tableaux_col[row] <= PRECISION_TOLERANCE ) {
         rows_to_pivot[row] = 0;
         continue;
      }
      cur_ratio = tableaux_rhs[row] / tableaux_col[row];
      
      if(( cur_ratio <= (min_ratio + PRECISION_TOLERANCE ))
         && ( cur_ratio >= (min_ratio - PRECISION_TOLERANCE ))) {
         rows_to_pivot[row] = 1;
         num_min_rows++;
      }
      else
         rows_to_pivot[row] = 0;
      
   } /* for row */

   return( num_min_rows );
}  /* findPivotRow */
/**********************************************************************/
int 
findBasicVariable( LP lp, int row, int* type, int *index ) 
{
  /*
    This routine will return the basic variable of the current tableau
    for the row specified in 'row'.  Since there are three distinct
    type of variables it returns the information in two parts: the type
    (either ORIGINAL_VARIABLE , ADDED_VARIABLE or SLACK_VARIABLE) and
    the index.  Note that for the added variable the index is not essential.
    This is because CPLEX is not aware that this variable is special in any 
    way.  CPLEX only knows about the original variables (which the added
    variable is a part of) and the slack variables it adds for each 
    constraint.  This routine will pick out the added variable and
    flag it as special, however it will also return the index of this 
    variable as gNumVariables since that is the index by which it can be 
    accessed.  If a problem ocurrs then the routine will return 0,
    if all goes well it returns 1.
  */

   if( LP_getgrad( lp, 0, tableaux_bv, dummy_double )) {
      fprintf( gStdErrFile, "CPLEX calling problem: getgrad().\n");
      exit( -1 );
   }

   /* CPLEX returns (via getgrad) and array of indices for the basic
      variables.  For the orginal problem variables the number is just
      its index (this will include the added variable whose index is
      gNumVariables).  The slack variables are indicated by 1 less
      than the negative of their index 
      */
   if( tableaux_bv[row] >= 0 ) {
      if( tableaux_bv[row] == gNumVariables )
            *type = ADDED_VARIABLE;  
      else
         *type = ORIGINAL_VARIABLE;
      *index = tableaux_bv[row];
   }
   else {
      *type = SLACK_VARIABLE;
      *index = (tableaux_bv[row] + 1 ) * -1;    
   }

   return( 1 );

}  /* findBasicVariable */
/**********************************************************************/
int 
verifyBasis( LP lp, int *row_basis ) 
{
  /*
    Make sure we do not have a tableaux where there are more slacks in
    the basis than we expect.  When there is degeneracy in the problem,
    it might be the case that not all of the x_i and/or y are actually
    in the basis.  Mattheiss says that the optimal tableaux is
    guaranteed to have all the x_i and y, but I think what he really
    meant was that there exists an optimal tableaux with such a
    property.  With degenerate problems, there could be more than 1
    optimal tableaux, so we need to ensure that we pick an optimal
    tableaux that has this property.

    We check to see which slacks are in the basis.  If there are too
    many then that means that there must be some x_i and/or y that are
    not in the basis.  We decide which slacks we will kick out of the
    basis, by the RHS of the row they are basic in (or their current
    solution value).  If the RHS is zero, then they will have zero
    value whether or not they are in the basis.  Furthermore the zero
    RHS will not change the values of any variables, since the addition
    or subtraction of this row to reformulate the tableaux will leave
    the RHS values in tact.  Therefore we can pivot in the omitted x_i
    or y in the row with the slack variable's RHS value is zero, and
    get an equivalent objective value but now with the x_i or y in the
    basis.  Note that whether in or out of the basis, these particular
    x_i that enter or the slacks that leave will have zero value.  

    Note that this only needs to be done when adding getting the basis 
    for the first tableaux.  All other tableaux's are specifically set by 
    this program and will thus have exactly the property we are trying 
    to verify with this routine.

    There is a sticky precision thing which makes this routine a little 
    more complex than it should be.  We will kick out basics that are 
    essentially zero, but we also need to kick them out in value order
    from least to greatest until we have reduced the number of slacks
    to the appropriate amount.  This is required as illustrated by 
    the following example:

    Let's say we only need to get rid of one slack and we have (in order)
    these two slack variable values:

    slack_vals[5] = -4.9294179453189837e-16
    slack_vals[14] = -4.163336342344337e-17

    The first one is essentially zero, but if we kick this one out then
    the RHS of the slack basic's row will become a large number, since
    they are relatively close in value.  This can cause the tableaux that 
    would result with the new basis to be infeasible, which we would rather
    not have.

  */
   int num_slacks, i;
   int count = 0;

   /* Since we always expect there to be all the x_i and y in the
      basis, it will leave lp->rows - lp->cols variable to fill out
      the basis.
      */
   num_slacks = lp->rows - lp->cols;

   /* First see how many are in the basis */
   for( i = 0; i < lp->rows; i++ ) 
      if( row_basis[i] == BASIC ) 
         count++;

   /* If the right amount, then return */
   if( count == num_slacks )
      return( count );

   if( count < num_slacks ) {
      fprintf( gStdErrFile, "** ERR ** Not enough slacks in basis!\n");
      fprintf( gStdErrFile, "          This shouldn't happen. Aborting.\n");
      exit( -1 );
   }

   /* Get solution values for slack variables (i.e., their RHS) */
   if( LP_getslack( lp, slack_vals, 0, gNumConstraints - 1 )) {
      fprintf( gStdErrFile, "CPLEX calling problem: getslack().\n");
      exit( -1 );
   }

   /* At this point we need to kick out some slacks, so we see which
      of the basic slacks have zero value */

   /* We now set count to be the number of slacks we need to remove from
      the basis.
      */
   count -= num_slacks;

   for( i = 0; i < lp->rows; i++ ) 

      if(( row_basis[i] == BASIC ) 
         && ( slack_vals[i] < PRECISION_TOLERANCE )) {
         count--;
         row_basis[i] = NON_BASIC;

         if( count == 0 )
            return( num_slacks );
      }

   if( count > 0 ) {
      fprintf( gStdErrFile, "** ERR ** Too many non-zero slacks in basis!\n");
      fprintf( gStdErrFile, "          This shouldn't happen. Aborting.\n");
      exit( -1 );
   }

   return( num_slacks );
}  /* verifyBasis */
/**********************************************************************/
int 
updateRecordList( LP lp ) 
{
  /*
    This routine assumes that there is a current solution that is
    valid from the CPLEX data structures.  It will get the basis
    status of the slack variables and the current value of the added
    variable, 'y', from the current 'tableaux' and add this to the list
    of records, if this basis set is not already on the list.
  */
   double y;
   int result;
   int i;

   /* Get the 'y' value (added or objective variable) 
    assume that the added variable is the last one in the 
    solution vector */

   if( LP_getx( lp, &y, gNumVariables, gNumVariables )) {
      fprintf( gStdErrFile, "CPLEX calling problem: getx().\n");
      exit( -1 );
   }
  
   /* Get the basis status of slacks */
   if( LP_getbase( lp, NULL, row_basis )) {
      fprintf( gStdErrFile, "CPLEX calling problem: getbase.\n");
      exit( -1 );
   }

   /* It is possible that not all of the x_i or y are in the current
      basis.  This happens when there is high degeneracy, such as when
      specifying only the simplex area.  When there are more slacks in
      the basis than we want, we assume that there is an equivalent
      optimal solution that has all the x_i, y and the slacks from the
      current basis minus any overflow of these. (i.e., any subset of
      the current slack basic variables that fill up the basis when
      all the x_i and y are considered will have the same objective
      value.  
      */
   verifyBasis( lp, row_basis );

   basisToBits( bit_basis, row_basis );

   if( gVerbose[V_VERTEX_ENUM] ) {
      fprintf( gStdErrFile, "Adding record: y = %.2lf, ( ", y);
      for( i = 0; i < gNumConstraints; i++ )
         if( row_basis[i] == BASIC )
            fprintf( gStdErrFile, "%d ", i );
      fprintf( gStdErrFile, ")\n");
   }

   gRecordList = addRecordUnique( gRecordList, y, bit_basis, &result );

   if( gVerbose[V_VERTEX_ENUM] && !result )
      fprintf( gStdErrFile, "Duplicate record, not adding to list.\n");

} /* updateRecordList */
/**********************************************************************/
int 
doUSPivot( LP lp, int entering_var, int leaving_var ) 
{
  /* 
     Change the current basis, but only temporarily.  We will
     get the tableau for this new basis just to get at the value 
     of the added variable 'y' in this tableau.  We need the value 
     of 'y' to decide where to put it in the ordered linked
     record list.  Note that we might not have to add this tableaux
     to the list, since the new basis might already be in the list.
     We could check this before construction the tableau, but then
     we will need to search the linked list twice: once to decide
     if it should go in and another to decide where it should go
     (assuming it does go into the list).  If we first get the value
     of 'y', as we search the list to see if the basis is in the
     list, we can also decide where it will go by comparing 'y'
     values along the way.  I do not know which is better.
  */

   /* Temporarily change the basis and tableau */
   gCurSlackBasis[entering_var] = BASIC;
   gCurSlackBasis[leaving_var] = NON_BASIC;
   
   /* On the off chance that we try to set the tableaux to
      a bad basis, we only update the record list if
      the status returned in CPX_ABORT_FEAS.
      */
   if( setTableaux( lp, gCurSlackBasis, Y_IN_BASIS ) == CPX_ABORT_FEAS )

      /* Note that this routine will get the basis from CPLEX, which
         is not necessary (assuming everything goes right) since we
         know what the basis is (i.e., gCurSlackBasis).  However,
         this will be a good sanity check point and should not be
         too time consuming
         */
      updateRecordList( lp );
   
   /* Switch the basis back to the way it was so we can get the 
      tableaux back to what it was when this routine was called.
      */
   gCurSlackBasis[entering_var] = NON_BASIC;
   gCurSlackBasis[leaving_var] = BASIC;
  
   if( setTableaux( lp, gCurSlackBasis, Y_IN_BASIS ) == CPX_ABORT_INFEAS )
      fprintf( gStdErrFile, 
               "** ERR ** Restored tableaux not feasible. (US)\n");
   
   return( 1 );
}  /* doUSPivot */
/**********************************************************************/
int 
doUYPivot( LP lp, int entering_var, double *vertex ) 
{
  /*
    This routine is called when we find that we would have to pivot
    the added variable 'y' out of the basis.  In Mattheiss' algorithm,
    this means that we are at one of the vertex points of interest.
    Therefore we construct the tableaux and get the solution values
    for the original variables. We then need to revert back to the tableaux
    and basis that we had at the start of this routine.

    This routine returns 1 if all goes well, and 0 if we have a problem
    doing the UY pivot.
  */

   /* Temporarily change the basis and tableau */
   gCurSlackBasis[entering_var] = BASIC;

   /* There is a chance that we could set the tableux to be infeasible.
      When this happens we should just restore the tableaux and move on.
      */
   if( setTableaux( lp, gCurSlackBasis, NOT_Y_IN_BASIS )
       == CPX_ABORT_INFEAS ) {
      fprintf( gStdErrFile, 
               "** WARN ** Cannot set tableaux for UY pivot.\n");
      gCurSlackBasis[entering_var] = NON_BASIC;
  
      if( setTableaux( lp, gCurSlackBasis, Y_IN_BASIS )
          == CPX_ABORT_INFEAS )
         fprintf( gStdErrFile, 
                  "** ERR ** Restored tableaux not feasible. (US)\n");

      return( 0 );
   }

   /* Get the 'vertex' values, which are all but the last
    column variables in the LP formulation */
   if( LP_getx( lp, vertex, 0, gNumVariables-1 )) {
      fprintf( gStdErrFile, "CPLEX calling problem: getx().\n");
      exit( -1 );
   }
   
   /* Switch the basis back to the way it was so we can get the 
      tableaux back to what it was when this routine was called.
      */
   gCurSlackBasis[entering_var] = NON_BASIC;
   if( setTableaux( lp, gCurSlackBasis, Y_IN_BASIS ) == CPX_ABORT_INFEAS )
      fprintf( gStdErrFile, 
               "** ERR ** Restored tableaux not feasible. (UY)\n");

   return( 1 );
}  /* doUYPivot */
/**********************************************************************/
int 
doPivotCheck( LP lp, int col, double *b ) 
{
  /*
    This routine we examine the current non-basic slack, which is held
    in col to see what variable would have to leave the basis in
    order for the non-basic slack to enter the basis.  Depending on the
    leavinhg variable, we might either ignore (UX), output a vertex
    (UY) or add an element to the record list (US).  For the latter two
    cases we will actually need to pivot in the non-basic variable to
    get some information from the resulting tableaux.  However, since
    the overall algorithm is searching through non-basic variables of
    some tableaux (the tableau that exists when this routine is called,
    this routine must restore the tableau to its original form.
    Fortunately, we have the gCurRecord variable that defines the basis
    for that tableau.  Note that we don't actually pivot per se, we
    simply tell CPLEX what we want the basis to be and it does the
    rest.  When a vertex is found the input parameter 'b' will be
    set to the vertex and the value 1 returned.  For all other cases
    the value 0 will be returned indicating that we did not find a
    vertex.

    When there are more than one row that are minimum, we will first
    check for a UY pivot, and if so we output a vertex and ignore any
    US pivots.  When there are multiple US pivot we will only pay
    attention to the first one.  In all cases we ignore any rows
    corresponding to UX pivots.
  */
   int row,                 /* The row which wins ratio test */
       leaving_var_type,    /* The leaving variable type (s, x, or y) */
       leaving_var_index;   /* The index of the leaving variable */
   int entering_var_index;  /* always a slack and a simple function of
                               the 'col' parameter, but useful to
                               have around */
   double min_ratio;
   int uy_pivot_row = -1;   /* Want to keep track of where the UY pivot is,
                               if any, since we will process it last.
                               */
   int num_rows;

   entering_var_index = col - gNumVariables - 1;

   /* Find the row(s) to pivot it into.  Note that there could be 
      ties in the ratio test which will mean we have to pivot in 
      more than one row.  This routine will return a boolean 
      vector indicating which rows won the minimum ratio test. */
   num_rows = findPivotRow( lp, col, row_ratio_winners );
   
   /* If no winners, just exit here.  No pivots possible */
   if( num_rows == 0 )
      return( 0 );

   /* We now loop through all the rows and pick out the ones that
      were winners in the ratio test.  Ignoring UX pivots,
      taking UY pivot if there is one and the first US pivot
      if there are more than one.  This assumes that the rows
      are ordered by their basic variables, x_i, y, s_i in that
      order.
      */
   for( row = 0; row < gNumConstraints; row++ ) {
      if( row_ratio_winners[row] == 0 )
         continue;   /* 'row' is a ratio loser */
     
      findBasicVariable( lp, row, 
                         &leaving_var_type, &leaving_var_index );

      if( gVerbose[V_VERTEX_ENUM] ) 
         fprintf( gStdErrFile,
                  "Entering variable col: %d, leaving variable row: %d\n",
                  col, row );

      switch( leaving_var_type ) {
      case ORIGINAL_VARIABLE:
         /* UX Pivot
            
            If variable to pivot out is one of problem variables, just
            ignore it, since we are only interested in solutions where
            all the variables are part of the basis.  When this occurs,
            ignore it.
            */
         break;

      case ADDED_VARIABLE:
         /* UY Pivot
            
            If the variable to pivot out is the added variable, 'y',
            then we have found a vertex.  Do the pivot to get the
            values for the problem variables and set it to 'b'.
            return 1 to indicate vertex found.  
            */

         /* First we check if the current tableaux has the added 
            variable == to zero.  This is a special case which we
            flag when we first load in this record's tableaux and
            generate a vertex for it.  If we do a UY pivot and the
            'y' value is zero, it will not change the values of the
            x_i and will only result in a redundant point.  So if
            we are searching a tableaux that has 'y=0' and find a UY
            pivot, we will just ignore it.  Note we can just check for
            equality with zero with the inequality because of the 
            non-negativity of all variables in the LP.
            */
         if( gCurRecord->y < PRECISION_TOLERANCE)
            return( 0 );

         if( doUYPivot( lp, entering_var_index, b ) )
            return( 1 );

         else  /* there was a problem setting the tableaux, so don't
                  indicate that we found a vertex */
            return( 0 );
         
      case SLACK_VARIABLE:
         /* US Pivot

            If the variable to pivot out is another slack variable,
            then we need to construct that tableaux. Construct the
            tableau to get the value for 'y' (and the new basis if you
            like) and add it to the list.  
            */
         doUSPivot( lp, entering_var_index, leaving_var_index );
         return( 0 );
         
      default:
         fprintf( gStdErrFile, 
                 "**ERR** doPivotcheck: Unreckognized leaving var type.\n");
         exit( -1 );
         
      }  /* switch */
      
   }  /* for row */

   return( 0 );  /* no UY or US pivots found */

}  /* doPivotCheck */
/**********************************************************************/
int 
getNextRecord( LP lp ) 
{
  /*
    This routine is called when we need to update the current tableau
    with a record from the record list.  This will need to clean up the 
    current tableau and update all the data.  Note that the first time 
    this is called there will not be a current tableau, so we need to 
    check for this.  If a record is removed and the tableau is updated,
    then this routine returns 1 and sets gCurCol to be BIG_COL_NUM.  If
    there are no more records, then all vertices have been enumerated
    and the routine returns 0.
  */

   gCurCol = gNumVariables;

   /* We need the loop, in case some of the records in the record
      list lead to infeasible tableaux.  In theory this should not
      happen, but with the degenerancy of the LPs we solve, we might
      have to adjust the basis which, unless done correctly, can
      lead to infeasible LPs.  We hope it is done correctly, but
      are not sure, so we put this in here for safe-guarding.  See
      the routine verifyBasis() for more information about this.
      */
   while( 1 ) {
      /* Just gives us a pointer into the list, doesn't remove it */
      gCurRecord = getFirstUnusedRecord( gRecordList );
   
      if( gCurRecord == NULL )
         return( 0 );

      /* remove any records at front of list that have a strictly
         greater value than the current record.
         */
      gRecordList = cleanRecordList( gRecordList, gCurRecord->y );
      
      bitsToBasis( gCurSlackBasis, gCurRecord->basic_slacks );
      
      /* Indicate that we have used this record */
      gCurRecord->used = 1;
      
      /* There is a chance that we will set the LP to be infeasible,
         if this happens we need to get another record off of the
         record list.
         */
      if( setTableaux( lp, gCurSlackBasis, Y_IN_BASIS )
          == CPX_ABORT_INFEAS ) {
         if( gVerbose[V_VERTEX_ENUM] )
            fprintf( gStdErrFile,
                     "**WARN** Infeasible tableaux, moving to next record.\n");

         continue;
      }
      
      /* otherwise we can just start to use this tableaux */
      return( 1 );

   } /* while */

}  /* getNextRecord */
/**********************************************************************/
int 
specialVertexCheck( LP lp, double *vertex ) 
{
  /*
    Although not explictly outlined in Mattheiss' paper, when the
    optimal solution has y=0 as the optimal solution value for the
    added variable we have a vertex in the original problem space.
    This routine checks for this case.  After a new tableau is created,
    and before we search through it, we look at its 'y' value and if it
    is zero, we output a vertex, before checking all the pivots.  This
    routine returns 1, if 'y=0' and 0 if not.  
  */
   if( gCurRecord->y < PRECISION_TOLERANCE ) {

      /* Then this is a vertex.  Get the 'vertex' values, which are
         all but the last column variables in the LP formulation 
         */
      if( LP_getx( lp, vertex, 0, gNumVariables-1 )) {
         fprintf( gStdErrFile, "CPLEX calling problem: getx().\n");
         exit( -1 );
      }

      else /* It is a vertex and we have gotten the values */
         return( 1 );

   }  /* if y == 0 */

   return( 0 );

}  /* specialVertexCheck  */

/**********************************************************************/
void 
showObjectiveRow( LP lp ) 
{
   int i;
   double obj;

   if( LP_getdj( lp, tableaux_col, 0, lp->cols-1 )) {
      fprintf( gStdErrFile, "CPLEX calling problem: getdj().\n");
      exit( -1 );
   }

   fprintf( gStdErrFile, " obj|   1.00");
   for( i = 0; i < lp->cols; i++ )
      fprintf( gStdErrFile, "    %.2lf", -1.0*tableaux_col[i] );

   if( LP_getpi( lp, tableaux_row, 0, lp->rows-1 )) {
      fprintf( gStdErrFile, "CPLEX calling problem: getpi().\n");
      exit( -1 );
   }

   for( i = 0; i < lp->rows; i++ )
      fprintf( gStdErrFile, "   %.2f", tableaux_row[i] );

   if( LP_getobjval( lp, &obj )) {
      fprintf( gStdErrFile, "CPLEX calling problem: getobjval().\n");
      exit( -1 );
   }

   fprintf( gStdErrFile, "   %.2lf |  z\n", obj);
}  /* showObjectiveRow */
/**********************************************************************/
int 
showTableauRow( LP lp, int row ) 
{
   int col;
   double coef;

   fprintf( gStdErrFile, " %3d|", row );
   
   fprintf( gStdErrFile, "   0.00");
   
   if( LP_binvarow( lp, row, tableaux_col )) {
      fprintf(  gStdErrFile, "CPLEX calling problem: binvarow().\n");
      exit( -1 );
   }

   for( col = 1; col <= lp->cols; col++ ) {
      fprintf(  gStdErrFile, "  %6.2lf", tableaux_col[col-1]  );
   }  /* for col */
   
   if( LP_binvrow( lp, row, tableaux_row )) {
      fprintf(  gStdErrFile, "CPLEX calling problem: binvrow().\n");
      exit( -1 );
   }

   for( col = 1; col <= lp->rows; col++ ) {
      fprintf(  gStdErrFile, "  %6.2lf", tableaux_row[col-1]  );
   }  /* for col */
   
   return( 1 );
}  /* showTableauRow */

      
/**********************************************************************/
int 
showTableaux(LP lp ) 
{
   int i, row, col;
   double coef;

   fprintf(  gStdErrFile, "Row |     z ");
   for( i = 0; i < (lp->cols-1); i++ )
      fprintf(  gStdErrFile, "     x%d", i );
   fprintf( gStdErrFile, "      y" );
   for( i = 0; i < lp->rows; i++ )
      fprintf(  gStdErrFile, "      s%d", i );
   fprintf( gStdErrFile, "    b  |   b.v.\n");

   fprintf(  gStdErrFile, "============");
   for( i = 0; i < lp->cols; i++ )
      fprintf(  gStdErrFile, "========%i", i );
   for( i = 0; i < lp->rows; i++ )
      fprintf(  gStdErrFile, "========%i", i );
   fprintf( gStdErrFile, "=============\n");

   showObjectiveRow( lp );

   getTableauxRHS( lp, tableaux_rhs );

   /* get list of basic variables */
   if( LP_getgrad( lp, 0, tableaux_bv, dummy_double )) {
      fprintf(  gStdErrFile, "CPLEX calling problem: getgrad().\n");
      exit( -1 );
   }
   
   for( row = 0; row < lp->rows; row++ ) {

      showTableauRow( lp, row );
      fprintf( gStdErrFile, "    %.2lf",tableaux_rhs[row] );
      fprintf(  gStdErrFile, " |"  );

      if( tableaux_bv[row] >= 0 ) {
         if( tableaux_bv[row] == gNumVariables )
            fprintf(  gStdErrFile, "  y" );
         else
            fprintf( gStdErrFile, "  x%d", tableaux_bv[row] );
      }
      else
         fprintf( gStdErrFile, "  s%d", (tableaux_bv[row] + 1 ) * -1 );    
 
      fprintf(  gStdErrFile, "\n");
      
   }  /* for row */;
  
   fprintf(  gStdErrFile, "\n");

   return( 1 );
}  /* showTableaux */
/**********************************************************************/


   
   
 /**********************************************************************/
int 
getVertex( double *b ) 
{
  /*
    Repeated calls to this routine will give the entire set of vertices.
    This assumes that a call has been made to initVertexEnum() so that
    the LP and data structures are properly set up.  The routine 
    returns 1 if a vertex is enumerated, 0 if all vertices have been 
    enumerated, and -1 if there is not a valid LP prepared yet.  Upon
    success the vertex will be returned in 'b', which assumes the caller
    has allocated.  It also returns -1 if 'b' is NULL.
  */
  LP lp;
  int vertex_found = 0;

  lp = gVertexEnumLp;

   if( !gVertexInit || b == NULL )
      return( -1 );

   while( !vertex_found ) {

      /* We bump up gCurCol to contain the next non-basic slack in the
         current tableau.  If this was the last one, then we move onto
         the next record in the record list, if any, and set the
         tableau to correspond to what its slack basis says.  If the
         list is emtpy then it returns 0.  Note that it depends on the fact
         that before any vertices are enumerated gCurCol will contain a
         really big value (i.e., BIG_COL_NUM).
         */
      
      gCurCol++;  /* This might not be a non-basic variable, or
                     even a valid column, but the following stuff
                     will take care of that.  We need to at least
                     move off of the last non-basic slack considered
                     which is what gCurCol will always contain
                     at the top of this loop.
                     */
      
      /* Find next non-basic slack column for current tableau, if any */
      while( gCurCol < gTotalColumns ) {
         if( gCurSlackBasis[gCurCol - gNumVariables - 1] == 0 )
            break;
         gCurCol++;
      }
      
      /* If we have exhausted all of the non-basic slack for the
         current tableau (or this is the first time this routine's
         loop has executed), then it is time to move onto the next
         record in the list.  This routine does that and sets gCurCol
         to be BIG_COL_NUM.  If there are no more records, it returns
         a 0 indicating that all vertices have been enumerated.  
         */
      if( gCurCol >= gTotalColumns ) {

         if( getNextRecord( lp ) == 0 ) 
            return( 0 );
         
         /* else we will just go to top of loop and start search through 
          this new tableaux (except id special vertex check turns out
          to be true.
          */

         if( gVerbose[V_VERTEX_ENUM] ) {
            fprintf( gStdErrFile,
                     "Getting new tableaux to search:\n");
            //FIXME: ‘showTableaux’ from incompatible pointer type
            // showTableaux( lp->lp );
         }

         /* We need a special case for when the added variable y = 0.
            In this case we are at a vertex, so we should output the
            vertex.
            */
         vertex_found = specialVertexCheck( lp, b );

      }  /* if gCurCol >= gTotalColumns */
      
      else  /* The gCurCol value is one we want to consider a pivot on */

         /* Find element that would need to pivot out of basis for the
            current non-basic slack (gCurCol) to enter basis. This routine
            might change the current tableaux and as a result,  it
            must also restore the tableaux to the state it had before it
            is called.  
            */
         vertex_found = doPivotCheck( lp, gCurCol, b );
      
   } /* while */

   return( 1 );
}  /* getVertex */
/**********************************************************************/

/**********************************************************************/
/*************  Cheng-Mattheiss Interface Routines      ***************/
/**********************************************************************/
/* Routines that will take the internal representation used for POMDP
   algorithms and produce and augmented LP in CPLEX.
*/

/**********************************************************************/
int 
setUpIndices( LP lp, int num_constraints ) 
{
  /* 
     We first need to set up the indices for CPLEX's sparse
     representation.  The actual coefs values are stored in the array
     lp->matval, but CPLEX needs to know which row and column each of
     these entries are in.  
     
     We could probably compact these into a fewer number of loops,
     but for clarity I set one thing at a time.
  */
   int row, col;
   
   
   /* This will set up the indices for the regular constraint coefs. */
   for( row = 0; row < num_constraints; row++ ) 
      for( col = 0; col < (lp->cols-1); col++ ) 
         lp->matind[ col * (num_constraints + 1) + row] = row;
   
   /* This sets up the indices for the added variable coefficients */
   for( row = 0; row < lp->rows; row++ ) 
      lp->matind[(lp->cols-1) * (num_constraints + 1) + row] = row;

   /* This sets up the indices for the added constraints */
   for( col = 0; col < (lp->cols-1); col++ )
      lp->matind[col*(num_constraints + 1) + num_constraints] = col + num_constraints;

}  /* setUpIndices */
/**********************************************************************/
int 
setUpSimplexConstraints( LP lp, int num_constraints ) 
{
   int col;

   /* The simplex constraints will be the first and second rows */
   lp->rhs[0] = 1.0;
   lp->rhs[1] = -1.0;

   /* simplex constraints */
   for( col = 0; col < (lp->cols-1); col++ ) {
      lp->matval[ (num_constraints + 1) * col ] = 1.0;
      lp->matval[ (num_constraints + 1) * col + 1] = -1.0;
   }  /* for col */
   
   /* Now set the added variable coef for these two simplex rows */
   lp->matval[ (num_constraints + 1) 
             * (lp->cols - 1) ] = sqrt( gNumStates );
   lp->matval[ (num_constraints + 1) 
             * (lp->cols - 1) + 1] = sqrt( gNumStates );

}  /* setUpSimplexConstraints */
/**********************************************************************/
int 
setUpRelaxedRegion( LP lp, AlphaList item, AlphaList list ) 
{
  /*
    This routine actually sets up the relaxed region and adds the extra 
    variable and constraints that is needed for Mattheiss algorithm.
    It assumes that item is a pointer to one of the elements in the list
    so that it does not have to consider the difference between 'item'
    and all vectors in the list (we can skip itself).

    It assumes that all memory has been allocated.
  */
   int row, col, index, k;
   int num_constraints;
   double squared_sum;

   /* set num_constraints, num_vectors + 1.  There will be num_vectors - 1
    actual region constraints (we don't need to compare it to itself). Then
    there will be the two simplex constraints which together will actually
    combine to form and equality.  I don't just give it an equality, because
    I am uncertain about what Mattheiss will do with constraints that are
    not <=.  When I coded Mattheiss, I assumed there would be a slack 
    variable for each row.
    */
   num_constraints = list->length + 1;

   /* Where each column begins is a simple formula for all columns */
   for( col = 0; col < lp->cols; col++ ) 
      lp->matbeg[col] = col * (num_constraints + 1 );

   /* There will be the same number of coefs in each column except the
      last one.  The added variable is defined for all the rows. 
      */
   for( col = 0; col < lp->cols-1; col++ ) 
      lp->matcnt[col] = num_constraints + 1;
   lp->matcnt[lp->cols-1] = lp->rows;

   setUpIndices( lp, num_constraints );

   /* most every constraint has the same sense and rhs */
   for( row = 0; row < lp->rows; row++ ) {
      lp->sense[row] = 'L';
      lp->rhs[row] = 0.0;
   }  /* for row */

   setUpSimplexConstraints( lp, num_constraints );

   /* simplex constraints take up the first two rows, so we start at 1 */
   row = 1;
   
   /* region constraints */
   list = list->head;
   while( list != NULL ) {

      /* We don't want to construct a constraint when item == list,
         since the vector subtracted from itself will just be zero.
         */
      if( item != list ) {
         row++;
         squared_sum = 0.0;  /* needed to calculate the added variable coef */
         
         for( col = 0; col < gNumStates; col++ ) {
            
            index = col * (num_constraints+1) + row;

            lp->matval[ index ] 
              = list->alpha[col] - item->alpha[col];

            squared_sum += lp->matval[ index ] * lp->matval[ index ];
            
         }  /* for col */
         
         lp->matval[gNumStates * (num_constraints + 1) + row] 
           = sqrt( squared_sum );
      }  /* if item != list */

      list = list->next;
   } /* while */

   /* Finally we need to set the coefs for the added constraints */
   for( col = 0, k = (num_constraints+1)*lp->cols -1;
       col < gNumStates;
       col++, k++ ) {

      /* Set the variable 'col' in the appropriate added constraint row */
      lp->matval[num_constraints + col * (num_constraints + 1)] = -1.0;

      /* Set the added variable coef in that row as well */
      lp->matval[k] = 1.0;

   }  /* for col and k */

}  /* setUpRelaxedRegion */
/**********************************************************************/
int 
startVertexEnum( AlphaList item, AlphaList list ) 
{
  /*
    This routine assumes that all the column related memory stuff has
    already been allocated (i.e., a call to initVertexEnum()).  It will
    allocate the row specific memory, set up the relaxed region in 
    question (based upon item and list) and then start the Mattheiss
    algorithms so that calls to getVertex() can be made.  When all
    vertcies have been enumerated, then a call to endVertexEnum() should
    happen.
  */
  LP lp;
  int status;
  
  lp = gVertexEnumLp;
  
   allocateVertexEnumLP( lp, gNumStates, list->length + 1 );

   /* Set variables to correspond to problem parameters */
   gNumVariables = lp->cols - 1;
   gNumConstraints = lp->rows;
   gTotalColumns = gNumVariables + 1 + gNumConstraints;
   gBasicSlackBytes = gNumConstraints / (8 * sizeof( unsigned int)) + 1;

   allocateTempMemory( lp );  /* Must be done after setting above variables */
      
   setUpRelaxedRegion( lp, item, list );

   LP_loadLP( lp );
 
   if ( lp->lp == NULL )  {
      fprintf( gStdErrFile, 
               "** ERR ** startVertexEnum: loadlp() problem.\n");
      exit( -1 );
   }

   /* First get the optimal solution */
   LP_setlpcallbackfunc( NULL );  /* Make sure we don't stop optimization,
                                     before it gets to optimal */

   LP_optimizeLP( lp );
   status = checkLPStatus( lp );

   /* We only want to add a record to the list if a solution was
      found.  For infeasible LPs, leaving this list empty will
      make the algorithm terminate right away.
      */
   if( status == CPX_OPTIMAL ) {

      if( gVerbose[V_VERTEX_ENUM] ) {
         fprintf( gStdErrFile, "Initial optimal tableaux:\n");
         // FIXME: pointer type
         //showTableaux( lp->lp );
      }

      updateRecordList( lp );  /* This gets the first record into the
                                  list, which primes the algorithm */
   }

   /* set the callback routine to return 1 to stop optimization.  For the
    rest of the algorithm, we never want the optimization process to
    advance beyond what the current basis that we set is. 
    */
   LP_setlpcallbackfunc( cplexCallback );

   gCurCol = BIG_COL_NUM;  /* Indicate that there is no current column */
   gCurRecord = NULL;      /* Indicates that there is no current record */

   gVertexInit = 1;

}  /* startVertexEnum */
/**********************************************************************/
void 
endVertexEnum( ) 
{
  /*
    Undoes what startVertexEnum() did.
  */
  LP lp;

  lp = gVertexEnumLp;

  /* Set variables to bogus values */
  gNumVariables = 0;
  gNumConstraints = 0;
  gTotalColumns = 0;
  gBasicSlackBytes = 0;
   
  freeTempMemory();
  
  LP_unloadLP( lp );
  freeVertexEnumLP( lp );
  
  destroyRecordList( gRecordList );
  gRecordList = NULL;
  
  gVertexInit = 0;

}  /* endVertexEnum */
/**********************************************************************/

/**********************************************************************/
/******************       Debugging Routines        *******************/
/**********************************************************************/

/**********************************************************************/
void 
displayVertexEnumLPSolution( LP lp ) 
{
  LP_writeSolution( lp, "temp-sol" );
}  /* displayVertexEnumLPSolution */
/**********************************************************************/
void 
displayVertexEnumLP( LP lp ) 
{
   LP_writeLP( lp, "temp" );
}  /* displayVertexEnumLP */
/**********************************************************************/
int 
showVertex( double *b ) 
{
   int i;

   for( i = 0; i < gNumVariables; i++ )
      fprintf( gStdErrFile, "%.*lf ", 
               VERTEX_ENUM_DISPLAY_PRECISION, b[i] );

   fprintf( gStdErrFile, "\n" );

   return( 1 );
}  /* showVertex */
