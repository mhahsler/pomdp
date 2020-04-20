
/*
 *<SOURCE_HEADER>
 *
 *  <NAME>
 *    two-pass.c
 *  </NAME>
 *  <AUTHOR>
 *    Anthony R. Cassandra
 *  </AUTHOR>
 *  <CREATE_DATE>
 *    July, 1998
 *  </CREATE_DATE>
 *
 *  <RCS_KEYWORD>
 *    $RCSfile: two-pass.c,v $
 *    $Source: /u/cvs/proj/pomdp-solve/src/two-pass.c,v $
 *    $Revision: 1.2 $
 *    $Date: 2004/09/03 19:14:42 $
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
 *   This file contains code that is specific to the two-pass algorithm
 *   for solving partially observable Markov decision processes.  This
 *   algorithm was first presented in Edward Jay Sondik's Thesis,
 *   Stanford 1971.
 * 
 */


#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "mdp/mdp.h"

#include "global.h"
#include "pomdp.h"
#include "alpha.h"
#include "params.h"
#include "common.h"
#include "neighbor.h"
#include "lp-interface.h"
#include "parsimonious.h"
#include "two-pass.h"
 
/**********************************************************************/
void 
initTwoPass( ) 
{

}  /* initTwoPass */
/**********************************************************************/
void 
cleanUpTwoPass( ) 
{

}  /* cleanUpTwoPass */
/**********************************************************************/
void 
setUpTwoPassLpObjective( LP lp, AlphaList neighbor ) 
{
  int i;
  Assert( lp != NULL && neighbor != NULL
          && neighbor->prev_source != NULL, 
          "Parameters and/or fields are NULL." );

  /* It really shouldn't matter whether we are maximizing or
     minimizing the objection function since we are mainly looking to
     see if it is feasible. */
  lp->objsen = MAXIMIZE;

  for ( i = 0; i < gNumStates; i++ ) {
    
    /* The objective is just to find if the neighbor is at least as
       good as the vector itself in the vector's region. */
    lp->obj[i] = neighbor->alpha[i] - neighbor->prev_source->alpha[i];
    
    /* For all the belief state variables in the LP, we know that must
       be >=0 and <=1, so we set their bounds here. */
    lp->lowbnd[i] = 0.0;
    lp->upbnd[i] = 1.0;

  } /* for i */

}  /* setUpTwoPassLpObjective */
/**********************************************************************/
int 
numTwoPassLpNonZeroes( AlphaList node, 
		       AlphaList *projection,
		       double sparse_epsilon ) 
{
  /*
    Counts the number of non-zero constraint coefficients that will be
    in the tow pass LP.  
  */  
  AlphaList proj_node;
  int col, z;
  int non_zero_count;
  
  /* All the row 0 (simplex constraint) coefficients are non-zero. */
  non_zero_count = gNumStates;

  /* Essentially the same looping structure that exists in the
     setUpTwoPassLpConstraints() routine. */
  for ( col = 0; col < gNumStates; col++ ) {
    
    for ( z = 0; z < gNumObservations; z++ ) {
      
      proj_node = projection[z]->head;
      while ( proj_node != NULL ) {
        
        /* If this node in the list is the same as the one that was
           used to constrauct the vector, then we just skip it. */
        if ( proj_node == node->obs_source[z] ) {
          proj_node = proj_node->next;
          continue;
        } /* if same projection[z] vector */

        if ( ! Equal( proj_node->alpha[col],  
                      node->obs_source[z]->alpha[col], 
                      sparse_epsilon ))
          non_zero_count++;
        
        proj_node = proj_node->next;
      } /* while proj_node != NULL */

    } /* for z */
    
  } /* for col */

  return( non_zero_count );
}  /* numTwoPassLpNonZeroes */
/**********************************************************************/
void 
setUpTwoPassLpConstraints( LP lp, AlphaList node, 
			   AlphaList *projection ) 
{
  int row, col, z, index;
  AlphaList proj_node;
  
  /* Iterate over the constraint coefficient matrix column by
     column. 'index' will be the current index into lp->matval and we
     will increment it everytime we set it. This makes sure things are
     set up for properly regardless of whether the LP is being set up
     sparse or not. */
  index = 0;
 
  for ( col = 0; col < gNumStates; col++ ) {

    /* Need to set the bookkeeping array for where this column
       starts. */
    lp->matbeg[col] = index;

    /* Since the first constraint is the simplex constraint, the first
       thing we need to do for this belief state variable (each column
       is a belief state variable) is set the coefficient to '1'. */
    lp->matval[index] = 1.0;
    lp->matind[index++] = 0;

    /* We have a set of constraints for each observation, so loop over
       the observations to iterate over these sets. */
    row = 1;
    for ( z = 0; z < gNumObservations; z++ ) {
      
      /* Looping over this list is now looping over the rows, but
         starting at row = 1, since The simplex constraint was row
         '0'. */ 
      proj_node = projection[z]->head;
      while ( proj_node != NULL ) {
        
        /* If this node in the list is the same as the one that was
           used to constrauct the vector, then we just skip it. */
        if ( proj_node == node->obs_source[z] ) {
          proj_node = proj_node->next;
          continue;
        } /* if same projection[z] vector */
        
#ifndef USE_DENSE_LPS
        if ( ! Equal( proj_node->alpha[col],  
                      node->obs_source[z]->alpha[col], 
                      lp->sparse_epsilon ))
#endif
          {
            lp->matval[index]
              = ( proj_node->alpha[col]
                  - node->obs_source[z]->alpha[col] );
            lp->matind[index++] = row;
          }
        
        proj_node = proj_node->next;
        row++;
      } /* while proj_node != NULL */

    } /* for z */
    
    /* We can compute the number of entries we entered by looking at
       the current index position and where this column started. */
    lp->matcnt[col] = index - lp->matbeg[col];
  } /* for col */

  /* After all is said and done, we should have entered the same
     number of entries as we computed we would need when we allocated
     the LP. */
  Assert( index == lp->matspace, 
          "Computed non-zeroes didn't match actual non-zeroes." );

  /***********************************************/
  /* Constraint RHS and sense.                   */
  /***********************************************/

  /* Simplex constraint. */
  lp->sense[0] = 'E';
  lp->rhs[0] = 1.0;
  
  /* Remainder of constraints. */
  for ( row = 1; row < lp->rows; row++ ) {
    lp->sense[row] = 'L';
    lp->rhs[row] = 0.0;
  } /* for row */

}  /* setUpTwoPassLpConstraints */
/**********************************************************************/
LP 
setUpTwoPassLp( AlphaList neighbor, 
		AlphaList *projection,
		double sparse_epsilon )
{
  /*
    The two-pass LP is constructed by taking the intersection of the a
    bunch of regions.  In particular, the intersection of a region
    defined on each projection[z] set.  The idea is that we want to
    determine if one of the neighbors of a vector has a region which is
    adjacent to the vector itself.  Thus, for the vector in questioon,
    we define its region by looking at each vector from the
    projection[z] sets and defining the region over this projection
    set.  The intersection of these region is exactly the region of the
    vector itself over the Q^a set being constructed. We then ask
    whether or not there is a point in this region where the neighbor is
    as good as the vector itself.  If the neighor's region is adjacent
    to the vector, then it should yield the same value at the border of
    the region.  Note that the neighbor node has the pointer pre_source
    which points to the vector that defines the region.
  */
  int num_constraints, num_variables, num_non_zeroes, z;
  LP lp;

  /* Just a variable for each belief state component. */
  num_variables = gNumStates;
  
  /* The number of constraints will be the sum of the number of
     constraints on the individual projection[z] sets, which will be
     one less than the total number of vectors in the projection set.
     Also, there is the extra simplex constraint. */
  num_constraints = 1;
  for ( z = 0; z < gNumObservations; z++ )
    num_constraints += projection[z]->length - 1;

#ifdef USE_DENSE_LPS
  /* Assuming all are non-zeroes, every variable has an entry for
     every constraint except the delta variable in the simplex
     constraint. */
  num_non_zeroes = num_constraints * num_variables;
#else
  /* If we are using sparse LPs, we need to count the non-zero
     entries.  This is actually a combination of counting and
     calculating based upon knowledge of the problem. 
     */
  num_non_zeroes = numTwoPassLpNonZeroes( neighbor->prev_source,
                                          projection,
                                          sparse_epsilon );
#endif

  lp = LP_newLP( num_constraints, num_variables, num_non_zeroes );
  lp->sparse_epsilon = sparse_epsilon;

  setUpTwoPassLpObjective( lp, neighbor );
  
  setUpTwoPassLpConstraints( lp, neighbor->prev_source, 
                             projection  );

  return( lp );
}  /* setUpTwoPassLp */
/**********************************************************************/
int 
feasibleTwoPassLp( AlphaList neighbor, 
		   AlphaList *projection,
		   PomdpSolveParams param ) 
{
  /*
    Determines whether or not this neighbor has an adjacent feasible
    region to the vector that lead to the production of this neighbor.
  */
  LP lp;
  
  lp = setUpTwoPassLp( neighbor, projection, param->sparse_epsilon );
  
  /* See if we get a feasible solution to the LP, but if not just
     return FALSE. */
  switch ( LP_solveLP( lp, param->stat )) {
  case LP_OPTIMAL:
    LP_freeLP( lp );
    return ( TRUE );

  case LP_INFEASIBLE:
    /* Code below handles this case. */
    break;

  case LP_UNBOUNDED:
    Warning( "LP_solveLP() return status is LP_UNBOUNDED." );
    break;

  default:
    Warning( "Unknown LP_solveLP() return status." );
    break;
  }

  LP_freeLP( lp );
  return ( FALSE );

}  /* feasibleTwoPassLp */
/**********************************************************************/
void 
searchRegionTwoPass( AlphaList cur_vector, 
				 AlphaList list,
				 AlphaList *projection,
				 PomdpSolveParams param ) 
{
  /* 
     This is the main part of the Sondik two pass approach.  It looks at
     all the neighbors of a vector to see if this neighbor forms a
     border with this vector's region.  If it does, then it adds it to
     the list.  
  */
   AlphaList neighbor_list, neighbor, node;

   /* Initialize this to false, that way if the dominance checking
      is not selected, we execute the witness LPs. */
   //int dominated = FALSE;

   /* We will search over all observations and previous vectors, which
      amounts to a search over neighbors. Just for reference a
      "neighbor" is a vector that differs from another vector in a
      single "obs_source[]" setting. */
   neighbor_list = newAlphaList();
   addAllNeighbors( neighbor_list, cur_vector, projection,
                    param->domination_check, 
                    param->alpha_epsilon );

   neighbor = neighbor_list->head;
   while( neighbor != NULL ) {

     /* Before bringing out the LP machinery, we can do some simple
        checks to see if this neighbor is going to be useful. This at
        least checks whether the vector is already in the list, and
        can also check for simple component-wise domination if the
        param->domination_check is TRUE. */ 
     if ( isEmptyRegionSimpleCheck( list, 
                                    neighbor->alpha,
                                    param->alpha_epsilon,
                                    param->domination_check)){
       neighbor = neighbor->next;
       continue;
     }
     
     /* Now try to LP stuff to see if the neighbor is useful. */
     if ( feasibleTwoPassLp( neighbor, 
                             projection,
                             param ) == FALSE ){
       neighbor = neighbor->next;
       continue;
     }
     
     /* If the neighbor is useful, then we will add it to the list,
        but make sure we remove it from the neighbor list or else when
        we destroy the neighbor list we'll be destroying this node too
        (a bad thing). Also note that we better update the next
        pointer now or else we will be in big trouble. */
     node = neighbor;
     neighbor = neighbor->next;
     extractAlphaNode( neighbor_list, node );

     /* We will need to set its obs_source array though.  As a
        neighbor it does not have even the space for the obs_source
        array allocated.  The obs_source array here is essentially the
        same as the vector used to construct this as a neighbor
        (prev_source).  The place where the obs_source array differs
        is in 'obs' and the pointer to the vector for obs_source[obs]
        was stored in the 'first_source' field. We also need to set
        the action. */
     node->obs_source 
       = duplicateObsSourceArray( node->prev_source->obs_source );
     
     node->obs_source[node->obs] = node->first_source;

     /* Neighbors don't have their action field set so make sure we
        copy the one from the source of this neighbor. */
     node->action = node->prev_source->action;

     /* Now we finally add it to the list.  Put it at the front,
        because we want to check it first. */
     prependNodeToAlphaList( list, node );

	if ( shouldTerminateEarly( list, param ))
	 break;

   } /* while neighbor != NULL */
   
   destroyAlphaList( neighbor_list );
   
} /* searchRegionTwoPass */
/**********************************************************************/
AlphaList 
improveTwoPass( AlphaList *projection, 
		PomdpSolveParams param ) 
{
  /*
    The main two-pass algorithm routine for finding the Q-function
    represention for value iteration with POMDPs.  
  */
  AlphaList new_alpha_list;
  AlphaList cur_vector;

  Assert ( projection != NULL, "Projection sets are NULL." );
  
  new_alpha_list = newAlphaList();

  /* The original sondik two-pass algorithm uses a single point to
     initialize things.  Initializes the set using the simplex
     corners and optionally other points.  If the set returned has
     only one vector then we know it is the entire set because the
      simplex corner check is guaranteed to return more than one
      vector if the parsimonious set contains more than 1 vector. */
  if ( initListSimpleQ( new_alpha_list, projection, param ) < 2 )
    return ( new_alpha_list );

  if ( gVerbose[V_TWO_PASS] )
    fprintf( param->report_file, "\t  Two Pass init: %d vectors.\n",
             new_alpha_list->length );

  /* Loop until all vectors are marked. */
  while ( (cur_vector = findUnmarkedVector( new_alpha_list ))
          != NULL ) {

    /* This will add vectors as unmarked into the list. */
     searchRegionTwoPass( cur_vector, new_alpha_list, 
                          projection, param );

     /* Mark this vector as being completed and get a new unmarked
        vector. */
     cur_vector->mark = MARKED;

	if ( shouldTerminateEarly( new_alpha_list, param ))
	  break;

   }  /* while an unmarked vector exists */

  return( new_alpha_list );

}  /* improveTwoPass */
/**********************************************************************/
