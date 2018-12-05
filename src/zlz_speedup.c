
/*
 *<SOURCE_HEADER>
 *
 *  <NAME>
 *    zlz_speedup.c
 *  </NAME>
 *  <AUTHOR>
 *    Anthony R. Cassandra
 *  </AUTHOR>
 *  <CREATE_DATE>
 *    
 *  </CREATE_DATE>
 *
 *  <RCS_KEYWORD>
 *    $RCSfile: zlz_speedup.c,v $
 *    $Source: /u/cvs/proj/pomdp-solve/src/zlz_speedup.c,v $
 *    $Revision: 1.2 $
 *    $Date: 2004/04/18 23:54:04 $
 *  </RCS_KEYWORD>
 *
 *  <COPYRIGHT>
 *
 *    1999-2003, Anthony R. Cassandra
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
 *   Value iteration speedup proposed by Zhang, Lee and Zhang in a paper
 *   to be submitted to UAI-99. It does some updates of the value
 *   function at a set of points, where these points come from the full
 *   DP update; they are the witness points for each vector.
 * 
 */

#include <stdio.h>
#include <math.h>

#include "mdp/mdp.h"

#include "global.h"
#include "timing.h"
#include "alpha.h"
#include "projection.h"
#include "common.h"
#include "stats.h"
#include "params.h"
#include "region.h"
#include "parsimonious.h"
#include "zlz_speedup.h"

/**********************************************************************/
void 
ZLZ_startStats( SolutionStats stat, AlphaList U_n ) 
{
  /*
    This is called just before the ZLZ_improve() call to disply starting
    information and initialize anything that needs it. 
  */
  double stop_time_user, stop_time_system;

  /* First thing is to get the time to mark the end of the
     epoch's contribution. */
  getSecsDetail( &stop_time_user, &stop_time_system );
  
  /* We want to show the initial size so we can compare it to the
     final size output from here. */
  fprintf( stat->report_file,
           "%d DP vectors in %.2lf secs.\n  ZLZ: .", 
           sizeAlphaList( U_n ),
           stop_time_user - stat->epoch_start_time_user
           + stop_time_system - stat->epoch_start_time_system );
  fflush( stat->report_file );
  
}  /* ZLZ_startStats */
/**********************************************************************/
void 
ZLZ_endStats( PomdpSolveParams param, int solution_size ) 
{
  /*
    This is called after the ZLZ_improve() routine to report things that
    happened and to clean up anyy stats that need it.
  */

  if ( param->stat == NULL )
    return;

}  /* ZLZ_endStats */
/**********************************************************************/
void 
ZLZ_dominationCheckSpecial( AlphaList U, AlphaList U_next ) 
{
  /*
    Removes all vectors from U which are dominated by vectors from
    U_next. 
  */
  AlphaList node;

  /* We will first set the 'mark' field of the list and then delete
     the 'mark'ed nodes.  So first we need to clear the 'mark' field,
     just in case they are set. */
  clearMarkAlphaList( U );

  for ( node = U_next->head; node != NULL; node = node->next )
    markDominatedAlphaList( node->alpha, U );

  removeMarkedAlphaList( U );

}  /* ZLZ_dominationCheckSpecial */
/**********************************************************************/
double *
ZLZ_backup( double *b, int a, AlphaList **projection ) 
{
  /*
    Mostly just a mimicking of the bestAlphaForBeliefQ() routine, but
    without using a node explicitly.
  */

   AlphaList best_proj_vector;
   int i, z;
   double best_value;
   double *alpha;

   alpha = newAlpha();
   
   /* Initialize the alpha vector to all zeroes. */
   for ( i = 0; i < gNumStates; i++ ) 
     alpha[i] = 0.0;
   
   /* Now pick out the best vector for the projection set for each
      observation.  The best overall vector is just the sum of these
      individual best vectors. */
   for ( z = 0; z < gNumObservations; z++ ) {

     /* Find the best vector for all the observation 'z' projections.
        If projection[z] is NULL, then this returns NULL. */
     best_proj_vector = bestVector( projection[a][z], b, 
                                    &best_value, SMALLEST_PRECISION  );

     /* By defnition, if projection[z] is NULL, then this means
        that the observation is not possible for this action. In
        this case, that observation will not contribute anything to
        the value of the state, so we can safely skip it.  Since it
        is impossible for all observations not to occur, we don't
        have to worry about all the projection[z] being NULL. */
     if ( best_proj_vector == NULL ) {
       continue;
     } /* if observation not possible */
     
     /* Now add this best projection vectors component values to the
        components in the node. */
     for ( i = 0; i < gNumStates; i++ ) 
       alpha[i] += best_proj_vector->alpha[i];
     
     /* Note that the immediate rewards have already been taken into 
        account for the projection vectors. */
     
   }  /* for z */

   return ( alpha );

}  /* *ZLZ_backup */
/**********************************************************************/
int 
ZLZ_pruneSpecial( AlphaList list, PomdpSolveParams param ) 
{
  /* 
     Only considers pruning vectors that have their 'marked' field set
     to FALSE. 
  */
  AlphaList cur_node;
  int num_pruned = 0;
  
  Assert( list != NULL, "List is NULL." );

  while ( sizeUnmarkedAlphaList( list ) > 0 ) {

    /* Remove a node from the original list. */
    cur_node = extractUnmarkedVector( list );

    /* If this node gives us a witness point, then we will leave it in
       the list. */
    if ( findRegionPoint( cur_node->alpha, list, 
                          gTempBelief, NULL, param )) {

      /* Mark it and add it back to list. */
      cur_node->mark = TRUE;
      appendNodeToAlphaList( list, cur_node );

      /* Save the witness point too! */
      addWitnessToAlphaNode( cur_node, gTempBelief );

    } /* If we did find a witness point. */

    /* Otherwise, no witness point was found which mean we can simply
       get rid of this node. */
    else {
      destroyAlphaNode( cur_node );
      num_pruned++;
    } /* else no witness point was found. */

  } /* while list->length > 0 */

  return( num_pruned );
}  /* ZLZ_pruneSpecial */
/**********************************************************************/
int 
ZLZ_stop( AlphaList U_next, AlphaList U_prev, double delta ) 
{
  AlphaList alpha_next, dummy;
  double epsilon1 = DEFAULT_EPSILON1_VALUE;
  double max_diff = -1.0 * HUGE_VAL;
  double diff;

  Assert( U_next != NULL && U_prev != NULL,
          "Bad parameters." );

  /* Only need to consider the anchoring (witness) points in U_next
     because of the manner in which it is constructed. */
  for ( alpha_next = U_next->head;
        alpha_next != NULL;
        alpha_next = alpha_next->next ) {

    diff = bestVectorValue( U_next, alpha_next->witness,
                            &dummy, SMALLEST_PRECISION )
      - bestVectorValue( U_prev, alpha_next->witness,
                            &dummy, SMALLEST_PRECISION );

    max_diff = Max( max_diff, diff );

  } /* for alpha_next  */
  
  return ( max_diff <= Min( epsilon1, epsilon1 * delta ) );

}  /* ZLZ_stop */
/**********************************************************************/
void 
ZLZ_improve( AlphaList U_orig, double delta, 
	     int recursion_level, PomdpSolveParams param ) 
{
  /*
    The improve routine which uses witness points to improve the value
    function. 
  */
  AlphaList alpha_node, new_node, U_prev, U_next;
  double *alpha_prime, alpha_prime_value, alpha_value;
  int done = FALSE;
  int i, k = 0;
  int pre_prune_size, num_pruned;
  AlphaList **projection;

  /* Set these up so loop's manner of copy-switching these two
	works. */  
  U_prev = newAlphaList();
  U_next = duplicateAlphaListWithWitnesses( U_orig );

  while ( ! done ) {
    k++;

    destroyAlphaList( U_prev );
    U_prev = U_next;
    U_next = newAlphaList();

    projection = makeAllProjections( U_prev );

    /* This loop over the original list is really just a loop over the
       witness points. */
    for ( alpha_node = U_prev->head; 
          alpha_node != NULL; 
          alpha_node = alpha_node->next ) {

      alpha_prime = ZLZ_backup( alpha_node->witness,
                                alpha_node->action,
                                projection );

      /* Compute value of both vectors at this point. */
      alpha_prime_value = alpha_value = 0.0;
      for ( i = 0; i < gNumStates; i++ ) {
        alpha_prime_value += alpha_prime[i] * alpha_node->witness[i];
        alpha_value += alpha_node->alpha[i] * alpha_node->witness[i];
      } /* for i */

      if ( alpha_value > alpha_prime_value )
        for ( i = 0; i < gNumStates; i++ )
          alpha_prime[i] = alpha_node->alpha[i];

      new_node 
        = appendAlphaList( U_next, alpha_prime, alpha_node->action );
      
      addWitnessToAlphaNode( new_node, alpha_node->witness );

    } /* for alpha_node */

    freeAllProjections( projection );

    done = ZLZ_stop( U_next, U_prev, delta );

    /* Increment the update count because it has just updated the
       values again. */
    (param->update_count)++;

    if ( (k % 20) == 0 ) {
      fprintf( param->report_file, "." );
      fflush( param->report_file );
    } /* if we are not being succinct */
    
  } /* while not done */

  dominationCheck( U_next );

  /* This special domination check prunes vectors from U_orig that are
     dominated by vectors in U_next */
  ZLZ_dominationCheckSpecial( U_orig, U_next );
  
  /* The special prune routine only considers pruning vectors from
     U_orig, so we will mark the vectors in U_next, merge the two
     lists and then execute a special prune routine that only tries to
     prune unmarked vectors. */
  markAllAlphaList( U_next );
  pre_prune_size = sizeAlphaList( U_orig );

  unionTwoAlphaLists( U_orig, U_next );
  
  num_pruned = ZLZ_pruneSpecial( U_orig, param );

  if (( pre_prune_size - num_pruned ) > 0 )
    ZLZ_improve( U_orig, delta, recursion_level+1, param );

}  /* ZLZ_improve */
/**********************************************************************/
void 
ZLZ_ViSpeedup( AlphaList U_n, double delta,
	       PomdpSolveParams param ) 
{
  /*
    This is the routine that implements the portion of the VI algorithm
    *after* the regular DP update.  The U_n set is the last computed
    value funciton (output of the DP update).  The 'delta' parameter is
    the current Bellman residual of the new value function and the
    previous value funciton as computed by the bellmanError() routine.
  */
  AlphaList V_n;

  /* Note that this should be checked prior to calling this routine by
     the meetStoppingCriteria() routine, but we put the check in here
     just for the heck of it. */
  if ( delta <= ( param->stop_delta 
                  * ( 1.0 - gDiscount ) / ( 2.0 * gDiscount )))
    return;

  ZLZ_startStats( param->stat, U_n );
  startContext( param->stat, Context_Zlz_Speedup );
  
  /* Does the improvement and will leave the final set in U_n (which
     is where the main VI loop is expecting it to be. */
  ZLZ_improve( U_n, delta, 0, param );

  /* Must do this outside the ZLZ_improve() routine, becuase that can
     result in a recursive call and we do not allow nesting of contexts
     at this time. */
  endContext( param->stat, Context_Zlz_Speedup );
  ZLZ_endStats( param, sizeAlphaList( U_n ) );

}  /* zlzSpeedup */
/**********************************************************************/
