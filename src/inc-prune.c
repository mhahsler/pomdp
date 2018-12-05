
/*
 *<SOURCE_HEADER>
 *
 *  <NAME>
 *    inc-prune.c
 *  </NAME>
 *  <AUTHOR>
 *    Anthony R. Cassandra
 *  </AUTHOR>
 *  <CREATE_DATE>
 *    July, 1998
 *  </CREATE_DATE>
 *
 *  <RCS_KEYWORD>
 *    $RCSfile: inc-prune.c,v $
 *    $Source: /u/cvs/proj/pomdp-solve/src/inc-prune.c,v $
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

#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "mdp/mdp.h"

#include "global.h"
#include "pomdp.h"
#include "alpha.h"
#include "params.h"
#include "common.h"
#include "cross-sum.h"
#include "parsimonious.h"
#include "inc-prune.h"

/**********************************************************************/
void 
initIncPrune( ) 
{

}  /* initCrossSum */
/**********************************************************************/
void 
cleanUpIncPrune(  ) 
{

}  /* cleanUpCrossSum */
/**********************************************************************/
void 
clearAlphaListCounter( AlphaList list ) 
{
  /*
    There are circumstances where we want to keep a counter for the
    nodes in an AlphaList.  Since the 'length' field is only used for
    headers, we will use this field in the nodes of the list as a
    counter.  This routine zero's out the counters for the nodes in a
    list.
  */
  Assert( list != NULL, "List is NULL." );

  list = list->head;
  while ( list != NULL ) {

    COUNT(list) = 0;

    list = list->next;
  } /* while */

}  /* clearAlphaListCounter */
/**********************************************************************/
void 
initializeCountersIp( AlphaList list ) 
{
  /* 
     Increments the count for the first and second source nodes of the
     nodes in this list.
  */
  Assert( list != NULL, "List is NULL." );

  list = list->head;
  while ( list != NULL ) {

    if ( list->first_source != NULL )
      COUNT(list->first_source)++;

    if ( list->second_source != NULL )
      COUNT(list->second_source)++;

    list = list->next;
  } /* while */

}  /* initializeCountersIp */
/**********************************************************************/
void 
addSimpleSumIp( AlphaList new_list, 
		AlphaList old_list, 
		AlphaList add_node,
		AlphaList skip_node ) 
{
  /*
    For each vector in the 'old' list, it adds a vector to the 'new'
    list that had the vector in 'add_node' added to it.  It will ignore
    the node that is equal to 'skip_node' when doing this.
  */
  AlphaList new_node;
  int i;

  Assert( new_list != NULL 
          && old_list != NULL 
          && add_node != NULL,
          "Bad (NULL) parameter(s)." );
  
  old_list = old_list->head;
  while( old_list != NULL ) {
    
    if ( old_list != skip_node ) {
      new_node = appendAlphaList( new_list, 
                                  duplicateAlpha( old_list->alpha ),
                                  old_list->action );
      
      for ( i = 0; i < gNumStates; i++ )
        new_node->alpha[i] += add_node->alpha[i];

    } /* if not the node to skip */

    old_list = old_list->next;
  } /* while */

}  /* addSimpleSumIp */
/**********************************************************************/
void 
addExtractedNodesIp( AlphaList new_list, 
		     AlphaList old_list,
		     AlphaList first_source, 
		     AlphaList second_source ) 
{
/*
  For each node in old_list, if the first or second source fields
  match the first and second sources sent in, then adds a copy of this
  node the new_list.  If first or second source is NULL, assume it
  never matches.
*/

  Assert( new_list != NULL && old_list != NULL,
          "Bad (NULL) parameter(s)." );
  
  old_list = old_list->head;
  while( old_list != NULL ) {
    
    if ( ((first_source != NULL) 
          && (old_list->first_source == first_source))
         || ((second_source != NULL) 
             && (old_list->second_source == second_source)))
      appendAlphaList( new_list, 
                       duplicateAlpha( old_list->alpha ),
                       old_list->action );

    old_list = old_list->next;
  } /* while */

}  /* addExtractedNodesIp */
/**********************************************************************/
AlphaList 
getGenIpCompareList( AlphaList node, 
		     AlphaList A, AlphaList B,
		     AlphaList cur_list,
		     GeneralizedIpChoice ip_type ) 
{
  /*
    Based on the parameters, returns a pointer to an AlphaList
    consisting of all the alpha vectors that need to be compared to
    'node' in order to guarantee that we get a witness point if one
    exists.  This is the heart of the generalized incremental pruning
    algorithm. 
  */
  AlphaList compare_list;

  Assert( node != NULL
          && A != NULL && B != NULL
          && cur_list != NULL,
          "Bad (NULL) parameter(s)." );

  /* What set we return depends on the parameter for the IP type to
     use. */
  switch( ip_type ) {

  case NormalIp:
    /* Normal IP just uses the current list of vectors, so just return
       this set as the one to use in the region comparison. */
    SHOULD_DESTROY(cur_list) = FALSE;
    return( cur_list );

  case RestrictedRegionIp:
    /* Restricted region IP always uses the same set to compare to. */ 
    compare_list = newAlphaList();
    SHOULD_DESTROY(compare_list) = TRUE;
    addSimpleSumIp( compare_list, A, node->second_source,
                    node->first_source );
    addSimpleSumIp( compare_list, B, node->first_source,
                    node->second_source );
    break;

  case GeneralizedIp:
    /* This is the complicated case because the list we construct
       depends upon which candidate is smallest and because we need to
       select vectors out of the cur_list depending on the source
       fields of 'node'. Because we may or may not actually create a
       new list, we need some way to indicate whether or not the
       compare_list wil need to be destroyed.  We do this by setting
       the 'mark' field of the header node to */

    /* First determine which set will be the smallest. The simplest
       thing is for us not to have to consruct a set. This happens if
       the cur_list is smaller than the other two candidates. */
    if (( cur_list->length 
          <= (COUNT(node->first_source) + A->length))
        && ( cur_list->length 
          <= (COUNT(node->second_source) + B->length))) {

      SHOULD_DESTROY(cur_list) = FALSE;
      return( cur_list );
    } /* If cur_list should be the comparison list. */

    /* Otherwise, we must construct a list from the remaining two
       candidates. */
    compare_list = newAlphaList();
    SHOULD_DESTROY(compare_list) = TRUE;

    /* Just see which variation is smaller. */
    if ( (COUNT(node->first_source) + A->length )
         < ( COUNT(node->second_source) + B->length)) {
      
      addSimpleSumIp( compare_list, A, node->second_source,
                      node->first_source );
      addExtractedNodesIp( compare_list, cur_list, 
                           node->first_source, NULL );
    }

    else {
      addSimpleSumIp( compare_list, B, node->first_source,
                      node->second_source );
      addExtractedNodesIp( compare_list, cur_list, 
                           NULL, node->second_source );

    }
    
    break;
    
  default:
    Abort( "Unknown incremental pruning choice ." );
  } /* switch */

  /* zzz Do we need to scan this list for vectors that would be equal
     to 'node'? We have made sure the vector itself is not added, but
     the nature of the cross-sum is such that it could construct a
     buch of similar vectors. */
  return ( compare_list );

}  /* getGenIpCompareList */
/**********************************************************************/
AlphaList 
generalizedIpCrossSum( AlphaList A, 
		       AlphaList B, 
		       PomdpSolveParams param ) 
{
  /*
    Implements the special version of the cross-sum operation for the
    incremental pruning algorithm.  This is really a special form of the
    prune() routine which selects the list to compare each node to
    based upon parameter settings and knowledge of the fact that the
    list being pruned was from the cross-sum of two sets.  It is very,
    very similar to the prune() routine.
  */
  AlphaList new_list, search_list, compare_list, 
    cur_node, best_node;

  /* We want to keep track of how many vectors in new_list were
     derived from vectors in A and B.  This initializes the counters
     for the nodes of A and B so we can start counting. */
  clearAlphaListCounter( A );
  clearAlphaListCounter( B );

  /* First do the cross-sum, setting the first_source and
     second_source fields appropriately. */
  search_list = crossSum( A, B, TRUE );

  /* See if we want to do the domination check. */
  if ( param->domination_check )
    dominationCheck( search_list ); 
    
  /* This is both an optimization and a convenience for trying out the
     epsilonPrune() routines.  If the incremental pruning being done
     is the normal type, then all we need to do is a simple prune of
     the list.  The prune routine will decide whether to do the normal
     thing or an epsilon-prune version. */
  if ( param->ip_type == NormalIp ) {

    prune( search_list, purge_prune, param );
    return( search_list );

  } /* If NormalIp variation (optimization) */

  /* We will mark the best node for each simplex vertex and ranodm
     point initialization, so first clear the 'mark' field. */
  clearMarkAlphaList( search_list );
  
  /* First we select vectors using this simple test. This will
     only mark the best vectors. */
  markBestAtSimplexVertices( search_list, 
                             param->use_witness_points,
                             param->alpha_epsilon );

  /* Use random points to initialize the list, but this will only do
     something if param->prune_init_rand_points > 0 */
  markBestAtRandomPoints( search_list, 
                          param->alg_init_rand_points,
                          param->use_witness_points,
                          param->alpha_epsilon );

  /* Now we actually initialize the parsimonious list with those
     vectors found through the simpler checks. */
  new_list = extractMarkedAlphaList( search_list );

  /* Make sure the header of this list has the proper action set. */
  new_list->action= A->action;

  /* For each node in the list, increment the counts on the source
     nodes. */
  initializeCountersIp( new_list );

  while ( search_list->length > 0 ) {

    /* Remove a node from the original list. */
    cur_node = dequeueAlphaNode( search_list );

    /* This is where we determine which list should be used. It will
       set a field in this list to tell us whether we will need to
       destroy the list when we are done with it.  In some cases this
       routine will just return 'new_list' (which we most definitely
       do not want to destroy) and in other cases return a list that
       was temporarily constructed for comparison purpose. */
    compare_list = getGenIpCompareList( cur_node, A, B, 
                                        new_list, param->ip_type );

    /* See if this node gives us a witness point that there must be a
       vector to be added to new list from original list. */
    if ( findRegionPoint( cur_node->alpha, compare_list, 
                          gTempBelief, NULL, param )) {

      /* Note that the finding of a witness point does *not*
         necessarily mean that cur_node is the best vector at this
         point.  Since we only compare cur_node to the new list, we do
         not know whether there are vectors in the original list which
         might be even better still. */

      /* Therefore, we first put this node back into the list and then
         find the vector in the list that is maximal for this
         point. */
      enqueueAlphaNode( search_list, cur_node );
      
      best_node = removebestVectorNode( search_list, gTempBelief,
                                        param->alpha_epsilon );

      /* zzz Depending on whether or not we can guarantee that the
         compare list does not have vectors equal to the node in
         question, the precision issue could mean that this is ading
         an epslion-redundant vector. */
      appendNodeToAlphaList( new_list, best_node );

      /* Increment the count of number of vectors in the list for the
         source nodes. */
      COUNT(best_node->first_source)++;
      COUNT(best_node->second_source)++;

    } /* If we did find a witness point. */

    /* Otherwise, no witness point was found which mean we can simply
       get rid of this node. */
    else      
      destroyAlphaNode( cur_node );

    if ( SHOULD_DESTROY( compare_list ))
      destroyAlphaList( compare_list );

    if ( shouldTerminateEarly( new_list, param ))
	 {
	   destroyAlphaList( search_list );
	   break;
	 }

  } /* while orig_list->length > 0 */

  return( new_list );

}  /* generalizedIpCrossSum */
/**********************************************************************/
AlphaList 
improveIncPrune( AlphaList *projection, 
		 PomdpSolveParams param ) 
{
  /* The main incremental pruning algorithm routine for finding the
     Q-function represention for value iteration with POMDPs.  
  */
  int z;
  AlphaList list = NULL;
  AlphaList next_list;

  Assert ( projection != NULL, "Projection is NULL." );

  /* Check for the special case of a single observation POMDP.  This
     corresponds to a completely unobservable problem, but for this
     we have no cross-sum to do. */
  if ( gNumObservations == 1 ) 
    return ( duplicateAlphaList( projection[0] ));

    /* We do one less cross-sum than the number of observations. */
   for( z = 1; z < gNumObservations; z++) {
     
     /* Note that we put the burden of setting the source arrays on the
        crossSum routine. */
     if ( z == 1 ) 
       next_list = generalizedIpCrossSum( projection[0], 
                                          projection[1],
                                          param );

     else {
       next_list = generalizedIpCrossSum( list, 
                                          projection[z],
                                          param);
       destroyAlphaList( list );
     }
     
     /* Note that if we simply put the pruning operation here, then
        you get the vanilla incremental pruning algorithm!!! */
     
     list = next_list;
     
   } /* for z */
   
   return ( list );

} /* improveIncPrune */
/**********************************************************************/


