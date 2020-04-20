
/*
 *<SOURCE_HEADER>
 *
 *  <NAME>
 *    witness.c
 *  </NAME>
 *  <AUTHOR>
 *    Anthony R. Cassandra
 *  </AUTHOR>
 *  <CREATE_DATE>
 *    July, 1998
 *  </CREATE_DATE>
 *
 *  <RCS_KEYWORD>
 *    $RCSfile: witness.c,v $
 *    $Source: /u/cvs/proj/pomdp-solve/src/witness.c,v $
 *    $Revision: 1.3 $
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
 *   This file contains code that is specific to the revised witness
 *   algorithm for solving partially observable Markov decision
 *   processes.  This algorithm was first presented in:
 * 
 *   "Acting Optimally in Partially Observable Stochastic Domains",
 *   Cassandra, Kaelbling and Littman, AAAI-94
 * 
 *   More details are provided in Brown University Technical Reports
 *   CS94-14 (Cassandra), and CS94-40 (Littman) as well as in Michael
 *   Littman's and Anthony Cassandra's Ph.D. Theses, both from Brown
 *   University, Department of Computer Science.
 * 
 */


#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "mdp/mdp.h"

#include "global.h"
#include "pomdp.h"
#include "alpha.h"
#include "stats.h"
#include "params.h"
#include "parsimonious.h"
#include "neighbor.h"
#include "common.h"
#include "region.h"
#include "witness.h"

/* Use this to get witness points for each iteration. */
double *gTempWitness;

/**********************************************************************/
void 
initWitness( ) 
{

  gTempWitness = (double *) XMALLOC( gNumStates * sizeof( double ));

}  /* initWitness */
/**********************************************************************/
void 
cleanUpWitness( ) 
{

  XFREE( gTempWitness );
}  /* cleanUpWitness */
/**********************************************************************/
AlphaList 
improveWitness( AlphaList *projection,
			 PomdpSolveParams param ) 
{
  /*
    The main witness algorithm routine for finding the Q-function
    represention for value iteration with POMDPs.  
  */
  AlphaList new_alpha_list, node;
  AlphaList agenda, cur_agenda_item;

  Assert ( projection != NULL, "Projection sets are NULL." );

  new_alpha_list = newAlphaList();

  /* Initializes the set using the simplex corners and optionally
     other points.  If the set returned has only one vector then we
     know it is the entire set because the simplex corner check is
     guaranteed to return more than one vector if the parsimonious set
     contains more than 1 vector. */
  if ( initListSimpleQ( new_alpha_list, projection, param ) < 2 )
    return ( new_alpha_list );

  if ( gVerbose[V_WITNESS] )
    fprintf( param->report_file, 
             "\t  Witness init: %d vectors.\n",
             new_alpha_list->length );

  /* We use an agenda of neighbors to drive the main outer witness
     loop. */
  agenda = newAlphaList();

  /* For each vector in the list initially, we must add all their
     'neighbors' to the agenda of things to be checked. Since each
     vector was created with the obs_source array set to point into
     the projection sets, we have the necessary information to define
     the neighbors of each vector. */
  addAllNeighborsFromList( agenda, new_alpha_list, 
                           projection, 
                           param->domination_check,
                           param->alpha_epsilon );

  /* Now we loop until every agenda item in the agenda list has ben
     marked. */
  while ( ( cur_agenda_item = extractUnmarkedVector( agenda )) 
          != NULL ) {

    /* This is the main thing: see if agenda item leads to a witness
       point. This condition calls the simple empty region check
       routine which does a number of things before bringing out the
       heavy LP guns: checks both if item is already in the list or if
       it is dominated by something in the list.  */
    if ( ! isEmptyRegionSimpleCheck( new_alpha_list, 
                                     cur_agenda_item->alpha,
                                     param->alpha_epsilon,
                                     param->domination_check )
         && findRegionPoint( cur_agenda_item->alpha, 
                             new_alpha_list, 
                             gTempWitness,
                             NULL,
                             param )) {

      /* Find the best vector at this point and add it to the list.
         Note that this will check to make sure the vector is not
         already in the list and return NULL if it is. */

      node = addVectorAtBeliefQ( new_alpha_list, 
                                 gTempWitness,
                                 projection,
                                 param->use_witness_points,
                                 param->alpha_epsilon );

      /* In theory, we should never get a NULL node here, since we in
         fact have a witness point showing that the current set is not
         sufficient.  However, the interaction of the LP precision an
         the precision we use to compare vectors might not be
         completely in synch, so a NULL return value is a
         possibility. When it is NULL, we better mark this node or
         else we will continually loop. */
      if ( node == NULL )
        cur_agenda_item->mark = TRUE;

      /* The normal case is this 'else' clause where we add the
         neighbors of the node just added to the new list. */
      else
        /* We have to add all its neighbors to the agenda. Note that
           this will do nothing if node == NULL. */
        addAllNeighbors( agenda, node, projection, 
                         param->domination_check,
                         param->alpha_epsilon );

    } /* if region is non-empty */

    /* Otherwise, this agenda item does not lead to a witness point
       and we can remove it from the list. */
    else 
      cur_agenda_item->mark = TRUE;

    /* Regardless of whether the item was marked or not we add it back
       into the agenda.  If it is marked, then we simply will not get
       it from the getNextAgendaItem() routine and it helps to know
       which neighbors we have already encountered. If it is unmarked,
       it prepends the node, if marked it appends the node.  This way
       we will always have the unmarked one up front whch make them
       quicker to get at. The only time we will have to run all the
       way through the list is when all the vectors are marked. */
    if ( cur_agenda_item->mark )
      prependNodeToAlphaList( agenda, cur_agenda_item );
    else
      appendNodeToAlphaList( agenda, cur_agenda_item );

    /* We may be using options that will cause us to want to terminate
	  this loop early.  Here we check this condition and bail if it
	  becomes true.  The param structure will contain the necessary
	  information. 
    */
    if ( shouldTerminateEarly( new_alpha_list, param ))
	 break;

  } /* while more agenda items exist */

  /* Since we did not destroy any agenda items while doing the witness
     loop, we must get rid of all of them now. */
  destroyAlphaList( agenda );

  return ( new_alpha_list );

}  /* improveWitness */
/**********************************************************************/
