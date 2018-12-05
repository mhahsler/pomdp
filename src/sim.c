
/*
 *<SOURCE_HEADER>
 *
 *  <NAME>
 *    sim.c
 *  </NAME>
 *  <AUTHOR>
 *    Anthony R. Cassandra
 *  </AUTHOR>
 *  <CREATE_DATE>
 *    
 *  </CREATE_DATE>
 *
 *  <RCS_KEYWORD>
 *    $RCSfile: sim.c,v $
 *    $Source: /u/cvs/proj/pomdp-solve/src/sim.c,v $
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
 *   Routines related to simulating steps in a POMDP.
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "mdp/mdp.h"

#include "global.h"
#include "random.h"
#include "alpha.h"
#include "belief.h"
#include "sim.h"

/**********************************************************************/
int 
getBeliefObservation( int action, double *b  ) 
{
  /*
    Gets an observation based on the current belief state.  Since the
    belief state represents the true occupational probabilities, any
    observation generated using this (and observation probabilities),
    will be a consistent one.  Essentially, you know what the
    distribution of possible observations will be, given your belief
    state. (i.e., it is not necessary to know the true underlying core
    state to know the observation distribution.) Otherwise know as the
    "Littman Method".  
  */
  double rand, prob_sum, sum;
  int obs, state, j, desperate_obs;
   
  Assert( ( action >= 0) && (b != NULL) && (action < gNumActions ),
          "Bad parameters." );
  
  rand = fran();
  prob_sum = 0.0;
  
  for( obs = 0; obs < gNumObservations; obs++ ) {
    sum = 0.0;

    for( state = 0; state < gNumStates; state++ ) 
      for( j = P[action]->row_start[state];
           j <  P[action]->row_start[state] + P[action]->row_length[state];
           j++ ) 

        sum += b[state] 
          * P[action]->mat_val[j] 
          * getEntryMatrix( R[action], P[action]->col[j], obs );
      
    /* There is a chance that precision errors could go through
       every observation and still not choose an observation.  In
       this case we want to return something reasonable.  We will
       set this desperate_obs to be any observation which has positive
       probability, and return this one if the precision problem
       occurs. */
    if( sum > 0.0 ) 
      desperate_obs = obs;
    
    prob_sum += sum;
    
    if( rand < prob_sum )
      return( obs );
  } /* for obs */

  /* If we just miss because of floating point precision
     problems, we still need to return something reasonable */
  return( desperate_obs );

}  /* getBeliefObservation */
/**********************************************************************/
int 
updateBeliefState( double *b, 
		   int action,
		   double *next_b ) 
{
  /*
    This routine is useful for running a simulation where we do not
    want/need to keep track of the underlying core state of the
    process.  Since the belief state has all the information we need,
    we can generate the observation and reward from it.  The expected
    values of each will be the same as if you were using an actual 
    underlying state.  It returns the observation seen and the 
    'reward' parameters is set to the reward received.
  */
  int obs;

  if (( b == NULL) || (action < 0) 
         || (action >= gNumActions) || (next_b == NULL))
    exit(0);

  Assert(( b != NULL) && (action >= 0) 
         && (action < gNumActions) && (next_b != NULL),
         "Bad parameters." );

  obs = getBeliefObservation( action, b );
  
  if( !transformBeliefState( b, next_b, 
                             action, obs ))
    Warning( "Could not transform belief state.");
  
  return( obs );
}  /* updateBeliefState */
/**********************************************************************/

/**********************************************************************/
/************   For trajectories    ***********************************/
/**********************************************************************/

/**********************************************************************/
Trajectory 
newTrajectoryNode( ) 
{
  /*
    Allocates the memory for one node in a trajectory linked list.
  */
  Trajectory node;

  node = (Trajectory) XMALLOC( sizeof ( *node ));
  node->belief = (double *) XCALLOC( gNumStates, sizeof ( double ));
  node->next = NULL;
  
  return ( node );
} /* newTrajectoryNode */
/**********************************************************************/
Trajectory 
prependTrajectoryNode( Trajectory list, Trajectory node ) 
{
  /*
    Prepends this node to the front of the 'list' sent in.
  */
  if ( node == NULL )
    return ( list );

  node->next = list;

  return ( node );
  
}  /* prependTrajectoryNode */
/**********************************************************************/
Trajectory 
newTrajectory( int length ) 
{
  /*
    Allocates memory for a trjectory of the given length.
  */
  Trajectory trajectory = NULL, node;
  int i;

  for ( i = 0; i < length; i++ ) {

    node = newTrajectoryNode();

    trajectory = prependTrajectoryNode( trajectory, node );

  } /* for i */
  
  return ( trajectory );

}  /* newTrajectory */
/**********************************************************************/
void 
freeTrajectory( Trajectory trajectory ) 
{
  /*
    Deallocates the memory for a trajectory linked list. 
  */
  Trajectory temp;

  while ( trajectory != NULL ) {

    temp = trajectory;
    trajectory = trajectory->next;

    XFREE( temp->belief );
    XFREE( temp );

  } /* while */

}  /* freeTrajectory */
/**********************************************************************/
void 
generateTrajectory( Trajectory trajectory, 
		    double *init_b,
		    AlphaList for_policy ) 
{
  /*
    Generates a trajectory using either randomly selection actions or
    the value function (AlphaLIst) sent in.  If the init_b
    initial belief state is NULL, then a random belief distribution is
    used, otherwise the first belief in the trajectory will be this
    belief state.  Assumes the memory for the trajectory has been
    allocated already.
  */
  AlphaList best;
  double dummy_value;

  /* If it is an empty trajectory, just bail out. */
  if ( trajectory == NULL )
    return;

  /* If no initial belief is specified then use a random one */
  if ( init_b == NULL )
    setRandomDistribution( trajectory->belief, gNumStates );

  /* Otherwise we have an initial belief. */
  else 
    copyBelief( trajectory->belief, init_b );

  /* We use the current belief to select an action and then generate
     the next belief in the next node in the list */
  while ( trajectory->next != NULL ) {

    if ( for_policy == NULL )
      trajectory->action = getRandomInt( 0, gNumActions-1 );

    /* Otherwise we use the value function sent in for selecting
       actions. */
    else { 
      best = bestVector( for_policy, trajectory->belief, 
                         &dummy_value, SMALLEST_PRECISION );

      trajectory->action = best->action;
    
    } /* if using a value function for selecting actions */

    /* Now update the belief state using that action and getting the
       observation too. */
    trajectory->obs = updateBeliefState( trajectory->belief,
                                         trajectory->action,
                                         trajectory->next->belief );

    trajectory = trajectory->next;

  } /* while */

}  /* generateTrajectory */
/**********************************************************************/
void 
showTrajectory( Trajectory trajectory ) 
{
  int i;

  while ( trajectory != NULL ) {

    printf( "[ " );

    for ( i = 0; i < gNumStates; i++ )
      printf( "%.3lf ", trajectory->belief[i] );

    printf ("] (a=%d, z=%d)\n", trajectory->action, trajectory->obs );

    trajectory = trajectory->next;

  } /* while */

}  /* showTrajectory */
/**********************************************************************/
