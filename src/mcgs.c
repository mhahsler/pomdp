
/*
 *<SOURCE_HEADER>
 *
 *  <NAME>
 *    approx_mcgs.c
 *  </NAME>
 *  <AUTHOR>
 *    Anthony R. Cassandra
 *  </AUTHOR>
 *  <CREATE_DATE>
 *    
 *  </CREATE_DATE>
 *
 *  <RCS_KEYWORD>
 *    $RCSfile: mcgs.c,v $
 *    $Source: /u/cvs/proj/pomdp-solve/src/mcgs.c,v $
 *    $Revision: 1.2 $
 *    $Date: 2005/02/22 22:10:36 $
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
 *   Routines related to approximately solving a POMDP using a Monte
 *   Carlo sampling of trajectories and a Gauss-Seidel like updating of
 *   the value function.
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "mdp/mdp.h"

#include "global.h"
#include "signal-handler.h"
#include "alpha.h"
#include "common.h"
#include "params.h"
#include "projection.h"
#include "parsimonious.h"
#include "pomdp-solve.h"
#include "sim.h"
#include "mcgs.h"

/**********************************************************************/
void 
MCGS_initialize( PomdpSolveParams param ) {

  /* Nothing to do for now. */
 
} /* MCGS_initialize */

/**********************************************************************/
void
MCGS_cleanup( ) {

  /* Nothing to do for now. */
 
} /* MCGS_cleanup */

/**********************************************************************/
AlphaList 
MCGS_improve( AlphaList **projections, PomdpSolveParams param ) {

  Trajectory trajectory, t;
  int trajectory_count = 0;
  AlphaList old_vector_list;
  AlphaList new_vector_list;
  double gen_time, prune_time;
  double tot_gen_time = 0.0, tot_prune_time = 0.0;
  int m, num_new_points = 0;

  /* FIXME: Need to rethink how this works before making it
	available. */
  Abort( "MCGS method not yet available. Sorry.");
 
  if ( param->opts->verbose == POMDP_SOLVE_OPTS_Verbose_mcgs ) {
    fprintf( param->report_file, 
             "<<Monte Carlo Gauss Seidel Approximation>>\n" );
  } /* if verbose */

  /* Use an old and a new and periodically merge them. */
  old_vector_list = newAlphaList();
  new_vector_list = newAlphaList();

  trajectory = newTrajectory( param->opts->mcgs_traj_length );

  if ( param->opts->verbose == POMDP_SOLVE_OPTS_Verbose_mcgs )
    fprintf( param->report_file,
             "Using trajectory length %d.\n", 
             param->opts->mcgs_traj_length );

  while ( (! gInterrupt) 
          && param->opts->mcgs_num_traj > trajectory_count ) {

    /* Generate a trajectory of length N from starting state,
       accumulating a set of N points. Uses random selection for the
       policy (the NULL parameter specifies this). */
    generateTrajectory( trajectory, gInitialBelief, NULL );
    trajectory_count++;

    if ( param->opts->verbose == POMDP_SOLVE_OPTS_Verbose_mcgs )
      fprintf( param->report_file,
               "Trajectory %d.\n", trajectory_count );
    
    /* Loop over this set of points M times. */
    for( m = 0; m < param->opts->mcgs_traj_iter_count; m++ ) {

      /* Loop over the points from the trajectory. */
      for ( t = trajectory; t != NULL; t = t->next ) {

        if ( gInterrupt )
          goto END_MCGS;

        /* For each point, and the current alpha vector list, generate the
           vector for this point. This routine also adds the vector to
           the list (if it is not already in the list. */
        makeAlphaVector( new_vector_list,
                         projections,
                         t->belief,
                         SMALLEST_PRECISION );

	   /* FIXME: On review, this does not look right to me.  Seems
		 like we would only want to increment *if* the new vector is
		 added, and further, this is a count of new *value function
		 facets* not points. Also, seems like we can just look at
		 the size of the new list to determine whether it is time to
		 merge and prune. */
        num_new_points++;

        if ( param->opts->mcgs_prune_freq == num_new_points ) {
        
          num_new_points = 0;

          if ( param->opts->verbose == POMDP_SOLVE_OPTS_Verbose_mcgs ) {
            fprintf( param->report_file,
                     "Changing value function..." );
            fflush(  param->report_file );
          } /* if versboe */

          /* First merge the two sets... */
          unionTwoAlphaLists( old_vector_list, new_vector_list );

          /* ... create a new list ... */
          new_vector_list = newAlphaList();

          if ( param->opts->verbose == POMDP_SOLVE_OPTS_Verbose_mcgs )
            fprintf( param->report_file,
                     "%d vectors -> ", sizeAlphaList( old_vector_list ) );

          /* ... then prune this set */
          normalPrune( old_vector_list, param );

          /* Clear the old projection set ... */
          clearAllProjections( projections );

          /* ... and create the new projection set */
          setAllProjections( projections, old_vector_list );

          if ( gVerbose[V_APPROX_MCGS] )
            fprintf( param->report_file,
                     "%d vectors.\n", sizeAlphaList( old_vector_list ) );

        } /* if time to prune */

      } /* for n */

    } /* for m */

  } /* while not done */

  END_MCGS:

  destroyAlphaList( new_vector_list );
  freeTrajectory( trajectory );
  freeAllProjections( projections );

  return ( old_vector_list );

}  /* MCGS_improve */

/**********************************************************************/
