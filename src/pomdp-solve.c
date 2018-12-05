
/*
 *<SOURCE_HEADER>
 *
 *  <NAME>
 *    pomdp-solve.c
 *  </NAME>
 *  <AUTHOR>
 *    Anthony R. Cassandra
 *  </AUTHOR>
 *  <CREATE_DATE>
 *    February, 1999
 *  </CREATE_DATE>
 *
 *  <RCS_KEYWORD>
 *    $RCSfile: pomdp-solve.c,v $
 *    $Source: /u/cvs/proj/pomdp-solve/src/pomdp-solve.c,v $
 *    $Revision: 1.14 $
 *    $Date: 2005/10/30 23:21:17 $
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
 *   This file contains the outer-most loop code for solving finite
 *   horizon partially observable Markov decision problems using value
 *   iteration with dynamic programming.  It will call the appropriate
 *   algorithm to solve for each epoch.
 *   
 *   The command line arguments are shown by running:
 * 
 *         pomdp-solve -h
 */


#define POMDP_SOLVE_C

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "mdp/mdp.h"

#include "global.h"
#include "signal-handler.h"
#include "cmd-line.h"
#include "pomdp.h"
#include "alpha.h"
#include "stats.h"
#include "lp-interface.h"
#include "common.h"
#include "pg.h"
#include "projection.h"
#include "enumeration.h"
#include "linear-support.h"
#include "two-pass.h"
#include "witness.h"
#include "inc-prune.h"
#include "finite-grid.h"
#include "zlz_speedup.h"
#include "mcgs.h"
#include "pomdp-solve.h"

/**********************************************************************/
/************* Routines for beginning and end of solving   ************/
/**********************************************************************/

/**********************************************************************/
void 
initPomdpSolve( PomdpSolveParams param ) 
{
  char msg[MAX_MSG_LENGTH];
  
  /* We need to do this first, since there are other things to come
     which need to know the POMDP parameters (or at least the sizes
     of the states, action, observation, etc. Will also do the
     precomputation of which observations are possible, so we need to
     provide the epsilon value to use to determine this. */
  fprintf( param->report_file, "[Initializing POMDP ... " );
  fflush( param->report_file );

  initializePomdp( param->param_filename, 
                   param->impossible_obs_epsilon );

  fprintf( param->report_file, "done.]\n" );

   /* We allow the discount factor to be over-ridden by a command line
      option.  To make sure we get the right discount, we have to
      check for the over-ridden value *AFTER* we parse the POMDP
      file, which is done in the initializePOMDP() routine. If
      we do override it, just set its value to the new value. */
   if ( param->override_discount >= 0.0 ) 
     gDiscount = param->override_discount;

   /* We will save the solution after each iteration in a temporary
      file, so that if the program terminates abnormally, we can
      recover the latest solution. We will use the PID of the file to
      make sure the filename is unique so multiple copies can run at
      the same time. Note that for POSIX, the 'pid_t' type returned by
      getpid() is an 'int'. */
   sprintf( param->backup_file, SAVE_FILE_NAME_FORMAT, getPid() );

   /* For saving penultimate alpha file (if selected). */
   strcpy(  param->penultimate_filename, param->opts->prefix_str );
   strcat(  param->penultimate_filename, PENULTIMATE_SUFFIX);
   strcat(  param->penultimate_filename, ALPHA_FILE_SUFFIX);

   /* Set the output file names based upon the gPrefixStr */
   strcpy(  param->alpha_filename, param->opts->prefix_str );
   strcat(  param->alpha_filename, ALPHA_FILE_SUFFIX);
   strcpy(  param->pg_filename, param->opts->prefix_str );
   strcat(  param->pg_filename, PG_FILE_SUFFIX );

   if ( param->initial_policy_filename[0] != NULL_CHAR ) {
     if (( param->initial_policy 
           = readAlphaList( param->initial_policy_filename, 
                            -1 )) == NULL) {
       
       sprintf
         ( msg, 
           "Cannot open initial policy file name: %s.\n\t(Using default)",
           param->initial_policy_filename );
       Warning( msg );
     }
   }

   /* Do the right initialization for whichever algorithm was chosen.
      We really want to only call appropriate initialization, but we
      may need Lark for the purging of the S_z sets.  Therefore, to
      ensure that we always have the proper memory allocated, we call
      initLark() in all algorithms. */
   switch ( param->opts->method ) {
   case POMDP_SOLVE_OPTS_Method_enum:
     initEnumeration( );
     break;
   case POMDP_SOLVE_OPTS_Method_witness:
     initWitness();
     break;

   case POMDP_SOLVE_OPTS_Method_twopass:
     initTwoPass( );
     break;
   case POMDP_SOLVE_OPTS_Method_linsup:
     // initLinSupport( );
     Abort("linsup not implemented!")
     break;
   case POMDP_SOLVE_OPTS_Method_incprune:
     initIncPrune( );
     break;

   case POMDP_SOLVE_OPTS_Method_grid:
	initFiniteGrid( param );
	break;

   case POMDP_SOLVE_OPTS_Method_mcgs:
	MCGS_initialize( param );
	break;

   default:
     break;
   }  /* switch gMethod */
   
   /* Some of the routines in common.c need a hunk of memory to
      compute things, thus we allocate this memory once up front to
      save the malloc/free calls. */ 
   initCommon();

   /* Set up any globally used things just before solving. Must do
      this after we have read in the POMDP problem, since it uses the
      problem size when allocating memory. */
   initGlobal();

   /* Set up anything we need in the LP stuff.  */
   initLpInterface( param );

   /* If an upper limit on time was specified on the command line,
      then set up a timer to signal us when time is up. */
   setUpIntervalTimer( param->max_secs );
   
   /* Set the virtual memory limit here.  This will help to stop
      run-away processes.  */
   setMemoryLimit( param->memory_limit );

   /* If we do get an interrupt, we wil need to be able to access the
      current context.  Set the parameter context here. */
   setInterruptParamContext( param );

   /* Set it up to catch CTRL-C */
   setUpCtrlC();

}  /* initPomdpSolve */
/**********************************************************************/
void 
cleanUpPomdpSolve( PomdpSolveParams param ) 
{

  switch ( param->opts->method ) {
  case POMDP_SOLVE_OPTS_Method_enum:
    cleanUpEnumeration( );
    break;
  case POMDP_SOLVE_OPTS_Method_witness:
    cleanUpWitness();
    break;
    
  case POMDP_SOLVE_OPTS_Method_twopass:
    cleanUpTwoPass( );
    break;
    
  case POMDP_SOLVE_OPTS_Method_linsup:
    Abort("linsup not implemented!");
    //cleanUpLinSupport( );
    break;
    
  case POMDP_SOLVE_OPTS_Method_incprune:
    cleanUpIncPrune( );
    break;
    
  case POMDP_SOLVE_OPTS_Method_grid:
    cleanUpFiniteGrid( );
    break;
    
  case POMDP_SOLVE_OPTS_Method_mcgs:
    MCGS_cleanup();
    break;

   default:
    break;
  }  /* switch gMethod */

  /* Undo whatever initLpInterface() does. */
  cleanUpLpInterface();

  /* Undo whatever initCommon() does. */
  cleanUpCommon();
  
  /* Undo whatever initGlobal() does. */
  cleanUpGlobal();
  
  /* Deallocate the POMDP problem parameters. */
  cleanUpPomdp();

  /* Close the reporting file. */
  fclose( param->report_file );
  
  /* Don't need this structure anymore. */ 
  destroyPomdpSolveParams ( param );
  
}  /* cleanUpPomdpSolve */
/**********************************************************************/
void 
endPomdpSolve( PomdpSolveParams param,
	       AlphaList solution ) 
{
  /* 
     The gSuccinct variable is used when we want a very concise reporting
     of the results of the program.  This is useful for running series
     of experiments and combining the results. Thus, we only output
     out final report if we are not being brief. 
  */
  AlphaList scaled_list;

  Assert( param != NULL && solution != NULL, 
          "Bad (NULL) parameters." );

  /* Write the solution files, but note that this must be done before
     we clear the prev_alpha_list to ensure policy graph information
     is preserved. Also, we have to check whether we need to adjust
     the values to compensate for adjustment in the immediate reward
     structure. */
  if ( valuesRequireScaling() ) {
    scaled_list = makeScaledAlphaList( solution, param->update_count );
    saveAlphaList( scaled_list, param->alpha_filename );
    destroyAlphaList( scaled_list );
  } /* if values required scaling */

  /* Otherwise, no scaling required so just write out the list. */
  else
    saveAlphaList( solution, param->alpha_filename );

  APG_writePolicyGraph( solution, param->pg_filename );
  
  if ( gVerbose[V_POMDP_SOLVE] ) {
    fprintf( param->report_file, 
             "The solution to the (in)finite horizon is:\n" );
    displayAlphaList( param->report_file, solution );
  }
  else {
    fprintf(param->report_file, 
            "++++++++++++++++++++++++++++++++++++++++\n");
    fprintf(param->report_file,
            "Solution found.  See file:\n\t%s\n\t%s\n",
            param->alpha_filename, param->pg_filename );
    fprintf(param->report_file,
            "++++++++++++++++++++++++++++++++++++++++\n");
  }
    
  /* Show all program stats depending upon execution parameters.. */
  reportStats( param->stat );
 
}  /* endPomdpSolve */
/**********************************************************************/
AlphaList 
getDefaultInitialPolicy( PomdpSolveParams param ) 
{
  /*
    For now our default policy is just all zeroes for exact solving,
    and for finite grid it is gotten from that module since it needs
    to ensure the starting values are a lower bound on the solution.
  */
  AlphaList alpha_list;
  double *alpha;
  int i;

  switch( param->opts->method ) {
    
  case POMDP_SOLVE_OPTS_Method_grid:
    return FG_getInitialvalueFunction( param );
    
    /* All other algorithms default to all zeroes */
  default:

    alpha_list = newAlphaList();
    
    alpha = newAlpha();
    for ( i = 0; i < gNumStates; i++ )
	 alpha[i] = 0.0;
    
    appendAlphaList( alpha_list, alpha, 0 );
    
    return ( alpha_list );
    
  } /* switch gMethod */
}  /* getInitialSolution */
/**********************************************************************/
void 
handleSaveAll( AlphaList list, PomdpSolveParams param ) 
{
  /*
    Handles the case if we want to save the solution for every epoch.
    Note that this saves the results of the DP update, so if the ZLZ
    speedup is being used, you will not get any of those itermediate
    results saved.
    
    This routine should first be called with a NULL parameter to serve
    as the initialization of the filenames that will be used.  We use
    static variables to maintain the filenames.
  */
  static char alpha_filename[MAX_FILENAME_LENGTH];
  static char pg_filename[MAX_FILENAME_LENGTH];
  static char *alpha_filename_null_term, *pf_filename_null_term;
  static char num_str[10];
  AlphaList scaled_list;

  /* A NULL list means we should initialize the names we will be using
     for saving each epoch's solution. */
  if ( list == NULL ) {
    
    /* See if the immediate rewards were scaled and if so, make sure
       we adjust the values in the value funciton to reflect this. */
    
    strcpy( alpha_filename, param->alpha_filename );
    
    strcpy( pg_filename, param->pg_filename );
    
    /* This is a memory address calculation to provide a pointer into
       the strings where the NULL terminator exists.  We will be
       appending unique ids for each epoch and want to just
       over-write the suffix each time. */
    alpha_filename_null_term = alpha_filename + strlen( alpha_filename );
    pf_filename_null_term = pg_filename + strlen( pg_filename );

    return;

  } /* if initialization ( list == NULL ) */

  /* If we have chosen to save each epoch's answer then write out
     the alpha vectors and policy tree with the unique filename.
     This just appends the epoch number to the name. */
  sprintf( num_str, "%d", param->cur_epoch );
  strcat( alpha_filename, num_str );
  strcat( pg_filename, num_str );
  
  if ( valuesRequireScaling() ) {
    scaled_list = makeScaledAlphaList( list, param->update_count );
    saveAlphaList( scaled_list, alpha_filename );
    destroyAlphaList( scaled_list );
  } /* if values required scaling */

  /* Otherwise, no scaling required so just write out the list. */
  else
    saveAlphaList( list, alpha_filename );

  /* If the ZLZ speedup is being used, then the policy raph likely
     points to vectors not of the previous DP iteration, but of the
     last ZLZ update. Thus, we do not write out the policy graphs
     when this option is in effect. */
  if ( param->vi_variation != ZlzSpeedup )
    APG_writePolicyGraph( list, pg_filename );
  
  /* Truncate the filenames to make them their original
     name. This just restores the null-terminator to the
     original position. */
  alpha_filename_null_term[0] = NULL_CHAR;
  pf_filename_null_term[0] = NULL_CHAR;

}  /* handleSaveAll */
/**********************************************************************/

/**********************************************************************/
/********  Routines for mid value iteration tests and checks   ********/
/**********************************************************************/

/**********************************************************************/
double 
weakBound( AlphaList cur_alpha_list,
		 AlphaList prev_alpha_list,
		 double delta ) 
{
/*
  Computes the weak bound error difference between two (the current
  and previous)  alpha lists.  Used as a form of stopping condition
  for value iteration.

  The weak bound stopping criteria compares the alpha vector
  components directly withouit regard to the belief space or areas
  over which the vector is dominant.  This results in a comparison at
  the belief simplex corners, which puts a maximal bound on the
  "intererior" difference. Given two hyperplaces over a region, the
  maximal difference between them must occur at the boundaries of the
  region.  
*/
   double max_p_x, min_p_y, max_s, val;
   AlphaList p_x, p_y;
   int s;

  Assert( prev_alpha_list != NULL && cur_alpha_list != NULL,
          "Bad (NULL) parameter(s)." );

   max_p_x = -1.0*HUGE_VAL;
   p_x = cur_alpha_list->head;
   while( p_x != NULL ) {

      min_p_y = HUGE_VAL;
      p_y = prev_alpha_list->head;
      while( p_y != NULL ) {

	   max_s = -1.0*HUGE_VAL; /* max component-wise difference */
         for ( s = 0; s < gNumStates; s++ ) {

            /* added fabs() 8/17/95 */
            val = fabs( p_x->alpha[s] - p_y->alpha[s] );

            if ( val > max_s )
               max_s = val;
         }
            
         if ( max_s < min_p_y )
            min_p_y = max_s;

         p_y = p_y->next;
      }  /* while p_y */

      if ( min_p_y > max_p_x ) 
         max_p_x = min_p_y;
      
      /* This is an optimization to exit the loop early */
      if ( max_p_x > delta )
         return HUGE_VAL;

      p_x = p_x->next;
   }  /* while p_x */

   return ( max_p_x );
}  /* weakBound */
/**********************************************************************/
int 
meetStopCriteria( AlphaList prev_alpha_list, 
			   AlphaList cur_alpha_list,
			   double *error,
			   PomdpSolveParams param ) 
{
  /* 
     Determines whether or not we can stop value iteration.  There are
     different stopping criteria and we want to see if the one selected
     is met.  This is done by comparing the current and previous value
     function (set of alpha vectors).  
  */

  Assert( prev_alpha_list != NULL && cur_alpha_list != NULL,
          "Bad (NULL) parameter(s)." );

  startContext( param->stat, Context_Stop_Criteria );

  /* The finite grid method (being a non-exact method usually), will
	use its own stopping criteria. */
  if ( param->opts->method  == POMDP_SOLVE_OPTS_Method_grid )
    {
	 *error = FG_computeError( prev_alpha_list, cur_alpha_list, param );
    }

  /* Else select from exact stopping criteria setting. */
  else
    {
	 switch( param->opts->stop_criteria ) {
	 case POMDP_SOLVE_OPTS_Stop_Criteria_exact:
	   
	   /* The exact stopping criteria is sensitive to the ordering of the
		 vectors.  Thus, we will sort the current solution before doing
  		 this comparison.  We will assume that the previous  soltuion is
		 already sorted as a result of the previous call to this routine
		 when it would have been the "current" solution. */
	   sortAlphaList( cur_alpha_list );
	   
	   if ( sameAlphaList( prev_alpha_list, 
					   cur_alpha_list,
					   param->alpha_epsilon )) {
		*error = 0.0;
		endContext( param->stat, Context_Stop_Criteria );
		return ( TRUE );
	   } /* if they are identical alpha lists */
	   
	   /* Otherwise they are different and we will assume the error is
		 infinite, since we do not do anything to compute a more precise
		 characterization of the error */
	   *error = HUGE_VAL;
	   endContext( param->stat, Context_Stop_Criteria );
	   return ( FALSE );
	   
	 case POMDP_SOLVE_OPTS_Stop_Criteria_weak:
	   *error = weakBound( cur_alpha_list, prev_alpha_list,
					   param->stop_delta );
	   break;
	   
	 case POMDP_SOLVE_OPTS_Stop_Criteria_bellman:
	   *error = bellmanError( prev_alpha_list,
						 cur_alpha_list,
						 param ); 
	   break;
	   
	 default:
	   Abort( "Unrecognized stopping criteria.\n" );
	   
	 }  /* switch */
	 
    } /* else using exact criteria from param setting */

  endContext( param->stat, Context_Stop_Criteria );

  if ( *error 
       <= param->stop_delta * ( 1.0 - gDiscount ) / ( 2.0 * gDiscount ) )
    return TRUE;
  else
    return FALSE;

}  /* meetStopCriteria */
/**********************************************************************/
double 
getSolvePrecision( PomdpSolveParams param ) 
{
  /*
    This routine will get the precision factor to use during solving of
    the POMDP.  Exactly what it does depends on the variation of value
    iteration.  For normal operation, we use the param->epsilon value
    directly.  For the epsilon pruning of the q-functions version, we
    use the pruning epsilon.  
  */

  if ( param->q_purge_option == purge_epsilon_prune ) 
    return (  param->prune_epsilon );
  
  else
    return ( param->epsilon );

}  /* getSolvePrecision */
/**********************************************************************/
void 
setSolvePrecision( double epsilon, PomdpSolveParams param ) 
{
  /*
    This routine will set the precision factor to use during solving of
    the POMDP.  Exactly what it does depends on the variation of value
    iteration.  For normal operation, changing the precision
    actually involves changing a lot of parameters. For the epsilon
    pruning of the q-functions version, we only need to change the
    pruning epsilon. 
  */

  if ( param->q_purge_option == purge_epsilon_prune )
    param->prune_epsilon = epsilon;

  else {
    
    param->epsilon = epsilon;

    param->lp_epsilon = Min( param->lp_epsilon, param->epsilon );

    LP_setPrecision( param->lp_epsilon );

    /* param->alpha_epsilon = param->epsilon; */
    param->vertex_epsilon = param->epsilon;
    param->double_equality_precision = param->epsilon;
    
    param->impossible_obs_epsilon = Min( param->epsilon, 
                                         DEFAULT_IMPOSSIBLE_OBS_EPSILON );
    
    /* Note that we do not adjust the weka bound stoping criteria
       because we do not want to have value iteration end prematurely
       due to a coarse epsilon */

  } /* else using non epsilon prune of Q functions */
  
}  /* setSolvePrecision */
/**********************************************************************/
void 
doAdjustableEpsilonVariation( PomdpSolveParams param ) 
{
  /*
    zzz Add description here when it solidifies.
  */
  double cur_epsilon;
  int epoch, min_vects, max_vects;
  EpochStats epoch_stats;

  Assert ( param != NULL,
           "NULL parameters." );

  /* Cannot do the adjustable epsilon if there are no stats because we
     need to access the epoch stats to tell when and how to adjust the
     epsilon. */
  if ( param->stat == NULL )
    return;

  /* If we are using the epsilon pruning for the Q sets, then we will
     adjust the epsilon prune parameter.  Otherwise, we will adjust
     the main program epsilon parameter directly. */
  cur_epsilon = getSolvePrecision( param );

  /* First log in the epsilon that was used for this epoch before
     making any changes. */
  recordEpochMaxEpsilon( param->cur_epoch,
                           cur_epsilon,
                           param->stat );

  /* If we have reached the end epsilon, then we don't have to worry
     about adjusting the epsilon at all. */
  if ( cur_epsilon <= param->ending_epsilon )
    return;

  /* If there haven't been enough epochs to have enough history for
     computing whether or not we should adjust the epsilon, then just
     bail out also. */
  if ( param->cur_epoch < param->epoch_history_window_length )
    return;

  /* Now we get the minimum and maximum sizes of the last bunch of
     history of the vector sizes to determine if we can adjust the
     epsilon downwards or not. */
  max_vects = 0;
  min_vects = 99999999;
  for ( epoch = (param->cur_epoch 
                 - param->epoch_history_window_length
                 + 1);
        epoch <= param->cur_epoch;
        epoch++ ) {

    epoch_stats = getEpochStats( param->stat, epoch );

    /* This really shouldn't return NULL, but in case it does, give a
       warning and bail out. */
    if ( epoch_stats == NULL ) {
      Warning( "Could not get the epoch stats." );
      return;
    } /* if couldn't find the epoch stats. */

    /* Maintain the maximum and minium vectors for each epoch */
    min_vects = Min( min_vects, epoch_stats->solution_size );
    max_vects = Max( max_vects, epoch_stats->solution_size );
    
  } /* for epoch */

  /* If the max and the min differ by more than the desired amount,
     then we do not need to adjust the epsilon. */
  if ( (max_vects - min_vects) > param->epoch_history_window_delta )
    return;

  /* If we get to here, then that means we should decreemnt the
     epsilon. */
  cur_epsilon /= param->epsilon_adjust_factor;

  /* If we are using the epsilon pruning for the Q sets,
     then we will adjust the epsilon prune parameter.  Otherwise, we
     will adjust the main program epsilon parameter directly. */
  setSolvePrecision( cur_epsilon, param );

  fprintf( param->report_file,
           ">>Adjusted epsilon to %.3e<<\n", cur_epsilon );
  
}  /* doAdjustableEpsilonVariation */
/**********************************************************************/
void 
doFixedSolnSizeVariation( PomdpSolveParams param ) 
{
  /*
    zzz Add description here when it solidifies.
  */
  double cur_epsilon;

  Assert ( param != NULL,
           "NULL parameters." );

  /* Cannot do the fixed soln size variation if there are no stats
     because we need to access the history of vector sizes to tell
     when and how to adjust the epsilon. */
  if ( param->stat == NULL )
    return;

  /* If we are using the epsilon pruning for the Q sets, then we will
     adjust the epsilon prune parameter.  Otherwise, we will adjust
     the main program epsilon parameter directly. */
  cur_epsilon = getSolvePrecision( param );

  /* First log in the epsilon that was used for this epoch before
     making any changes. */
  recordEpochMaxEpsilon( param->cur_epoch,
                           cur_epsilon,
                           param->stat );

  fprintf( stderr, "doFixedSolnSizeVariation() says:\n" );
  fprintf( stderr, "!!! Implement me !!!\n" );
  exit( 0 );

}  /* doFixedSolnSizeVariation */
/**********************************************************************/
void 
startViEpoch( PomdpSolveParams param ) 
{
  /* 
     This is called at the beginning of each epoch of value iteration so
     that value iteration variations can adjust anything that needs it
     prior to the next iteration. 
  */

  /* One thing we always will adjust is the epoch number. */
  (param->cur_epoch)++;

  switch ( param->vi_variation ) {
  case NormalVi:
    /* Normal value iteration doesn't need to do anything else. */
    return;

  case AdjustableEpsilonVi:
  case FixedSolnSizeVi:
    /* If this is not the first epoch, then we do not need to do
       anything else. */
    if ( param->cur_epoch != 1 ) 
      return;
    
    /* If this is the first epoch, then we need to set the starting
       epsilon. If we are using the epsilon pruning for the Q sets,
       then we will adjust the epsilon prune parameter.  Otherwise, we
       will adjust the main program epsilon parameter directly. */
    setSolvePrecision( param->starting_epsilon, param );
      
    fprintf( param->report_file,
             ">>Starting epsilon set to %.3e<<\n", 
             param->starting_epsilon );

    return;
    
  default:
    break;
  } /* switch */

}  /* startViEpoch */
/**********************************************************************/
void 
endViEpoch( PomdpSolveParams param ) 
{
  /*
    This is called at the end of each epoch of value iteration so that
    value iteration variations can adjust anything that needs it prior
    to the next iteration. 
  */

  switch ( param->vi_variation ) {
  case NormalVi:
    /* Normal value iteration doesn't need to do anything. */
    return;

  case AdjustableEpsilonVi:
    doAdjustableEpsilonVariation( param );
    return;

  case FixedSolnSizeVi:
    doFixedSolnSizeVariation( param );
    return;
    
  default:
    break;
  } /* switch */
  
}  /* endViEpoch */
/**********************************************************************

/**********************************************************************/
/**************     High Level Solution Routines      *****************/
/**********************************************************************/

/**********************************************************************/
AlphaList 
improveByQ( AlphaList **projection,
	    PomdpSolveParams param ) 
{
  /* 
     Some algorithms will solve one iteration of POMDP value iteration
     by breaking the problem into a separate one for each action.
     This routine will implement the basic structure needed and call the
     appropriate routines depending on the specific algorithm being used.
     
     Current algorithms that do it this way:
       TwoPass
       Witness
       IncrementalPruning
*/
  AlphaList new_list, cur_list;
  int a;
  int start_lps, start_constraints, end_lps, end_constraints;

  /* Nothing to do if no projections. */
  Assert ( projection != NULL && param != NULL, 
           "Bad (NULL) parameters." );
  
  new_list = newAlphaList();
  
  for( a = 0; a < gNumActions; a++ ) {
    
    if ( gVerbose[V_POMDP_SOLVE] == TRUE ) 
      getLpStats( param->stat, &start_lps, &start_constraints );
    
    startContext( param->stat, Context_Q_a_build );
    
    switch ( param->opts->method ) {
    case POMDP_SOLVE_OPTS_Method_twopass:
      cur_list = improveTwoPass( projection[a], param );
      break;
      
    case POMDP_SOLVE_OPTS_Method_witness:
      cur_list = improveWitness( projection[a], param );
      break;
      
    case POMDP_SOLVE_OPTS_Method_incprune:
      cur_list = improveIncPrune( projection[a], param );
      break;
      
    default:
      Abort( "Unreckognized solution method for improveByQ()." );
      
    }  /* switch gMethod */
    
    endContext( param->stat, Context_Q_a_build );
    
    if ( gVerbose[V_POMDP_SOLVE] == TRUE ) {
      
      getLpStats( param->stat, &end_lps, &end_constraints );
      fprintf( param->report_file, 
               "Construct Q end: Q^%d size: %d  M: %d  Z: %d\n",
               a, sizeAlphaList( cur_list ),
               maxSizeAlphaLists( projection[a], gNumObservations ), 
               gNumObservations );
      fprintf( param->report_file, 
               "\tLPs: %d\tConstraints: %d\n",
               end_lps - start_lps, end_constraints - start_constraints );
      
    } /* if verbose */
    
    /* Must do this *after* referencing cur_list since this is a
       destructive union that will obliterate cur_list. */
    unionTwoAlphaLists( new_list, cur_list );
    
  }  /* for a */
  
  startContext( param->stat, Context_Q_a_merge );
  purgeAlphaList( new_list, 
                  param->q_purge_option, 
                  param );

  /* If we used epsilon pruning, then record the computed difference
     which was temporarily stored in the 'param' structure. */
  if (( param->stat != NULL ) 
      && ( param->q_purge_option == purge_epsilon_prune ))
    recordEpochMaxEpsilon( param->stat->cur_epoch,
                           param->epsilon_diff_of_last_prune,
                           param->stat );

  endContext( param->stat, Context_Q_a_merge );

  return ( new_list );

}  /* improveByQ */
/**********************************************************************/
AlphaList 
improveV( AlphaList prev_alpha_list,
		PomdpSolveParams param ) 
{
  /*
    This does a single DP step of value iteration for a POMDP.  It takes
    in the previous value function and parameters for solving and
    returns the next or improved solution.  
  */
  AlphaList next_alpha_list;
  AlphaList **projection;

  Assert( prev_alpha_list != NULL && param != NULL,
          "Bad (NULL) parameters." );

  /* No matter what the algorithm, we will use the projection sets
     to construct the solutions, so make all the projections
     vector sets now. */
  startContext( param->stat, Context_Projection_build );
  projection = makeAllProjections( prev_alpha_list );
  endContext( param->stat, Context_Projection_build );
  
  startContext( param->stat, Context_Projection_purge );
  purgeProjections( projection, param );
  endContext( param->stat, Context_Projection_purge );
  
  switch( param->opts->method ) {
    
  case POMDP_SOLVE_OPTS_Method_enum:
    next_alpha_list = improveEnumeration( projection, param );
    break;
    
  case POMDP_SOLVE_OPTS_Method_linsup:
    next_alpha_list = improveLinSupport( projection, param );
    break;
    
    /* The witness, incremental pruning and two-pass algorithms
       construct the next alpha list one action at a time.  As a
       result, they share a lot of common structure.  Therefore,
       we will just call the improveByQ() routine which will do
       the right thing for each algorithm. */
  case POMDP_SOLVE_OPTS_Method_witness:
  case POMDP_SOLVE_OPTS_Method_incprune:
  case POMDP_SOLVE_OPTS_Method_twopass:
    next_alpha_list = improveByQ( projection, param );
    break;

  case POMDP_SOLVE_OPTS_Method_grid:
    next_alpha_list = improveFiniteGrid( prev_alpha_list, projection, param );
    break;

  case POMDP_SOLVE_OPTS_Method_mcgs:
    next_alpha_list = MCGS_improve( projection, param );
    break;
      
  default:
    Abort( "Unrecognized solution method.");
    
  } /* switch gMethod */

  /* While building the next alpha list, the 'obs_source' points
     into the projection vectors, but now we want them to point
     directly into prev_alpha_list for purposes of the policy
     graph stuff. */
  relinkObsSources( next_alpha_list );
  
  /* Having redirected the obs_source pointer in next_alpha_list
     from projection to prev_alpha_list, we can now free up the
     memory for the projections, since we no longer need them and
     we don't have to worry about leaving pointers to nowhere. */
  freeAllProjections( projection );
  
  return ( next_alpha_list );

}  /* improveV */
/**********************************************************************/
void 
solvePomdp( PomdpSolveParams param ) 
{
  /*
    If horizon < 0 then it will run until it converges or until
    SIGINT signal is received.  If initial_policy is NULL, then the default
    initial policy will be used.
  */
  AlphaList prev_alpha_list = NULL;
  AlphaList next_alpha_list;
  int done = FALSE;
  double cur_error;

  Assert( param != NULL,
          "Parameter structure is NULL." );

  /* Set up name for saving alpha vectors and policy tree
     in case -save_all flag used.  Calling this routine with a NULL
     list serves to initialize it. */
  if ( param->save_all == TRUE )
    handleSaveAll( NULL, param );

  /* Get the initial policy/value function to use. Note that we set
     these to next_alpha_list because the first thing the loop does
     is to swap in the next_alpha_list for the current_alpha_list. */
  if ( param->initial_policy == NULL )
    next_alpha_list = getDefaultInitialPolicy( param );
  else
    next_alpha_list = duplicateAlphaList( param->initial_policy );
  
  /* Just report the initial policy used. */
  if ( gVerbose[V_POMDP_SOLVE] == TRUE ) {
    fprintf( param->report_file, "The initial policy being used:\n");
    displayAlphaList( param->report_file, next_alpha_list );
  }
  else
    fprintf( param->report_file, 
             "[Initial policy has %d vectors.]\n", 
             sizeAlphaList( next_alpha_list ));
   
  /* Make a structure to hold a place to accumulate the solution
     statistics and initialize the global solution time and counters.
     Note this needs to be done just before starting the solution
     process so that we do not include spurious computations in the
     solution time. Also initializes the epoch to '0'. We want the
     iterations to end with this set to the last epoch computed.
     Thus, we increment this at the start of the loop, and need this
     to start at 0 to get '1' for the first epoch. */
  param->stat = newSolutionStats( param->report_file, 
                                  param->stat_summary );

  /* Set the epoch number to zero.  It gets incremented at start of
     loop so '1' will be the first epoch. */
  param->cur_epoch = 0;

  /* Ok, hold on now, 'cause here we go! */

  fprintf( param->report_file, "++++++++++++++++++++++++++++++++++++++++\n" );

  while(( gInterrupt == FALSE )
        && ( done == FALSE )) {

    /* Here we see if we want to save the previous solution to a
	  file. Skip doing this on the first epoch though. */
    if (( param->cur_epoch > 0 )
	   && ( param->opts->save_penultimate == Boolean_true ))
	 {
	   saveAlphaList( next_alpha_list, param->penultimate_filename ); 
	 }

    /* Some variations of value iteration need to adjust things at the
       startb of an iteration. This routine handles these things.
       This also increments the current epoch number. */
    startViEpoch( param );

    /* Delete the obs_source pointer array from next_alpha_list into
       prev_alpha_list since we are about to delete
       prev_alpha_list. The first time through the loop this will not
       do anything, since there will be no obs_source array for the
       initial alpha_list. */
    clearObsSourceAlphaList( next_alpha_list );
    
    /* Get rid of old value function, but make sure this is done
       after the policy graph information is extracted from
       next_alpha_list, since there are pointers from
       next_alpha_list into prev_alpha_list. First time through the
       loop this will be NULL. */
    if ( prev_alpha_list != NULL )
      destroyAlphaList( prev_alpha_list );

    /* We are at the top of the loop so now the next_alpha_list
       becomes the prev_alpha_list. */
    prev_alpha_list = next_alpha_list;
    next_alpha_list = NULL;

    /* Start the clock ticking on this epoch time and show some
       information indicating the epoch as started. */
    epochStartStats( param->stat );
    
    /* This is the heart of solution process: computing one value
       function from the other. */
    next_alpha_list = improveV( prev_alpha_list, param );

    /* Although the 'epsilon' precision value is used to determine
	  equality in an approximate manner, the solutions that are saved
	  and/or fed into the next value iteration epoch retain their
	  computed values.  Thus, two value that are considered the same
	  from an epilon-approximate viewpoint might actually have
	  different values from a machine-precision viewpoint, and thus
	  wehn used to seed the next iteration may not lead to the same
	  values.  This flag will force the alpha vector coef values to
	  be rounded in accordance with the 'epilson' value after each
	  iteration.
    */
    if ( param->opts->force_rounding == TRUE ) {
	 roundAlphaList( next_alpha_list, param->epsilon );
    }

    /* In case the -save_all option was selected, save the files. We
       need to do this *before* we do the ZLZ update, otherwise the
       policy graph and value functions might not make much sense. 

	  Note that with the -save_all option, we incur the sorting and
	  file I/O time inside the timing of each epoch.  Thus, one
	  should not use this if the timing will matter.  A future change
	  might fix this so this time does not get included. 
    */
    if ( param->save_all == TRUE ) {
	 sortAlphaList( next_alpha_list );
	 handleSaveAll( next_alpha_list, param );
    }

    /* The number of value function updates is not always the same as
       the number of value funtion epochs, mostly due to the ZLZ
       speedup. */
    (param->update_count)++;
    
    /* We will first check the stopping criteria, which will also
       compute the current error for the weak and bellman stopping
       criteria. In case we are using the ZLZ speedup, we use this
       error both to decide whether to do the speedup and for deciding
       when the terminate an inner ZLZ loop. */
    if ( meetStopCriteria( prev_alpha_list, 
                           next_alpha_list,
                           &cur_error,
                           param ) == TRUE )
      done = TRUE;
    
    /* This is a speedup by Zhang, Lee and Zhang for a UAI-99
       submission. It uses the witness points uncovered by the full DP
       update to do more updates of the value function. Note that we
       only consider doing this if the Bellman error is still too
       large to stop value iteration. */
    else if ( param->vi_variation == ZlzSpeedup )
      ZLZ_ViSpeedup( next_alpha_list, cur_error, param );

    /* Stop the clock for this epoch and show some end of epoch
       information */
    epochEndStats( param->stat, 
                   sizeAlphaList( next_alpha_list ),
                   cur_error );

    /* Check for finite horizon stopping condition. */
    if ( param->cur_epoch == param->horizon )
      done = TRUE;

    /* Adding to the alpha lists has an automatic numbering scheme
       as things are added.  This may or may not means the vectors
       will be numbered nicely.  This will just make sure there is a
       nice numbering and will leave the vector lexicographically
       sorted. 

	  Note that there are a number of places we sort the vectors.
	  This is somewhat redundant, but since an slready sorted list
	  requires little effort, this is not as bug a computational hit
	  as it might first appear to be. */
    sortAlphaList( next_alpha_list );

    /* We will always save the current alpha list to a file so that
       abnormal termination will leave the lastest epoch's
       solution.  This will allow you to start it with the last
       solution and not have to re-run it. */
    saveAlphaList( next_alpha_list, param->backup_file ); 

    /* If we are using a variation on value iteration, then we may
       need to adjust some parameters (epsilon) after an epoch.
       Either way we have to at least check whether parameters need to
       change. This function handles this case. */
    if ( ! done )
      endViEpoch( param );

  }  /* while( done == FALSE && gInterrupt = FALSE ) */
  
  gInterrupt = FALSE;
  
  /* Give some final tally information, such as solution location and
     time. Also write the solution files, but note that this must be
     done before we clear the prev_alpha_list to ensure policy graph
     information is preserved. */
  endPomdpSolve( param, next_alpha_list );
  
  /* Get rid of previous and next value functions.  Note that at this
     point there are obs_source pointers from next_alpha_list into
     prev_alpha_list, so free'ing prev_alpha_list first would leave
     pointers to nowhere.  Thus, we free them in the other order to
     show this dependency explicitly. */
  destroyAlphaList( next_alpha_list );
  destroyAlphaList( prev_alpha_list );

  /* We were writing out a backup file after each iteration, but now
     that we are done, remove it. */
  removeFile( param->backup_file );

}  /* solvePomdp */
/**********************************************************************/
