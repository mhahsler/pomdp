
/*
 *<SOURCE_HEADER>
 *
 *  <NAME>
 *    pomdp-fg-options.h
 *  </NAME>
 *  <AUTHOR>
 *    Anthony R. Cassandra
 *  </AUTHOR>
 *  <CREATE_DATE>
 *    February 2005
 *  </CREATE_DATE>
 *
 *  <RCS_KEYWORD>
 *    $RCSfile: pomdp-fg-options.h,v $
 *    $Source: /u/cvs/proj/pomdp-solve/src/pomdp-fg-options.h,v $
 *    $Revision: 1.2 $
 *    $Date: 2005/02/04 15:10:22 $
 *    $Author: arc $

*  </RCS_KEYWORD>
 *
 *  <COPYRIGHT>
 *

 *    2005, Anthony R. Cassandra
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
 * This code was automatically generated on February 2005 by the program:
 *
 *     gen-program-opts.py
 */

#ifndef POMDP_FG_OPTS_OPTIONS_H
#define POMDP_FG_OPTS_OPTIONS_H

#include "global.h"

#include "program-options.h"

/*
 * Enumerated types for 'enum' parameters.
 */

typedef enum {
  POMDP_FG_OPTS_Grid_Type_simplex,
  POMDP_FG_OPTS_Grid_Type_pairwise,
  POMDP_FG_OPTS_Grid_Type_search,
  POMDP_FG_OPTS_Grid_Type_initial,
  POMDP_FG_OPTS_Grid_Type__END__
} POMDP_FG_OPTS_Grid_Type_Type;

/*
 * String arrays for 'enum' parameters.
 */

#define POMDP_FG_OPTS_OPT_GRID_TYPE_STRINGS { \
    "simplex", \
    "pairwise", \
    "search", \
    "initial", \
    "" \
  }

/*
 * Structure to hold all parameters.
 */
typedef struct PomdpFgProgOptionsStruct* PomdpFgProgOptions;
struct PomdpFgProgOptionsStruct {

  /* Executable name. */
  char __exec_name__[MAX_OPT_STRING_LEN];

  /* Executable version. */
  char __version__[MAX_OPT_STRING_LEN];

  /* Error flag for internal use only. */
  int __error__;

  /*
   *  This parameter allows you to set an upper bound on the
   * amount of time that this program will run. When this amount
   * of time has elapsed, the program execution is terminated.
   * Without specifying this parameter, there will be no upper
   * bound imposed by the pomdp-fg program.
   */
  int max_secs;

  /*
   *  The finite grid method needs a set of belief points to
   * compute over. There are a number of ways to generate this
   * grid, and this parameter selects the maximum number of
   * points that should be generated during this process.
   */
  int finite_grid_points;

  /*
   *  Value iteration assumes that at the end of the lifetime of
   * the decision maker that no more values will be accrued. This
   * corresponds to a terminal value function of all zeroes. This
   * is essentially the default starting point for the program.
   * However, with this parameter, you can set a different
   * terminal value function, which serves as the seed or initial
   * starting point for value iteration. Effectively, this allows
   * you to take the output of one value iteration run and send
   * it as input to the next. The file format for this input file
   * is identical to the output file format of this program (the
   * ".values" file).
   */
  char initial_values_filename[MAX_OPT_STRING_LEN];

  /*
   *  The pomdp-fg program normally only saves the last epoch's
   * solution to a file. There is an option (save_all) to save
   * every iteration, but that can require a lot of space for a
   * long horizon. There are times when it is useful to have the
   * current and immediately previous epoch' solutions. When this
   * flag is set, you will always find the previous epochs
   * solution in *.prev.alpha (where '*' is the prefix defined
   * for the execution run.
   */
  Boolean_Type save_penultimate;

  /*
   *  This parameter allows you to set an upper bound on the
   * amount of memory that this program uses. If the memory
   * threshold is met, the program execution is terminated.
   * Without specifying this parameter, there will be no upper
   * bound imposed by the pomdp-fg program (though the OS will
   * naturally have something to say about this).
   */
  int memory_limit;

  /*
   *  All executions for solution of a POMDP needs a file as
   * input to the solution process. This filename by convention
   * will end with with or ".pomdp" or ".POMDP" and needs to
   * conform to the pomdp-solve file format (which is described
   * elsewhere.)
   */
  char pomdp_filename[MAX_OPT_STRING_LEN];

  /*
   * What configuration file should be read.
   */
  char true[MAX_OPT_STRING_LEN];

  /*
   *  For any functionality that requires random numbers, we want
   * to be able to reproduce a given run by executing with the
   * same random number seed. This parameter allows you to set
   * the initial random seed by specifying a string consisting of
   * three integers separated by a colon (e.g.,
   * "34523:12987:50732" ) Not setting this value will result in
   * the random seed being pseudo-randomized based on the system
   * clock.
   */
  char rand_seed[MAX_OPT_STRING_LEN];

  /*
   *  Normally, only the final solution is saved to a file, but
   * if you would like to write out the solution to every epoch
   * of value iteration, then set this flag to true. The epoch
   * number will be appened to the filenames that are output.
   */
  Boolean_Type save_all;

  /*
   *  Sets the initial finite grid (belief points) to use by
   * reading them from a file.
   */
  char initial_grid_filename[MAX_OPT_STRING_LEN];

  /*
   *  All the information relevant to the solution of the POMDP
   * are written to files. This parameter allows you to set the
   * prefix to use for the given run of this program. The suffix
   * is generated internall by the program connected to the time
   * and contents of the various files.
   */
  char prefix_str[MAX_OPT_STRING_LEN];

  /*
   *  Value iteration is iterative and thus we may want to find
   * 'finite horizon' solutions for various reasons. To make
   * pomdp-fg terminate after a fixed number of iterations (aka
   * epochs) set this value to be some positive number. By
   * default, value iteration will run for as many iterations as
   * it take to 'converge' on the infinite horizon solution
   * (i.e., when the error gets small enough).
   */
  int horizon;

  /*
   *  The finite grid method needs a set of belief points to
   * compute over. This parameter will turn on and off the saving
   * of these belief points to an external file.
   */
  Boolean_Type finite_grid_save;

  /*
   *  The finite grid method needs a set of belief points to
   * compute over. There are a number of ways to generate this
   * grid, and this parameter selects the technique to use. We do
   * not yet here discuss the details of each of these.
   */
  POMDP_FG_OPTS_Grid_Type_Type finite_grid_type;

}; /* end PomdpFgProgOptionsProgOptionStruct */

/*
 * Default values for parameters.
 */
#define POMDP_FG_OPTS_OPT_MAX_GRID_POINTS_DEFAULT 10000
#define POMDP_FG_OPTS_OPT_SAVE_PENULTIMATE_DEFAULT Boolean_false
#define POMDP_FG_OPTS_OPT_SAVE_ALL_DEFAULT Boolean_false
#define POMDP_FG_OPTS_OPT_O_DEFAULT "solution"
#define POMDP_FG_OPTS_OPT_GRID_SAVE_DEFAULT Boolean_false
#define POMDP_FG_OPTS_OPT_GRID_TYPE_DEFAULT POMDP_FG_OPTS_Grid_Type_initial

/*
 * Strings for config file parameters.
 */
#define POMDP_FG_OPTS_CFG_TIME_LIMIT_STR "time_limit"
#define POMDP_FG_OPTS_CFG_MAX_GRID_POINTS_STR "max_grid_points"
#define POMDP_FG_OPTS_CFG_INITIAL_VALUES_STR "initial_values"
#define POMDP_FG_OPTS_CFG_SAVE_PENULTIMATE_STR "save_penultimate"
#define POMDP_FG_OPTS_CFG_MEMORY_LIMIT_STR "memory_limit"
#define POMDP_FG_OPTS_CFG_POMDP_STR "pomdp"
#define POMDP_FG_OPTS_CFG_F_STR "f"
#define POMDP_FG_OPTS_CFG_RAND_SEED_STR "rand_seed"
#define POMDP_FG_OPTS_CFG_SAVE_ALL_STR "save_all"
#define POMDP_FG_OPTS_CFG_INITIAL_GRID_STR "initial_grid"
#define POMDP_FG_OPTS_CFG_O_STR "o"
#define POMDP_FG_OPTS_CFG_HORIZON_STR "horizon"
#define POMDP_FG_OPTS_CFG_GRID_SAVE_STR "grid_save"
#define POMDP_FG_OPTS_CFG_GRID_TYPE_STR "grid_type"

/*
 * Strings for cmd line parameters.
 */
#define POMDP_FG_OPTS_ARG_TIME_LIMIT_STR "-time_limit"
#define POMDP_FG_OPTS_ARG_MAX_GRID_POINTS_STR "-max_grid_points"
#define POMDP_FG_OPTS_ARG_INITIAL_VALUES_STR "-initial_values"
#define POMDP_FG_OPTS_ARG_SAVE_PENULTIMATE_STR "-save_penultimate"
#define POMDP_FG_OPTS_ARG_MEMORY_LIMIT_STR "-memory_limit"
#define POMDP_FG_OPTS_ARG_POMDP_STR "-pomdp"
#define POMDP_FG_OPTS_ARG_F_STR "-f"
#define POMDP_FG_OPTS_ARG_RAND_SEED_STR "-rand_seed"
#define POMDP_FG_OPTS_ARG_SAVE_ALL_STR "-save_all"
#define POMDP_FG_OPTS_ARG_INITIAL_GRID_STR "-initial_grid"
#define POMDP_FG_OPTS_ARG_O_STR "-o"
#define POMDP_FG_OPTS_ARG_HORIZON_STR "-horizon"
#define POMDP_FG_OPTS_ARG_GRID_SAVE_STR "-grid_save"
#define POMDP_FG_OPTS_ARG_GRID_TYPE_STR "-grid_type"

/*
 * String arrays for cmd line parameters.
 */

extern char* POMDP_FG_OPTS_Grid_Type_Str[];

/*
 * Function prototyeps.
 */

extern PomdpFgProgOptions
POMDP_FG_OPTS_new( );

extern void
POMDP_FG_OPTS_delete( PomdpFgProgOptions );

extern ConfigFile
POMDP_FG_OPTS_toConfigFile( PomdpFgProgOptions );

extern void 
POMDP_FG_OPTS_showUsageBrief( FILE*, char* );

extern void 
POMDP_FG_OPTS_showUsage( FILE*, char* );

extern PomdpFgProgOptions
POMDP_FG_OPTS_parse( ProgramOptions );

extern PomdpFgProgOptions
POMDP_FG_OPTS_create( int, char** );

#endif
/* end header file */
