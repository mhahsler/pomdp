
/*
 *<SOURCE_HEADER>
 *
 *  <NAME>
 *    pomdp-tools-options.h
 *  </NAME>
 *  <AUTHOR>
 *    Anthony R. Cassandra
 *  </AUTHOR>
 *  <CREATE_DATE>
 *    February 2005
 *  </CREATE_DATE>
 *
 *  <RCS_KEYWORD>
 *    $RCSfile: pomdp-tools-options.h,v $
 *    $Source: /u/cvs/proj/pomdp-solve/src/pomdp-tools-options.h,v $
 *    $Revision: 1.9 $
 *    $Date: 2005/10/30 23:21:17 $
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

#ifndef POMDP_TOOLS_OPTS_OPTIONS_H
#define POMDP_TOOLS_OPTS_OPTIONS_H

#include "global.h"

#include "program-options.h"

/*
 * Enumerated types for 'enum' parameters.
 */

typedef enum {
  POMDP_TOOLS_OPTS_Tool_update_beliefs,
  POMDP_TOOLS_OPTS_Tool_compare_alphas,
  POMDP_TOOLS_OPTS_Tool_map_beliefs,
  POMDP_TOOLS_OPTS_Tool_pg_isomorphism,
  POMDP_TOOLS_OPTS_Tool_purge_alphas,
  POMDP_TOOLS_OPTS_Tool_sort_alphas,
  POMDP_TOOLS_OPTS_Tool_pg_eval,
  POMDP_TOOLS_OPTS_Tool_pg_relink,
  POMDP_TOOLS_OPTS_Tool_belief_alpha_compare,
  POMDP_TOOLS_OPTS_Tool_none,
  POMDP_TOOLS_OPTS_Tool__END__
} POMDP_TOOLS_OPTS_Tool_Type;

/*
 * String arrays for 'enum' parameters.
 */

#define POMDP_TOOLS_OPTS_OPT_TOOL_STRINGS { \
    "update_beliefs", \
    "compare_alphas", \
    "map_beliefs", \
    "pg_isomorphism", \
    "purge_alphas", \
    "sort_alphas", \
    "pg_eval", \
    "pg_relink", \
    "belief_alpha_compare", \
    "none", \
    "" \
  }

/*
 * Structure to hold all parameters.
 */
typedef struct PomdpToolsProgOptionsStruct* PomdpToolsProgOptions;
struct PomdpToolsProgOptionsStruct {

  /* Executable name. */
  char __exec_name__[MAX_OPT_STRING_LEN];

  /* Executable version. */
  char __version__[MAX_OPT_STRING_LEN];

  /* Error flag for internal use only. */
  int __error__;

  /*
   *  Some pomdp tools require one or more 'alpha' files to do
   * their work. When it does, this is the name of the 'second'
   * such file, where 'second' will be defined in a tool-specific
   * manner. An 'alpha' file is the pomdp-solve representation of
   * the value function over belief space.
   */
  char alpha2[MAX_OPT_STRING_LEN];

  /*
   *  Some pomdp tools require one or more 'alpha' files to do
   * their work. When it does, this is the name of the 'first'
   * such file, where 'first' will be defined in a tool-specific
   * manner. An 'alpha' file is the pomdp-solve representation of
   * the value function over belief space.
   */
  char alpha1[MAX_OPT_STRING_LEN];

  /*
   *  Some pomdp tools require one or more belief state list
   * files to do their work. When it does, this is the name of
   * the 'first' such file, where 'first' will be defined in a
   * tool-specific manner.
   */
  char belief[MAX_OPT_STRING_LEN];

  /*
   * What configuration file should be read.
   */
  char true[MAX_OPT_STRING_LEN];

  /*
   *  For some of the tools it needs the POMDP model file.
   */
  char pomdp[MAX_OPT_STRING_LEN];

  /*
   *  This program collects miscellaneous tools to help with
   * pomdp-solve input/output manipulation and interpretation. As
   * such, you need to seelct which of these tools you need to
   * use.
   */
  POMDP_TOOLS_OPTS_Tool_Type tool;

  /*
   *  This is the main precision setting parameter which will
   * effect the preciseness fo the tools when employing numerical
   * comparisons on floating point numbers.
   */
  double epsilon;

  /*
   *  Tells the tool how many states are in the POMDP model for
   * which we are aplying the tool. This saves from having to
   * consult the original POMDP file, since many of the tool
   * operations need know only this piece of information.
   */
  int states;

  /*
   *  Some pomdp tools require one or more policy graph files to
   * do their work. When it does, this is the name of the
   * 'second' such policy graph, where 'second' will be defined
   * in a tool-specific manner.
   */
  char pg2[MAX_OPT_STRING_LEN];

  /*
   *  Determines the file to which the output result should go.
   * The exact nature of the output is tool specific.
   */
  char result_filename[MAX_OPT_STRING_LEN];

  /*
   *  Some pomdp tools require one or more policy graph files to
   * do their work. When it does, this is the name of the 'first'
   * such policy graph, where 'first' will be defined in a
   * tool-specific manner.
   */
  char pg1[MAX_OPT_STRING_LEN];

  /*
   *  Tells the tool how many states are in the POMDP model for
   * which we are aplying the tool. This saves from having to
   * consult the original POMDP file, since many of the tool
   * operations need know only this piece of information.
   */
  int obs;

}; /* end PomdpToolsProgOptionsProgOptionStruct */

/*
 * Default values for parameters.
 */
#define POMDP_TOOLS_OPTS_OPT_TOOL_DEFAULT POMDP_TOOLS_OPTS_Tool_none
#define POMDP_TOOLS_OPTS_OPT_EPSILON_DEFAULT 1e-9
#define POMDP_TOOLS_OPTS_OPT_O_DEFAULT "toolsoln"

/*
 * Strings for config file parameters.
 */
#define POMDP_TOOLS_OPTS_CFG_ALPHA2_STR "alpha2"
#define POMDP_TOOLS_OPTS_CFG_ALPHA1_STR "alpha1"
#define POMDP_TOOLS_OPTS_CFG_BELIEF_STR "belief"
#define POMDP_TOOLS_OPTS_CFG_F_STR "f"
#define POMDP_TOOLS_OPTS_CFG_POMDP_STR "pomdp"
#define POMDP_TOOLS_OPTS_CFG_TOOL_STR "tool"
#define POMDP_TOOLS_OPTS_CFG_EPSILON_STR "epsilon"
#define POMDP_TOOLS_OPTS_CFG_STATES_STR "states"
#define POMDP_TOOLS_OPTS_CFG_PG2_STR "pg2"
#define POMDP_TOOLS_OPTS_CFG_O_STR "o"
#define POMDP_TOOLS_OPTS_CFG_PG1_STR "pg1"
#define POMDP_TOOLS_OPTS_CFG_OBS_STR "obs"

/*
 * Strings for cmd line parameters.
 */
#define POMDP_TOOLS_OPTS_ARG_ALPHA2_STR "-alpha2"
#define POMDP_TOOLS_OPTS_ARG_ALPHA1_STR "-alpha1"
#define POMDP_TOOLS_OPTS_ARG_BELIEF_STR "-belief"
#define POMDP_TOOLS_OPTS_ARG_F_STR "-f"
#define POMDP_TOOLS_OPTS_ARG_POMDP_STR "-pomdp"
#define POMDP_TOOLS_OPTS_ARG_TOOL_STR "-tool"
#define POMDP_TOOLS_OPTS_ARG_EPSILON_STR "-epsilon"
#define POMDP_TOOLS_OPTS_ARG_STATES_STR "-states"
#define POMDP_TOOLS_OPTS_ARG_PG2_STR "-pg2"
#define POMDP_TOOLS_OPTS_ARG_O_STR "-o"
#define POMDP_TOOLS_OPTS_ARG_PG1_STR "-pg1"
#define POMDP_TOOLS_OPTS_ARG_OBS_STR "-obs"

/*
 * String arrays for cmd line parameters.
 */

extern char* POMDP_TOOLS_OPTS_Tool_Str[];

/*
 * Function prototyeps.
 */

extern PomdpToolsProgOptions
POMDP_TOOLS_OPTS_new( );

extern void
POMDP_TOOLS_OPTS_delete( PomdpToolsProgOptions );

extern ConfigFile
POMDP_TOOLS_OPTS_toConfigFile( PomdpToolsProgOptions );

extern void 
POMDP_TOOLS_OPTS_showUsageBrief( FILE*, char* );

extern void 
POMDP_TOOLS_OPTS_showUsage( FILE*, char* );

extern PomdpToolsProgOptions
POMDP_TOOLS_OPTS_parse( ProgramOptions );

extern PomdpToolsProgOptions
POMDP_TOOLS_OPTS_create( int, char** );

#endif
/* end header file */
