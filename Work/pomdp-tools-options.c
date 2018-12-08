
/*
 *<SOURCE_HEADER>
 *
 *  <NAME>
 *    pomdp-tools-options.c
 *  </NAME>
 *  <AUTHOR>
 *    Anthony R. Cassandra
 *  </AUTHOR>
 *  <CREATE_DATE>
 *    February 2005
 *  </CREATE_DATE>
 *
 *  <RCS_KEYWORD>
 *    $RCSfile: pomdp-tools-options.c,v $
 *    $Source: /u/cvs/proj/pomdp-solve/src/pomdp-tools-options.c,v $
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <limits.h>

#include "pomdp-tools-options.h"

/*
 * Strings arrays for parameters.
 */

char* POMDP_TOOLS_OPTS_Tool_Str[] = POMDP_TOOLS_OPTS_OPT_TOOL_STRINGS;

/*******************************************************/
PomdpToolsProgOptions
POMDP_TOOLS_OPTS_new( )
{

  PomdpToolsProgOptions options;

  options = (PomdpToolsProgOptions) XMALLOC( sizeof( *options ));

  strcpy( options->__exec_name__, "pomdp-tools" );

  strcpy( options->__version__, "1.0" );

  options->__error__ = 0;

  options->alpha2[0] = '\0';
  options->alpha1[0] = '\0';
  options->belief[0] = '\0';
  options->true[0] = '\0';
  options->pomdp[0] = '\0';
  options->tool = POMDP_TOOLS_OPTS_OPT_TOOL_DEFAULT;
  options->epsilon = POMDP_TOOLS_OPTS_OPT_EPSILON_DEFAULT;
  options->states = 0;
  options->pg2[0] = '\0';
  strcpy( options->result_filename, POMDP_TOOLS_OPTS_OPT_O_DEFAULT );
  options->pg1[0] = '\0';
  options->obs = 0;

  return( options );

}  /* POMDP_TOOLS_OPTS_new */

/*******************************************************/
void
POMDP_TOOLS_OPTS_delete( PomdpToolsProgOptions options )
{

  XFREE( options );

}  /* POMDP_TOOLS_OPTS_delete */

/*******************************************************/
ConfigFile
POMDP_TOOLS_OPTS_toConfigFile( PomdpToolsProgOptions options )
{
  ConfigFile cfg;
  char str[1024];

  cfg = CF_new();

  sprintf( str, "%s", options->alpha2 );
  CF_addParam( cfg, POMDP_TOOLS_OPTS_CFG_ALPHA2_STR, str );

  sprintf( str, "%s", options->alpha1 );
  CF_addParam( cfg, POMDP_TOOLS_OPTS_CFG_ALPHA1_STR, str );

  sprintf( str, "%s", options->belief );
  CF_addParam( cfg, POMDP_TOOLS_OPTS_CFG_BELIEF_STR, str );

  sprintf( str, "%s", options->true );
  CF_addParam( cfg, POMDP_TOOLS_OPTS_CFG_F_STR, str );

  sprintf( str, "%s", options->pomdp );
  CF_addParam( cfg, POMDP_TOOLS_OPTS_CFG_POMDP_STR, str );

  sprintf( str, "%s", POMDP_TOOLS_OPTS_Tool_Str[options->tool] );
  CF_addParam( cfg, POMDP_TOOLS_OPTS_CFG_TOOL_STR, str );

  sprintf( str, "%.6f", options->epsilon );
  CF_addParam( cfg, POMDP_TOOLS_OPTS_CFG_EPSILON_STR, str );

  sprintf( str, "%d", options->states );
  CF_addParam( cfg, POMDP_TOOLS_OPTS_CFG_STATES_STR, str );

  sprintf( str, "%s", options->pg2 );
  CF_addParam( cfg, POMDP_TOOLS_OPTS_CFG_PG2_STR, str );

  sprintf( str, "%s", options->result_filename );
  CF_addParam( cfg, POMDP_TOOLS_OPTS_CFG_O_STR, str );

  sprintf( str, "%s", options->pg1 );
  CF_addParam( cfg, POMDP_TOOLS_OPTS_CFG_PG1_STR, str );

  sprintf( str, "%d", options->obs );
  CF_addParam( cfg, POMDP_TOOLS_OPTS_CFG_OBS_STR, str );

  return cfg;

} /* POMDP_TOOLS_OPTS_toConfigFile */

/*******************************************************/
void 
POMDP_TOOLS_OPTS_showUsageBrief( FILE* file, char* exec_name )
{
  fprintf( file, "Usage: %s [opts...] [args...]\n", exec_name );
  fprintf( file, "Use '-h' for help.\n");

}  /* POMDP_TOOLS_OPTS_showUsageBrief */

/*******************************************************/
void 
POMDP_TOOLS_OPTS_showUsage( FILE* file, char* exec_name )
{
  fprintf( file, "Usage: %s [opts...] [args...]\n", exec_name );

  /*******************************/
  /* __MAIN__ parameters  */
  /*******************************/
  fprintf( file, "General options:\n" );

  fprintf( file, "\t%s <string>\n", POMDP_TOOLS_OPTS_ARG_ALPHA2_STR );
  fprintf( file, "\t%s <string>\n", POMDP_TOOLS_OPTS_ARG_ALPHA1_STR );
  fprintf( file, "\t%s <string>\n", POMDP_TOOLS_OPTS_ARG_BELIEF_STR );
  fprintf( file, "\t%s <string>\n", POMDP_TOOLS_OPTS_ARG_F_STR );
  fprintf( file, "\t%s <string>\n", POMDP_TOOLS_OPTS_ARG_POMDP_STR );
  PO_showUsageEnumType( file,
                     POMDP_TOOLS_OPTS_ARG_TOOL_STR,
                     POMDP_TOOLS_OPTS_Tool_Str );
  fprintf( file, "\t%s <double>\n", POMDP_TOOLS_OPTS_ARG_EPSILON_STR );
  fprintf( file, "\t%s <int>\n", POMDP_TOOLS_OPTS_ARG_STATES_STR );
  fprintf( file, "\t%s <string>\n", POMDP_TOOLS_OPTS_ARG_PG2_STR );
  fprintf( file, "\t%s <string>\n", POMDP_TOOLS_OPTS_ARG_O_STR );
  fprintf( file, "\t%s <string>\n", POMDP_TOOLS_OPTS_ARG_PG1_STR );
  fprintf( file, "\t%s <int>\n", POMDP_TOOLS_OPTS_ARG_OBS_STR );

}  /* POMDP_TOOLS_OPTS_showUsage */

/*******************************************************/
PomdpToolsProgOptions
POMDP_TOOLS_OPTS_parse( ProgramOptions opts )
{
  PomdpToolsProgOptions options;
  int enum_idx;
  int ret_value;

  options = POMDP_TOOLS_OPTS_new();

  PO_startValidate( opts );

  ret_value = PO_getStringOption( opts,
                         POMDP_TOOLS_OPTS_ARG_ALPHA2_STR,
                         options->alpha2,
                         NULL,
                         NULL );
  if ( ret_value == PO_OPT_PRESENT_ERROR )
    PO_handleError( opts, "Option 'alpha2' has invalid value." );

  ret_value = PO_getStringOption( opts,
                         POMDP_TOOLS_OPTS_ARG_ALPHA1_STR,
                         options->alpha1,
                         NULL,
                         NULL );
  if ( ret_value == PO_OPT_PRESENT_ERROR )
    PO_handleError( opts, "Option 'alpha1' has invalid value." );

  ret_value = PO_getStringOption( opts,
                         POMDP_TOOLS_OPTS_ARG_BELIEF_STR,
                         options->belief,
                         NULL,
                         NULL );
  if ( ret_value == PO_OPT_PRESENT_ERROR )
    PO_handleError( opts, "Option 'belief' has invalid value." );

  ret_value = PO_getStringOption( opts,
                         POMDP_TOOLS_OPTS_ARG_F_STR,
                         options->true,
                         NULL,
                         NULL );
  if ( ret_value == PO_OPT_PRESENT_ERROR )
    PO_handleError( opts, "Option 'true' has invalid value." );

  ret_value = PO_getStringOption( opts,
                         POMDP_TOOLS_OPTS_ARG_POMDP_STR,
                         options->pomdp,
                         NULL,
                         NULL );
  if ( ret_value == PO_OPT_PRESENT_ERROR )
    PO_handleError( opts, "Option 'pomdp' has invalid value." );

  ret_value = PO_getEnumOption( opts,
                         POMDP_TOOLS_OPTS_ARG_TOOL_STR,
                         &(enum_idx),
                         POMDP_TOOLS_OPTS_Tool_Str );
  if ( ret_value == PO_OPT_PRESENT_VALID )
    options->tool = enum_idx;
  if ( ret_value == PO_OPT_PRESENT_ERROR )
    PO_handleError( opts, "Option 'tool' has invalid value." );

  ret_value = PO_getDoubleOption( opts,
                            POMDP_TOOLS_OPTS_ARG_EPSILON_STR,
               &(options->epsilon),
                           0,
                           HUGE_VAL );
  if ( ret_value == PO_OPT_PRESENT_ERROR )
    PO_handleError( opts, "Option 'epsilon' has invalid value." );

  ret_value = PO_getIntegerOption( opts,
                           POMDP_TOOLS_OPTS_ARG_STATES_STR,
                            &(options->states),
                           1,
                           INT_MAX );
  if ( ret_value == PO_OPT_PRESENT_ERROR )
    PO_handleError( opts, "Option 'states' has invalid value." );

  ret_value = PO_getStringOption( opts,
                         POMDP_TOOLS_OPTS_ARG_PG2_STR,
                         options->pg2,
                         NULL,
                         NULL );
  if ( ret_value == PO_OPT_PRESENT_ERROR )
    PO_handleError( opts, "Option 'pg2' has invalid value." );

  ret_value = PO_getStringOption( opts,
                         POMDP_TOOLS_OPTS_ARG_O_STR,
                         options->result_filename,
                         NULL,
                         NULL );
  if ( ret_value == PO_OPT_PRESENT_ERROR )
    PO_handleError( opts, "Option 'result_filename' has invalid value." );

  ret_value = PO_getStringOption( opts,
                         POMDP_TOOLS_OPTS_ARG_PG1_STR,
                         options->pg1,
                         NULL,
                         NULL );
  if ( ret_value == PO_OPT_PRESENT_ERROR )
    PO_handleError( opts, "Option 'pg1' has invalid value." );

  ret_value = PO_getIntegerOption( opts,
                           POMDP_TOOLS_OPTS_ARG_OBS_STR,
                            &(options->obs),
                           1,
                           INT_MAX );
  if ( ret_value == PO_OPT_PRESENT_ERROR )
    PO_handleError( opts, "Option 'obs' has invalid value." );

  PO_endValidate( opts );

  if( ! PO_isValid( opts ))
    options->__error__ = 1;
  return options;

}  /* POMDP_TOOLS_OPTS_parse */

/*******************************************************/
PomdpToolsProgOptions
POMDP_TOOLS_OPTS_create( int argc, char** argv )
{
  PomdpToolsProgOptions options;
  ProgramOptions opts;
  ConfigFile cfg;

  opts = PO_create( argc, argv );

  if ( opts->usage )
    {
      POMDP_TOOLS_OPTS_showUsage( stdout,
          opts->cmd_line->exec_name );
      PO_delete( opts );
      exit( 1 );
    }

  if ( ! PO_isValid( opts ))
    {
      POMDP_TOOLS_OPTS_showUsageBrief( stdout, 
          opts->cmd_line->exec_name );
      PO_delete( opts );
      exit( 1 );
    }

  options = POMDP_TOOLS_OPTS_parse( opts );

  if ( options->__error__ )
    {
      POMDP_TOOLS_OPTS_showUsageBrief( stdout, 
          opts->cmd_line->exec_name );
      PO_delete( opts );

      POMDP_TOOLS_OPTS_delete( options );
      exit( 1 );
    }

  PO_delete( opts );

  return options;

} /* POMDP_TOOLS_OPTS_create */

/* end C code */
