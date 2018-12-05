/*
 *<SOURCE_HEADER>
 *
 *  <NAME>
 *    params.c
 *  </NAME>
 *  <AUTHOR>
 *    Anthony R. Cassandra
 *  </AUTHOR>
 *  <CREATE_DATE>
 *    July, 1998
 *  </CREATE_DATE>
 *
 *  <RCS_KEYWORD>
 *    $RCSfile: fg-params.c,v $
 *    $Source: /u/cvs/proj/pomdp-solve/src/fg-params.c,v $
 *    $Revision: 1.1 $
 *    $Date: 2005/02/03 05:59:15 $
 *  </RCS_KEYWORD>
 *
 *  <COPYRIGHT>
 *
 *    2005 Anthony R. Cassandra
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
 *   Stuff to specify all POMDP solution parameters.
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "global.h"
#include "timing.h"
#include "random.h"
#include "pomdp.h"
#include "fg-params.h"

/**********************************************************************/
FiniteGridParams 
FGP_new(  ) 
{
  /*
    Creates the memory for the structure to hold the parameters used in
    solving a POMDP using a finite grid.  Also sets the fields to the
    default values.  
  */
  FiniteGridParams params;
  int i;

  params = (FiniteGridParams) XMALLOC( sizeof( *params ));

  /* We do not allocate the memory for this structure.  We just ensure
	it starts with a sane NULL value.  We will clean this up if it is
	non-null in the destructor.
  */

  params->opts = NULL;

  params->cur_epoch = 0;
  params->report_file = stdout;

  params->values_filename[0] = NULL_CHAR;
  params->policy_filename[0] = NULL_CHAR;
  params->grid_filename[0] = NULL_CHAR;

  params->initial_values = NULL;
  params->initial_grid = NULL;

  params->max_secs = 0;
  params->memory_limit = 0;
  params->save_all = FALSE;
  params->backup_file[0] = NULL_CHAR;
  params->penultimate_filename[0] = NULL_CHAR;

  return ( params );

}  /* FGP_new */

/**********************************************************************/
void 
FGP_destroy( FiniteGridParams params ) 
{

  if ( params->opts != NULL )
    POMDP_FG_OPTS_delete( params->opts );
   
  if ( params->initial_values != NULL )
    VF_destroy( params->initial_values );

  if ( params->initial_grid != NULL )
    BS_destroyGrid( params->initial_grid );

  XFREE( params );

}  /* FGP_destroy */

/**********************************************************************/
void
FGP_doPreParseActions( ) {
  /*
    The very first routine that is called. Put anything that needs to
    happen before parsing the command line in this routine. 
   */

  /****************/
  /* I used to initialize this variable during its declaration, but I
	found that when updating to pomdp-solve version 4.1, this was no
	longer allowed. Thus, I do it here first thing. */
  gStdErrFile = stderr;

} /* FGP_doPreParseActions */

/**********************************************************************/
void
FGP_doPostParseActions( FiniteGridParams params ) {
  /*
    Does more customized checking of the options to the program.
  */
  PomdpFgProgOptions opts;
  char tmp_str[MAX_OPT_STRING_LEN];
  int idx;

  /* just for convenience within this routine */
  opts = params->opts;

  /* Must have a POMDP filename or else things ill not work. */
  if (( opts->pomdp_filename == NULL )
	 || ( opts->pomdp_filename[0] == '\0' ))
    {
	 fprintf( stderr, "No POMDP file specified. Use '-h' for help.\n" );
	 exit( 1 );
    }

  /****************/
   /* First see if a random number seed is given, and if it is set the
      seed to this value. */
  if( opts->rand_seed[0] != '\0' )
    setRandomSeedFromString( opts->rand_seed );

  /* Otherwise initialize the random number generator with
	psuedo-random seed. */
  else
    randomize();

  /****************/
  /* Try to make the prefix be the prefix of the POMDP file if the
	default is chosen. */
  if ( strcmp( opts->prefix_str, POMDP_FG_OPTS_OPT_O_DEFAULT ) == 0 ) {

    strcpy( tmp_str, opts->pomdp_filename );

    /* This will point to null term at first */
    idx = strlen( tmp_str );  

    /* Start at the end and move left until we see the first
	  "period". */
    while (( idx > 0 ) && ( tmp_str[idx] != '.'))
	 idx--;

    /* Only override if we found a period in param filename */
    if ( idx > 0 ) {

	 /* Null terminate at the period */
	 tmp_str[idx] = '\0';

	 sprintf( opts->prefix_str, "%s-%d", tmp_str, getPid() );
	 
    } /* if we can override the default */

  } /* if default prefix is being used */

} /* FGP_doPostParseActions */

/**********************************************************************/
FiniteGridParams 
FGP_parse( int argc, char **argv ) 
{
  /*
    Parses the pomdp-fg command line, setting variables for the
    different settings.  The actual actions taken because of these
    settings are done later.

    Returns the structure with all this information, or exits the
    program iwth an error message.  
  */
  FiniteGridParams params;

  params = FGP_new();

  /* Put anything that needs to happen before parsing the command line
	in this routine. */   
  FGP_doPreParseActions();

   /* Parse command line to check for validity and to extract all the
	 things we need. The program will exit and/or print a message
	 indicating problems with the command line parsing, so we need
	 not take any action here. */ 
  params->opts = POMDP_FG_OPTS_create( argc, argv );

  /* Customized option handling for this program. */
  FGP_doPostParseActions( params );

  return ( params );

}  /* FGP_parse */

/************************************************************/
void
FGP_show( FiniteGridParams params ) 
{
  ConfigFile cfg;

  fprintf( params->report_file, 
		 " //****************\\\\\n" );
  fprintf( params->report_file, 
		 "||    %s      ||\n", params->opts->__exec_name__ );
  fprintf( params->report_file, 
		 "||     v. %s       ||\n", params->opts->__version__ );
  fprintf( params->report_file, 
		 " \\\\****************//\n" );
  fprintf( params->report_file, 
		 "      PID=%d\n", getpid() );

  cfg = POMDP_FG_OPTS_toConfigFile( params->opts );

  fprintf( params->report_file, 
		 "- - - - - - - - - - - - - - - - - - - -\n" );

  CF_fprintf( cfg, params->report_file );
 
  fprintf( params->report_file, 
		 "- - - - - - - - - - - - - - - - - - - -\n" );

  CF_delete( cfg );

} /* FGP_show */
