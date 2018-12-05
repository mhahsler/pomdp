/*
 *<SOURCE_HEADER>
 *
 *  <NAME>
 *    main.c
 *  </NAME>
 *  <AUTHOR>
 *    Anthony R. Cassandra
 *  </AUTHOR>
 *  <CREATE_DATE>
 *    August, 1998
 *  </CREATE_DATE>
 *
 *  <RCS_KEYWORD>
 *    $RCSfile: pomdp-fg-main.c,v $
 *    $Source: /u/cvs/proj/pomdp-solve/src/pomdp-fg-main.c,v $
 *    $Revision: 1.4 $
 *    $Date: 2005/02/22 22:10:36 $
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
 *   This file contains the main routine for the pomdp-fg program.
 *   
 *   The command line arguments are shown by running:
 * 
 *         pomdp-fg -h
 */

#define POMDP_FG_MAIN_C

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "mdp/mdp.h"

/*
#include "global.h"
#include "signal-handler.h"
#include "cmd-line.h"
#include "pomdp.h"
#include "alpha.h"
#include "belief.h"
#include "stats.h"
#include "random.h"
#include "pg.h"
#include "projection.h"
#include "enumeration.h"
#include "linear-support.h"
#include "two-pass.h"
#include "witness.h"
#include "inc-prune.h"
#include "pomdp-solve.h"
*/

#include "fg-params.h"
#include "pomdp-fg.h"

/**********************************************************************/
int 
main( int argc, char **argv ) {

  FiniteGridParams params;

  /* Handles configuration file and command line options. */
  params = FGP_parse( argc, argv );

  /* To document what is actually being executed we dump out
     all the parameters of the execution. */
  FGP_show( params );

  /* Do any file reading and memory allocations that are required. */
  FG_initialize( params );
  
  /* And away we go... */
  FG_solve( params );

  /* undo any file/memory stuff that initilization did. */
  FG_finalize( params );

  /* Let's make all nice afterwards for all other things. Note that
     this deallocates the space for 'param' and its 'opts'
     component. */
  FGP_destroy( params );

  return( 0 );

} /* main */

/**********************************************************************/
