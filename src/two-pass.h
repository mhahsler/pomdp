
/*
 *<SOURCE_HEADER>
 *
 *  <NAME>
 *    two-pass.h
 *  </NAME>
 *  <AUTHOR>
 *    Anthony R. Cassandra
 *  </AUTHOR>
 *  <CREATE_DATE>
 *    July, 1998
 *  </CREATE_DATE>
 *
 *  <RCS_KEYWORD>
 *    $RCSfile: two-pass.h,v $
 *    $Source: /u/cvs/proj/pomdp-solve/src/two-pass.h,v $
 *    $Revision: 1.1 $
 *    $Date: 2003/05/13 21:46:40 $
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
#ifndef TWO_PASS_H
#define TWO_PASS_H

#include "params.h"

/*******************************************************************/
/**************      USEFUL MNENOMIC CONSTANTS      ****************/
/*******************************************************************/

/* These are the command line arguments that are valid.  To add one
   you need to increment NUM_WITNESS_ARGS, add a line to the
   WITNESS_ARGS list of strings. and finally add a constant of the
   form CMD_ARG_XXX with the new string. */
#define NUM_TWO_PASS_ARGS       0

#define UNMARKED              0
#define MARKED                1

/*******************************************************************/
/**************       EXTERNAL VARIABLES            ****************/
/*******************************************************************/


/*******************************************************************/
/**************       EXTERNAL FUNCTIONS            ****************/
/*******************************************************************/

extern void initTwoPass( );

extern void cleanUpTwoPass( );
  
/* The main two-pass algorithm routine for finding the Q-function
  represention for value iteration with POMDPs.  */
extern AlphaList improveTwoPass( AlphaList *projection,
                                 PomdpSolveParams param );
 
#endif


