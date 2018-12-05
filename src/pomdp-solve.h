
/*
 *<SOURCE_HEADER>
 *
 *  <NAME>
 *    pomdp-solve.h
 *  </NAME>
 *  <AUTHOR>
 *    Anthony R. Cassandra
 *  </AUTHOR>
 *  <CREATE_DATE>
 *    July, 1998
 *  </CREATE_DATE>
 *
 *  <RCS_KEYWORD>
 *    $RCSfile: pomdp-solve.h,v $
 *    $Source: /u/cvs/proj/pomdp-solve/src/pomdp-solve.h,v $
 *    $Revision: 1.2 $
 *    $Date: 2004/03/08 08:33:39 $
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
#ifndef POMDP_SOLVE_H
#define POMDP_SOLVE_H

/*******************************************************************/
/**************             CONSTANTS               ****************/
/*******************************************************************/

/*******************************************************************/
/**************             TYPEDEFS                ****************/
/*******************************************************************/

/*******************************************************************/
/**************       EXTERNAL VARIABLES            ****************/
/*******************************************************************/

/*******************************************************************/
/**************       EXTERNAL FUNCTIONS            ****************/
/*******************************************************************/

extern void showUsagePomdpSolve( FILE *file );

extern void initPomdpSolve( PomdpSolveParams param );

extern void cleanUpPomdpSolve( PomdpSolveParams param );

extern void solvePomdp( PomdpSolveParams param );

/* For now our default policy is just all zeroes.  */
extern AlphaList getDefaultInitialPolicy( );

/* Some algorithms will solve one iteration of POMDP value iteration
   by breaking the problem into a separate one for each action.  This
   routine will implement the basic structure needed and call the
   appropriate routines depending on the specific algorithm being
   used.  Current algorithms that do it this way: TwoPass, Witness and
   IncrementalPruning */
extern AlphaList improveByQ( AlphaList **projection,
                             PomdpSolveParams param );
/* This does a single DP step of value iteration for a POMDP.  It
   takes in the previous value function and parameters for solving and
   returns the next or improved solution.  */
extern AlphaList improveV( AlphaList prev_alpha_list,
                           PomdpSolveParams param );


#endif
