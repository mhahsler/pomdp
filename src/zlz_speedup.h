
/*
 *<SOURCE_HEADER>
 *
 *  <NAME>
 *    zlz_speedup.h
 *  </NAME>
 *  <AUTHOR>
 *    Anthony R. Cassandra
 *  </AUTHOR>
 *  <CREATE_DATE>
 *    
 *  </CREATE_DATE>
 *
 *  <RCS_KEYWORD>
 *    $RCSfile: zlz_speedup.h,v $
 *    $Source: /u/cvs/proj/pomdp-solve/src/zlz_speedup.h,v $
 *    $Revision: 1.1 $
 *    $Date: 2003/05/13 21:46:41 $
 *  </RCS_KEYWORD>
 *
 *  <COPYRIGHT>
 *
 *    1999-2003, Anthony R. Cassandra
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
#ifndef ZLZ_SPEEDUP_H
#define ZLZ_SPEEDUP_H

/**********************************************************************/
/********************   DEFAULT VALUES       **************************/
/**********************************************************************/

/* This is the value cited in the ppaer and used as part of the
   stopping criteria for the improve() routine. */
#define DEFAULT_EPSILON1_VALUE             0.001

/**********************************************************************/
/********************   EXTERNAL FUNCTIONS    *************************/
/**********************************************************************/

/* This is the routine that implements the portion of the VI1
  algorithm *after* the regular DP update.  The U_n set is the last
  computed value funciton (output of the DP update).  The 'delta'
  parametre is the current Bellman residual of the new value function
  and the previous value funciton as computed by the bellmanError()
  routine.  */
extern void ZLZ_ViSpeedup( AlphaList U_n,
                           double delta,
                           PomdpSolveParams param );

#endif
