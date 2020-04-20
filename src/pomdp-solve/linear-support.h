
/*
 *<SOURCE_HEADER>
 *
 *  <NAME>
 *    linear-support.h
 *  </NAME>
 *  <AUTHOR>
 *    Anthony R. Cassandra
 *  </AUTHOR>
 *  <CREATE_DATE>
 *    July, 1998
 *  </CREATE_DATE>
 *
 *  <RCS_KEYWORD>
 *    $RCSfile: linear-support.h,v $
 *    $Source: /u/cvs/proj/pomdp-solve/src/linear-support.h,v $
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

/*
 *  For Hsien-Te Cheng's Linear Support algorithm for POMDPs.
 */

#ifndef LINEAR_SUPPORT_H
#define LINEAR_SUPPORT_H

#include "params.h"

/**********************************************************************/
/********************       CONSTANTS       ***************************/
/**********************************************************************/

/* For the set gVertexSet, we will keep a flag that indicates
   which of the three sets it is in.  In Cheng's terminology,
   a checked vertex is one that is in set \tilde{E}.  We are able 
   to save space by not duplicating points in the three different 
   sets.  The deleted flag is used to mark vertices for deletion.
   This marking, then deleting makes the code more efficient and 
   easier to read, than if we incorporated the deltion and checking 
   for deletion in the same loop.
   */
#define  IN_SET_E          1
#define  IN_SET_E_TILDE    2
#define  IN_SET_C          4
#define  DELETED           256

/**********************************************************************/
/********************   DEFAULT VALUES       **************************/
/**********************************************************************/

/* This defines how good an approximation we can expect from Cheng's
   linear support algorithm.
   */
#define DEFAULT_CHENG_EPSILON         0.0000000001

/**********************************************************************/
/********************    TYPEDEFS            **************************/
/**********************************************************************/

/* Currently I will keep the values of Hv and err in the data structure,
   in hopes that the extra memory requirements will outweigh the extra
   computational requirements, if I decided to generate them as needed.
   */
typedef struct VertexListStruct *VertexList;
struct VertexListStruct {
  double *b;              /* the point */
  unsigned int flags;     /* IN_SET_E, IN_SET_E_TILDE, IN_SET_C */
  double Hv;              /* current Hv value */
  double err;             /* Current error of this point */
  VertexList next;
};

/**********************************************************************/
/********************   EXTERNAL VARIABLES   **************************/
/**********************************************************************/

/* Access this after a call to improveCheng() to see how many vertices
   were enumerated.
   */
extern int gNumVertices;

/**********************************************************************/
/********************   EXTERNAL FUNCTIONS    *************************/
/**********************************************************************/


/* The main incremental pruning algorithm routine for finding the
  Q-function represention for value iteration with POMDPs.  */
extern AlphaList improveLinSupport( AlphaList **projection,
                                    PomdpSolveParams param );

  
#endif
