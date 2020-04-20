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
 *    $RCSfile: belief-state.h,v $
 *    $Source: /u/cvs/proj/pomdp-solve/src/belief-state.h,v $
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
 *  Representations and methods for a grid of belief points.
 */

#ifndef BELIEF_GRID_H
#define BELIEF_GRID_H

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "double-vector.h"

/*******************************************************************/
/**************      USEFUL MNENOMIC CONSTANTS      ****************/
/*******************************************************************/

/*******************************************************************/
/**************             TYPEDEFS                ****************/
/*******************************************************************/

/* Sometimes we just need a belief state, which we represent simply
   as a DoubleVector. */  
typedef DoubleVector BeliefState;

typedef DoubleVectorList BeliefStateList;

/* A belief grid is a wrapper around the double vector list structure
   that adds its own attributes. */

typedef DoubleVectorList BeliefGrid;

/* The attributes for individual belief points (nodes in the list) */
typedef struct BeliefGridNodeAttrStruct*  BeliefGridNodeAttr;
struct BeliefGridNodeAttrStruct 
{
  DoubleVectorList facets;

};

/* The attributes for the lists themselves. */

typedef struct BeliefGridAttrStruct*  BeliefGridAttr;
struct BeliefGridAttrStruct
{
  int id;
};


/*******************************************************************/
/**************       EXTERNAL FUNCTIONS            ****************/
/*******************************************************************/

extern BeliefGrid BS_newGrid( );
extern void BS_destroyGrid( BeliefGrid bg );

#endif
