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
 *    $RCSfile: value-function.h,v $
 *    $Source: /u/cvs/proj/pomdp-solve/src/value-function.h,v $
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
 *  Representing value functions (was alpha.[ch]).
 */

#ifndef VALUE_FUNCTION_H
#define VALUE_FUNCTION_H

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "double-vector.h"
#include "belief-state.h"

/*******************************************************************/
/**************      USEFUL MNENOMIC CONSTANTS      ****************/
/*******************************************************************/

/*******************************************************************/
/**************             TYPEDEFS                ****************/
/*******************************************************************/

/* A value function is a wrapper around the double vector list
   structure that adds its own attributes. The nodes of this list are
   'facets'. */
typedef DoubleVectorList ValueFunction;

/* Often we will want to represent lists of value function facets, but
   which do not really comprise a value function per-se.  This
   requires fewer attributes (currently none) in the list header. */
typedef DoubleVectorList FacetList;

/* A value function is basically a list of these 'facet' nodes. */
typedef struct FacetAttrStruct *FacetAttr;
struct FacetAttrStruct {

  /* Each vector can have an associated action. */ 
  int action;

  /* When looking at a facet of the value function, we may do a linear
	program to find the point of maximal difference (a witness
	point). This provides a place to save both the point and the LP
	objective function (maximum diff). */
  BeliefState witness;

  double witness_delta;

  /* Sometimes we will want to associate one or more belief points
     with this vector (not specifically LP-derived witness points).
     This is an array of pointers to belief points.  is the point that
     we used to determine that this vector was indeed a useful vector.
     This will only exist for vectors in a parsimonious
     representation. */
  /*
  BeliefStateList belief_list;
  */
};

typedef struct ValueFunctionAttrStruct *ValueFunctionAttr;
struct ValueFunctionAttrStruct {

  /* When constructing V_t from V_{t-1} we will use a vector from
     V_{t-1} for each observation in the construction of a vector
     in V_t.  We would like to keep track of which ones we used
     so that we can trace out the policy tree.  This array will be
     pointers, but the confusing part is that at one point they are
     pointers into the projection sets and at another they are
     pointers into the previous alpha list.  They are mostly the
     former, but just before the policy graph stuff is used these are
     set to point directly into the previous alpha list.*/
  ValueFunction *obs_source;
  
  /* When dealing with a set that represents a projection of the
     previous value function, we may also want to keep track of the
     observation used in the projection.  For othert sets this
     variable will have no useful value. This will only have meaning
     in the header of a list. */
  int obs;

  /* Also, when we have a projection set, we will want to maintain a
     pointer into the previous alpha vector set which this vector is
     the projection of. This field is used when the vector is a
     projection of a previous alpha list vector. */
  ValueFunction prev_source;

};

/*******************************************************************/
/**************       EXTERNAL FUNCTIONS            ****************/
/*******************************************************************/

extern ValueFunction VF_new( );
extern void VF_destroy( ValueFunction vf );

#endif
