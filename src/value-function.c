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
 *    $RCSfile: value-function.c,v $
 *    $Source: /u/cvs/proj/pomdp-solve/src/value-function.c,v $
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

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <mdp/mdp.h>

#include "global.h"
#include "value-function.h"

/**********************************************************************/
ValueFunction 
VF_new(  ) 
{
  /* Allocates memory for a new BeliefGrid which will be initially
	empty. */
  FacetList f;
  ValueFunction vf;
  ValueFunctionAttr attr;

  attr = (ValueFunctionAttr) XMALLOC( sizeof( *attr ));

  attr->obs_source = NULL;
  attr->obs = INVALID_OBS;
  attr->prev_source = NULL;

  vf = DV_newList( attr );

  return vf;

}  /* VF_new */

/**********************************************************************/
void 
VF_destroy( ValueFunction vf ) 
{
  /* Deallocates memory for a BeliefGrid. The grid cannot be NULL. */

  DoubleVectorNode node;

  Assert( vf != NULL, "Cannot destroy NULL value function." );
    
  /* First we need to free the attributes for each node (facet) in the 
	value function. */ 
  node = vf->head;
  while( node != NULL )
    {
	 if ( node->attr != NULL )
	   {

		/* FIXME: Decide if these need to also be deallocated here 

		   node->attr->witness = NULL;
		   node->attr->belief_list = NULL;
		*/

		XFREE( node->attr );
		node->attr = NULL;
	   }

	 node = node->next;
    } /* while node != NULL */

  if ( vf->attr != NULL )
    {

	 /* FIXME: Decide if these need to also be deallocated here 

	    vf->attr->obs_source = NULL;
	    vf->attr->prev_source = NULL;
	 */

	 XFREE( vf->attr );
	 vf->attr = NULL;
    }

  DV_destroyList( vf );




}  /* VF_destroy */
