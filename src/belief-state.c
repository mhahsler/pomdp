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
 *    $RCSfile: belief-state.c,v $
 *    $Source: /u/cvs/proj/pomdp-solve/src/belief-state.c,v $
 *    $Revision: 1.2 $
 *    $Date: 2005/02/04 15:10:22 $
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
 *  Representing a grid of belief points.
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "global.h"
#include "belief-state.h"

/**********************************************************************/
BeliefGrid
BS_newGrid(  ) 
{
  /* Allocates memory for a new BeliefGrid which will be initially
	empty. */

  BeliefGrid grid;
  BeliefGridAttr attr;

  attr = (BeliefGridAttr ) XMALLOC( sizeof( *attr ));

  /* Always zero for now */
  attr->id = 0;

  grid = DV_newList( attr );

  return grid;

}  /* BG_newGrid */

/**********************************************************************/
void
BS_destroyGrid( BeliefGrid grid ) 
{
  /* Deallocates memory for a BeliefGrid. The grid cannot be NULL. */

  DoubleVectorNode node;

  Assert( grid != NULL, "Cannot destroy NULL belief grid." );
    
  /* First we need to free the attributes for each node in the belief
	grid. */ 
  node = grid->head;
  while( node != NULL )
    {
	 if ( node->attr != NULL )
	   {
		XFREE( node->attr );
		node->attr = NULL;
	   }

	 node = node->next;
    } /* while node != NULL */

  if ( grid->attr != NULL )
    {
	 XFREE( grid->attr );
	 grid->attr = NULL;
    }

  DV_destroyList( grid );

}  /* BG_destroyGrid */
