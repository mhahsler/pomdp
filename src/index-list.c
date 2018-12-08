/*
 *<SOURCE_HEADER>
 *
 *  <NAME>
 *    alpha.h
 *  </NAME>
 *  <AUTHOR>
 *    Anthony R. Cassandra
 *  </AUTHOR>
 *  <CREATE_DATE>
 *    July, 1998
 *  </CREATE_DATE>
 *
 *  <RCS_KEYWORD>
 *    $RCSfile: index-list.c,v $
 *    $Source: /u/cvs/proj/pomdp-solve/src/index-list.c,v $
 *    $Revision: 1.1 $
 *    $Date: 2005/02/01 15:18:23 $
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
 *   Data structure and routines for maintaining a dynamic list of
 *   indices.
 *
 *   A list of indices is an ordered list of non-negative integers.
 * 
 */

#include "global.h"
#include "xalloc.h"
#include "index-list.h"

/*************************************************************/
IndexList
IL_new()
{
  IndexList list;

  list = (IndexList) XMALLOC( sizeof( *list ));

  list->size = 0;

  list->element = (int *) XMALLOC( INITIAL_CAPACITY * sizeof(int));

  list->capacity = INITIAL_CAPACITY;

  return list;
} /* IL_new */

/*************************************************************/
void
IL_dispose( IndexList list )
{

  if ( list == NULL )
    {
	 Warning( "Cannot dispose NULL list." );
	 return;
    }

  XFREE( list->element );

  XFREE( list );

} /* IL_dispose */

/*************************************************************/
void
IL_resize( IndexList list )
{
  int i;
  int* old_element;

  if ( list == NULL )
    {
	 Warning( "Cannot resize NULL list." );
	 return;
    }

  old_element = list->element;

  list->element = (int *) XMALLOC( 2 * list->capacity * sizeof(int) );
  
  list->capacity *= 2;
  
  for ( i = 0; i < list->size; i++ )
    list->element[i] = old_element[i];

  XFREE( old_element );

} /* IL_resize */

/*************************************************************/
void
IL_append( IndexList list, int value )
{
  if ( list == NULL )
    {
	 Warning( "Cannot append to NULL list." );
	 return;
    }

  if ( list->size >= list->capacity )
    {
	 IL_resize( list );
    }

  list->element[list->size] = value;
  (list->size)++;
  
} /* IL_append */
/*************************************************************/
int
IL_sizeOf( IndexList list )
{
  if ( list == NULL )
    {
	 Warning( "Cannot get size of NULL list." );
	 return 0;
    }

  return list->size;

} /* IL_sizeOf */
