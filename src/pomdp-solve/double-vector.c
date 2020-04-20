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
 *    $RCSfile: double-vector.c,v $
 *    $Source: /u/cvs/proj/pomdp-solve/src/double-vector.c,v $
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
 *  Representations for vectors of doubles (used in value functions
 *  and belief points) and dynamically allocated lists of these.
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "mdp/mdp.h"

#include "global.h"
#include "double-vector.h"

/**********************************************************************/
/******************  DoubleVector Routines       **********************/
/**********************************************************************/

/**********************************************************************/
DoubleVector
DV_new(  ) 
{
  /* 
     Allocate memory for a vector, whose size is determined by
     the number of states.
  */
  return (  (double *) XMALLOC( sizeof( double ) * gNumStates ));

}  /* DV_new */

/**********************************************************************/
DoubleVector
DV_duplicate( DoubleVector v ) 
{
  /*
    Makes a copy of the vector also allocating the memory for it. 
  */
  double *temp;
  int i;
  
  if ( v == NULL)
    return NULL;
  temp = DV_new( );
  for ( i = 0; i < gNumStates; i++)
    temp[i] = v[i];
  
  return ( temp );
}  /* DV_duplicate */

/**********************************************************************/
void 
DV_copy( DoubleVector dest, DoubleVector src ) 
{
  /*
    Assumes the memory has been allocated and simply copies the values
    from the src to the dest argument.
  */
  int i;
  
  if (( src == NULL) || ( dest == NULL ))
    return;
  
  for( i = 0; i < gNumStates; i++)
    dest[i] = src[i];
  
}  /* DV_copy */
/**********************************************************************/
void 
DV_destroy( DoubleVector v ) 
{
  /*
    Free the memory for an vector. 
  */

  if ( v != NULL )
    XFREE( v );
}  /* DV_destroy */

/**********************************************************************/
int 
DV_isSame( DoubleVector first, DoubleVector second, double epsilon ) 
{
  /* 
     Compares two vectors and determines if they are the identical
     vector.  The tricky part here is that there is floating point
     comparisons that we need to deal with and that can have a
     substantial effect on the algorithm. 
  */
  int i;

  if (( first == NULL) && (second == NULL))
    return TRUE;
  
  if (( first == NULL ) || (second == NULL))
    return FALSE;
  
  for (i = 0; i < gNumStates; i++)
    if ( ! Equal( first[i], second[i], epsilon ))
      return (FALSE);
  
  return (TRUE);
}  /* DV_isSame */

/**********************************************************************/
int 
DV_isZero( DoubleVector v, double epsilon ) 
{
  /* 
     Just checks to see if all components are zero.  Will return
     TRUE if it is zero of if NULL is sent in and FALSE otherwise. 
  */
  int i;

  if ( v == NULL)
    return ( TRUE );

  for (i = 0; i < gNumStates; i++)
    if ( ! Equal( v[i], 0.0, epsilon ))
	 return ( FALSE );

  return ( TRUE );

} /* DV_isZero */

/**********************************************************************/
void 
DV_display( FILE *file, DoubleVector v ) 
{
  /*
    Display the vector to the file stream putting no extra whitespace
    before or after the vector.
  */
   int i;

   if ( v == NULL) {
      fprintf( file, "<NULL>");
      return;
   }

   fprintf( file, "[%.*lf", NUM_DECIMAL_DISPLAY, v[0] );
   for (i = 1; i < gNumStates; i++) {
      fprintf(file, " ");
      fprintf( file, "%.*lf", NUM_DECIMAL_DISPLAY, v[i] );
   }  /* for i */
      fprintf(file, "]");

}  /* DV_display */

/**********************************************************************/
void 
DV_show( DoubleVector v ) 
{
  /*
    Displays vector to stdout.
  */
  DV_display( stdout, v );
  fprintf( stdout, "\n" );

}  /* DV_show */

/**********************************************************************/
int 
DV_isLexicographicallyBetter( DoubleVector first,
						DoubleVector second,
						double epsilon ) 
{
  /* 
     Does a lexicographic check on two vectors, given the two vectors.
     Return TRUE if first is lexicographically better than
     second. 
  */
  int i;

  /* This loops iterates until it finds two components that are not
     exactly the same.  */
  for ( i = 0; i < gNumStates; i++ ) {

    if ( Equal( first[i], second[i], epsilon )) 
      continue;
    
    if ( GreaterThan( first[i], second[i], epsilon )) 
      return ( TRUE );

    else
      return ( FALSE );

  }  /* for i */

  /* If we get to here then they really are equal, so the first one is
     not better than the second. */
  return ( FALSE );

}  /* DV_isLexicographicallyBetter */


/**********************************************************************/
int 
DV_isDominated( DoubleVector first, DoubleVector second ) 
{
  /* 
     Returns true if second is component-wise dominated by first. The
     assumption here is that with two identical vectors neither would be
     considered dominating toward the other.  
  */
  int i;
 
  Assert( first != NULL && second != NULL,
          "Vector(s) is NULL." );

  /* We really can get away with an epsilon of 0.0 here; I can prove
     it!  */
  
  for (i = 0; i < gNumStates; i++) 
    if ( first[i] <= second[i] )
      return ( FALSE );

  return ( TRUE );
} /* isDominatedVector */
/**********************************************************************/


/**********************************************************************/
/******************  DoubleVectorNode Routines   **********************/
/**********************************************************************/

/**********************************************************************/
DoubleVectorNode 
DV_newNode( DoubleVector v, void* attr ) 
{
  /*
    Allocates the memory for and sets initial values for a list
    node. 
  */
  DoubleVectorNode temp;

  temp = (DoubleVectorNode) XMALLOC( sizeof( *temp ));

  temp->v = v;

  /* These fields should start out with some default values. */
  temp->id = UNINITIALIZED;
  temp->next = NULL;
  temp->attr = attr;

  return ( temp );
}  /* DV_newNode */


/**********************************************************************/
void 
DV_destroyNode( DoubleVectorNode temp ) 
{
  /*
    Frees the memory for an double vector node and the containing
    DoubleVector. Does not free the memory for 'attr'.
  */

  Assert( temp != NULL, "Cannot destroy NULL DV node." );

  DV_destroy( temp->v );
  
  /* Note that we *do not* clear out memory of 'source' and other
     fields because these are just pointers into other data structures
     whose memory might already have been freed or which is still in
     use. */

  XFREE( temp );

}  /* DV_destroyNode */

/**********************************************************************/
void 
DV_appendNodeToList( DoubleVectorList list, DoubleVectorNode node ) 
{
  /*
    Adds the node to the end of the list.
  */
  Assert( list != NULL && node != NULL, "Bad (NULL) parameter(s)." );
  
  if ( list->length == 0 ) {
    node->id = 0;
    list->head = node;
  }
  else {
    node->id = list->tail->id + 1;
    list->tail->next = node;
  }
  
  list->tail = node;
  (list->length)++;
  
}  /* DV_appendNodeToList */

/**********************************************************************/
void 
DV_prependNodeToList( DoubleVectorList list, DoubleVectorNode node ) 
{
  /*
    Adds the node to the beginning of the list. 
  */
  Assert( list != NULL && node != NULL, "Bad (NULL) parameter(s)." );

  if ( list->length == 0 ) {
    node->id = 0;
    list->tail = node;
  }
  else
    node->id = list->head->id - 1;
  
  node->next = list->head;
  list->head = node;
  (list->length)++;
  
}  /* DV_prependNodeToList */

/**********************************************************************/
DoubleVectorNode 
DV_dequeueNode( DoubleVectorList list ) 
{
  /*
    Removes the first item in the list and returns it.
  */
   DoubleVectorNode item;
   
   if ( list->length < 1 )
     return ( NULL );
   
   if ( list->length == 1 )
     list->tail = NULL;
   
   item = list->head;
   list->head = list->head->next;
   item->next = NULL;
   (list->length)--;
   
   return ( item );

}  /* DV_dequeueNode */

/**********************************************************************/
void 
DV_enqueueNode( DoubleVectorList list, DoubleVectorNode node ) 
{
  /*
    Puts a list node at the end of the list.
  */
  DV_appendNodeToList( list, node );
  
}  /* DV_enqueueNode */

/**********************************************************************/
DoubleVectorNode 
DV_duplicateNode( DoubleVectorNode node ) 
{
  /*
    Allocates the memory and copies an double vector node. Copies
    pointers if it has any, but not objects they point to. 
  */
  DoubleVectorNode new_node;

  Assert( node != NULL, "Cannot duplicate NULL node." );

  new_node = DV_newNode( DV_duplicate( node->v ), node->attr );

  new_node->id = node->id;
    
  return ( new_node );
}  /* DV_duplicateNode */

/**********************************************************************/
DoubleVectorNode
DV_appendDuplicateNodeToList( DoubleVectorList list, 
						DoubleVectorNode orig_node ) 
{
  /*
    Make a copy of the node and appends it to the list.  Returns a
    pointer to the newly created node.
  */
  DoubleVectorNode node;

  node = DV_duplicateNode( orig_node );
  DV_appendNodeToList( list, node );

  return ( node );

}  /* DV_appendDuplicateNodeToList */

/**********************************************************************/
/******************  DoubleVectorList Routines   **********************/
/**********************************************************************/

/**********************************************************************/
DoubleVectorList 
DV_newList( void* attr ) 
{
  /*
    Allocates the memory for the header node of a new  list. 
  */
  DoubleVectorList list;
  
  list = (DoubleVectorList) XMALLOC( sizeof( *list ));

  list->length = 0;
  list->head = NULL;
  list->tail = NULL;
  list->attr = attr;
  
  return ( list );

}  /* DV_newList */

/**********************************************************************/
void 
DV_renumberList( DoubleVectorList list ) 
{
  /*
    Renumbers the list so vectors are numbered sequentially.
  */
  DoubleVectorNode node;
  int index = 0;
  
  Assert( list != NULL, "List is NULL." );
   
  node = list->head;
  while( node != NULL ) {
    node->id = index++;
    node = node->next;
  }  /* while */
  
}  /* DV_renumberList */

/**********************************************************************/
DoubleVectorNode
DV_prependList( DoubleVectorList list,
			 DoubleVector v,
			 void* attr ) 
{
  /*
    Puts an  node at the beginning of the list and retruns a
    pointer to the node added.
  */
  DoubleVectorNode temp;
  
  Assert( list != NULL, "List is NULL." );
  
  temp = DV_newNode( v, attr );
  
  DV_prependNodeToList( list, temp );

  return ( temp );
}  /* DV_prependList */

/**********************************************************************/
DoubleVectorNode
DV_appendList( DoubleVectorList list,
			DoubleVector v,
			void* attr ) 
{
  /*
    Puts an node at the end of the list and retruns a
    pointer to the node added.
  */
  DoubleVectorNode temp;

  Assert( list != NULL, "List is NULL." );

  temp = DV_newNode( v, attr );

  DV_appendNodeToList( list, temp );

  return ( temp );
}  /* DV_appendList */

/**********************************************************************/
void 
DV_clearList( DoubleVectorList orig_list ) 
{
  /*
    Frees the memory for each node in the list and resets the header
    node to reflect an empty list. Does not change the attribute data
    i the lkst header and does not free the mmeory in the individual
    node attributes (if any).
  */
  DoubleVectorNode node, temp;
  
  Assert( orig_list != NULL, "List is NULL." );

  node = orig_list->head;
  while( node != NULL ) {
    temp = node;
    node = node->next;
    
    DV_destroyNode( temp );
  }  /* while */

  orig_list->length = 0;
  orig_list->head = NULL;
  orig_list->tail = NULL;

}  /* DV_clearList */
/**********************************************************************/
void 
DV_destroyList( DoubleVectorList list ) 
{
  /*
    Comletely frees up the memory for the entire list, including all
    nodes and the header node.  Doesn't free attribute memory though,
    for either the list header or the individual nodes of the list.
  */

  Assert( list != NULL, "List is NULL." );
  
  DV_clearList( list );
  XFREE( list );

}  /* DV_destroyList */

/**********************************************************************/
DoubleVectorNode 
DV_findNode( DoubleVectorList list, 
		   DoubleVector target,
		   double epsilon ) 
{
  /*
    This routine returns a pointer to the list node that contains
    the vector 'target' of interest if it is found.  It returns NULL
    if the vector is not in the list.
  */
  DoubleVectorNode node;

  Assert( list != NULL, "List is NULL." );

  node = list->head;
  
  while( node != NULL ) {
    
    if( DV_isSame( node->v, target, epsilon ) == TRUE )
      return( node );;
    
    node = node->next;
  }  /* while */
  
  return( NULL );
}  /* DV_findNode */

/**********************************************************************/
int 
DV_queryList( DoubleVectorList list, 
		    DoubleVector target,
		    double epsilon ) 
{
  /*
    Returns TRUE if the vector is in the list. 
  */

  return ( DV_findNode( list, target, epsilon ) != NULL );

}  /* DV_queryList */

/**********************************************************************/
int 
DV_sizeOfList( DoubleVectorList list ) 
{
  /*
    Just get the number of vectors in the list by accessing the
    variable in the header.
  */
  Assert( list != NULL, "List is NULL." );

  return ( list->length );

}  /* DV_sizeOfList */

/**********************************************************************/
void 
DV_copyList( DoubleVectorList dest_list,
		   DoubleVectorList src_list ) 
{
  /*
    Doesn't copy the attr fields, just leaves the destination ones in
    tact in the header and copies the individual node attributes.. 
  */
  DoubleVectorNode node, temp;
  DoubleVector v;

  Assert( dest_list != NULL, "Destination list is NULL." );
  Assert( dest_list->length < 1, "Destination list is not empty." );
  Assert( src_list != NULL, "Source list is NULL." );

  /* It is wrong to copy these fields, since they will want to point
     to whole new fragments of memory where the copy resides. */
  dest_list->head = NULL;
  dest_list->tail = NULL;
  dest_list->length = 0;
  
  /* Just copy from the source list. */
  node = src_list->head;
  while( node != NULL ) {

    v = DV_duplicate( node->v );
    temp = DV_appendList( dest_list, v, node->attr );

    /* Copy all the fields, though the burden on the being sane values
       lies in the orginal list. */
    temp->id = node->id;

    node = node->next;
  } /* while */
  
}  /* DV_copyList */

/**********************************************************************/
DoubleVectorList 
DV_duplicateList( DoubleVectorList src_list ) 
{
  /* 
     Allocates a new list and copies the src_list into it.
  */
  DoubleVectorList dest_list;

  Assert( src_list != NULL, "Source list is NULL." );

  dest_list = DV_newList( NULL );
  
  DV_copyList( dest_list, src_list );

  return ( dest_list );

}  /* DV_duplicateList */

/**********************************************************************/
int 
DV_isSameList( DoubleVectorList list1,
			DoubleVectorList list2,
			double epsilon ) 
{
  /* 
     Just checks if the two lists contain the same vectors in
     exactly the same order.   Ignores IDs and attributes.
  */
  DoubleVectorNode node1, node2;
  
  Assert( list1 != NULL && list2 != NULL, "List(s) is NULL." );

  if ( list1->length != list2->length )
    return ( FALSE );
  
  node1 = list1->head;
  node2 = list2->head;
  while( node1 != NULL ) {
    
    if ( DV_isSame( node1->v, node2->v, epsilon ) == FALSE )
      return ( FALSE );
    
    node1 = node1->next;
    node2 = node2->next;
  } /* while */
  
  return ( TRUE );
}  /* DV_isSameList */

/**********************************************************************/
int 
DV_isSimilaraList( DoubleVectorList list1, 
			    DoubleVectorList list2,
			    double epsilon )
{
  /*
    Returns true if the two lists contains the same vectors,
    though the order is not important.  
  */
  DoubleVectorNode node;

  Assert( list1 != NULL && list2 != NULL, 
          "Bad (NULL) parameter(s)." );

  if ( list1->length != list2->length )
    return ( FALSE );

  /* We do not want to make any assumptions about the lists containing
     unique vectors, so we must go through both lists to ensure there
     is a vector in the other list. */

  node = list1->head;
  while( node != NULL ) {

    if ( ! DV_queryList( list2, node->v, epsilon ))
      return ( FALSE );
    
    node = node->next;
  } /* while */
  
  node = list2->head;
  while( node != NULL ) {

    if ( ! DV_queryList( list1, node->v, epsilon ))
      return ( FALSE );
    
    node = node->next;
  } /* while */
  
  return ( TRUE );
}  /* DV_isSimilaraList */

/**********************************************************************/
void 
DV_unionTwoLists( DoubleVectorList list, DoubleVectorList other_list ) 
{
  /*
    Takes the union of the two lists sent in and returns the union in
    the 'list' argument.  It is a destructive union, since effectively
    all nodes in the other_list are moved to this list.
  */
  if (( list == NULL ) 
      || ( other_list == NULL )
      || ( other_list->length == 0 ))
    return;

  if ( list->length < 1 ) {
    list->head = other_list->head;
    list->length = other_list->length;
    list->tail = other_list->tail;
  }

  else {
    list->tail->next = other_list->head;
    list->tail = other_list->tail;
    list->length += other_list->length;
  }

  /* We only want to free the header node memory, since it still
     contains pointers to the nodes in the list which now exist in the
     first list. We first disconnect the nodes before calling the
     destroy method to prevent accidental memory recovery of nodes
     that were moved. */

  other_list->length = 0;
  other_list->head = NULL;
  other_list->tail = NULL;

  DV_destroyList( other_list );

} /* unionTwoAlphaLists */

/**********************************************************************/
void 
DV_displayList( FILE *file, DoubleVectorList list ) 
{
  /*
    Printout a textual version of the list.
  */
  DoubleVectorNode node;

  Assert( file != NULL, "File handle is NULL." );
  Assert( list != NULL, "List is NULL." );
  
  fprintf(file, "Vector List: Length=%d\n", list->length );

  node = list->head;
  while( node != NULL ) {
    
    fprintf(file, "<id=%d:", node->id );
    fprintf(file, " attr=%d", list->attr );

    fprintf ( file, "> " );

    DV_display(file, node->v );
    fprintf ( file, "\n" );

    node = node->next;
  }  /* while */

}  /* DV_displayList */

/**********************************************************************/
void 
DV_showList( DoubleVectorList list ) 
{
  /*
    Printout to stdout, Especially useful in debugger.
  */
  DV_displayList( stdout, list );
}  /* DV_showList */


#ifdef UMMAGUMMA

/**********************************************************************/
AlphaList 
readAlphaList( char *filename, int max_alphas ) 
{
  /*
    Reads a list of alpha vectors from a file.  The format of the file
    is very specific and does not allow comments.  It simply reads a
    sequence of numbers which are assumed to be in the correct order.
    This should only be used to read in things written out by the code.
    Also, there is no checking to make sure the file is consistent with
    the problem. i.e., if the probelm has 3 states and you read a file
    of 4 component vectors, this will not notice and might result in
    strange things.  It does a simple check of this by seeing if it is
    in the middle of a vector when the file ends.  However, this does
    not guarantee anything.
    
    It can also read only a subset of the file of vectors.
    Set max_alphas to <= 0 if you want the whole file read, otherwise
    it will only read the first max_alphas in the file.
  */
   FILE *file;
   int a, i;
   double *alpha;
   AlphaList list = NULL;
   
   if ((file = fopen(filename , "r")) == NULL) {
     fprintf( gStdErrFile, 
              "** Error: The alpha vector file: %s does not exist.\n",
             filename);
     return ( NULL );
   }
   
   list = newAlphaList();
   
   /* We want specifying zero to be like specifying a negative number,
      only we can't start out at zero, or else the loop will never be
      entered.  Just decrement max_alphas and everything will be
      hunky-dorry. */
   if ( max_alphas == 0 )
     max_alphas--;
   
   while( max_alphas != 0 ) {
     max_alphas--;
     if ( fscanf( file, "%d", &a ) == EOF )
       break;
     alpha = newAlpha();
     
     for ( i = 0; i < gNumStates; i++ )
       if ( fscanf( file, "%lf", &( alpha[i] )) == EOF ) {
         fprintf(gStdErrFile, 
                 "** Error: Alpha vector file format incorrect.\n");
         return ( NULL );
       }
     
     appendAlphaList( list, alpha, a );
   }  /* while */
   
   fclose( file );
   
   return ( list );
}  /* readAlphaList */
/**********************************************************************/
void 
writeAlphaList( FILE *file, AlphaList list ) 
{
  int i;

  Assert( file != NULL, "File handle is NULL." );
  Assert( list != NULL, "Alpha list is NULL." );
  
  list = list->head;
  while( list != NULL ) {
    fprintf( file, "%d\n", list->action );
    
    for ( i = 0; i < gNumStates; i++ )
	 fprintf( file, "%.*lf ", ALPHA_FILE_DECIMAL_PRECISION,
			list->alpha[i] );
    fprintf( file, "\n\n");
    
    list = list->next;
  }  /* while */

} /* writeAlphaList */
/**********************************************************************/
void 
saveAlphaList( AlphaList list, char *filename ) 
{
  /*
    Write the alpha list out to a file in the format that
    readAlphaList() will read them in.  It is not a very pretty format,
    but makes reading it in trivial.
  */
   FILE *file;
   
   if ((file = fopen(filename , "w")) == NULL) {
     fprintf(gStdErrFile, 
             "** Error: The alpha vector file: %s cannot be opened.\n",
             filename);
     return;
   }
   
   writeAlphaList( file, list );
   
   fclose( file );
}  /* saveAlphaList */

#endif
