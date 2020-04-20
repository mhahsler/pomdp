
/*
 *<SOURCE_HEADER>
 *
 *  <NAME>
 *    enumeration.c
 *  </NAME>
 *  <AUTHOR>
 *    Anthony R. Cassandra
 *  </AUTHOR>
 *  <CREATE_DATE>
 *    July, 1998
 *  </CREATE_DATE>
 *
 *  <RCS_KEYWORD>
 *    $RCSfile: enumeration.c,v $
 *    $Source: /u/cvs/proj/pomdp-solve/src/enumeration.c,v $
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
 *   This module implements the enumeration type algorithms where all
 *   possible vectors are enumerated and then useless ones removed.  This
 *   approach was first proposed by Sondik in his thesis, but Monahan is
 *   often credited with this because his survey paper presents this
 *   algorithm as if it was Sondik's one-pass algorithm.  The enumeration
 *   approach was improved by introducing a more efficient was to
 *   discover the useless vectors.  Eagle proposed the simple domination
 *   checks which help significantly, and White and Lark devised a clever
 *   way to set up the LPs.
 * 
 */

#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "mdp/mdp.h"

#include "global.h"
#include "pomdp.h"
#include "cmd-line.h"
#include "alpha.h"
#include "cross-sum.h"
#include "parsimonious.h"
#include "enumeration.h"

/* These will have the total number of vectors enumerated during a
   call to improveEnumeration.  */
int gNumVectorsEnum;

/**********************************************************************/
AlphaList 
enumerationByQ( AlphaList *projection ) 
{
  /*
    Enumerates all the vectors in the Q-function only.
  */
  
  int z;
  AlphaList list = NULL;
  AlphaList next_list;
  int last_enum_count = 0;;

  Assert ( projection != NULL, "Projection is NULL." );

  /* Check for the special case of a single observation POMDP.  This
     corresponds to a completely unobservable problem, but for this
     we have no cross-sum to do. */
  if ( gNumObservations == 1 ) {
    gNumVectorsEnum += projection[0]->length;
    return ( duplicateAlphaList( projection[0] ));
  }

   /* We do one less cross-sum than the number of observations. */
   for( z = 1; z < gNumObservations; z++) {
     
     /* Note that we put the burden of setting the source arrays on the
        crossSum routine. */
     if ( z == 1 ) 
       next_list = crossSum( projection[0], projection[1], TRUE );

     else {
       next_list = crossSum( list, projection[z], TRUE );
       destroyAlphaList( list );
     }

     /* Note that if we simply put the pruning operation here, then
        you get the vanilla incremental pruning algorithm!!! */

     list = next_list;

     /* To make sure gNumVectorsEnum alwasy has the current value, we
        need to add only the change in the list size from doing the
        last cross-sum. */
     gNumVectorsEnum += list->length - last_enum_count;
     last_enum_count = list->length;

   } /* for z */

   return ( list );
}  /* enumerationByQ */
/**********************************************************************/
AlphaList 
enumerationByV( AlphaList **projection ) 
{
  /*
    Enumerates all vectors (i.e., for each action).
  */
  int a;
  
  AlphaList list, next_list;
  
  Assert ( projection != NULL, "Projection is NULL." );

  /* Initialize this to the empty list because we will be unioning all
     the list together. */
  list = newAlphaList();

  /* In case we are interrupted in the course of enumerating the
     vectors, we want to keep a running count so the interrupt handler
     can report the status. This count is updated in the
     enumerationByQ() routine. */
  gNumVectorsEnum = 0;  

  for( a = 0; a < gNumActions; a++ ) {
    
    next_list = enumerationByQ( projection[a] );

    /* This not only puts everything from next_list into list, but
       also free the memory for the header of next_list. */
    unionTwoAlphaLists( list, next_list );
    
  } /* for a */
  
  return ( list );

}  /* enumerationByV */
/**********************************************************************/
AlphaList 
improveEnumeration( AlphaList **projection, 
		    PomdpSolveParams param ) 
{
  /*
    This is the routine to call to do the enumeration exact POMDP
    algorithm given a set of this epochs projection vectors.
  */
  AlphaList new_list;
  int total, final;

  Assert ( projection != NULL, "Projection is NULL." );

  new_list = enumerationByV( projection );
  total = sizeAlphaList( new_list );

  purgeAlphaList( new_list, param->enum_purge_option, param );
  final = sizeAlphaList( new_list );

  if ( gVerbose[V_ENUMERATE] )
    fprintf( param->report_file,
             " (Total=%d, Final=%d, %.1lf%% dominated) ", 
             total, final,
             100.0* ((double) (total - final))/ ((double) total) );

  return ( new_list );

 }  /* improveEnumeration  */
/**********************************************************************/
void 
initEnumeration() 
{

}  /* initEnumeration */
/**********************************************************************/
void 
cleanUpEnumeration( ) 
{

}  /* cleanUpEnumeration */
/**********************************************************************/
