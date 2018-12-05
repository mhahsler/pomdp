
/*
 *<SOURCE_HEADER>
 *
 *  <NAME>
 *    linear-support.c
 *  </NAME>
 *  <AUTHOR>
 *    Anthony R. Cassandra
 *  </AUTHOR>
 *  <CREATE_DATE>
 *    July, 1998
 *  </CREATE_DATE>
 *
 *  <RCS_KEYWORD>
 *    $RCSfile: linear-support.c,v $
 *    $Source: /u/cvs/proj/pomdp-solve/src/linear-support.c,v $
 *    $Revision: 1.2 $
 *    $Date: 2004/10/10 03:44:54 $
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
 *   Implementation of Hsien-Te Cheng's Linear Support algorithm for
 *   solving partially observable Markov decision problems.  Computes a
 *   one step improvement.
 * 
 *   Note that it only handles reward structures.  I would need to add
 *   extra cases to handle cost structures.  
 */


#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "mdp/mdp.h"

#include "global.h"
#include "pomdp.h"
#include "alpha.h"
#include "params.h"
#include "common.h"
#include "vertex-enum.h"
#include "linear-support.h"

/* We would like to keep track of the number of vertices generated for
   each epoch.  The improveLinSupport() routine will set this to zero
   when it is called and within the course of generating the set of
   next vectors this global variable will be updated.  After the
   improveLinSupport() routine finishes, gNumVertices will be set to
   the number of vertices that were enumerated.  */
int gNumVertices;

/**********************************************************************/
void 
initLinSupport( ) 
{

#ifndef HAVE_LIBCPLEX
  Abort( "Linear support currently requires the CPLEX LP solver." );
#endif

  initVertexEnum( gNumStates );
  
}  /* initLinSupport */
/**********************************************************************/
void 
cleanUpLinSupport( ) 
{
  
  cleanUpVertexEnum( );
  
}  /* cleanUpLinSupport */
/**********************************************************************/
int 
sameVertex( double *b1, double *b2, double epsilon ) 
{
   int i;

   for( i = 0; i < gNumStates; i++ )
      if( ! Equal( b1[i], b2[i], epsilon ) )
         return( 0 );

   return( 1 );
         
}  /* sameVertex */
/**********************************************************************/
VertexList 
queryVertexList( VertexList list, 
		 double *vertex,
		 double epsilon ) 
{

   while( list != NULL ) {
      if( sameVertex( list->b, vertex, epsilon ))
         return( list );

      list = list->next;
   }  /* while */

   return( NULL );
}  /* queryVertexList */
/**********************************************************************/
void 
showLinSupportVertex( double *b, FILE *file ) 
{
   int i;

   fprintf( file, "[ " );
   for( i = 0; i < gNumStates; i++ )
      fprintf( file, "%.3lf ", b[i] );
   fprintf(file, "]" );
}  /* showLinSupportVertex */
/**********************************************************************/
void 
showVertexList( VertexList list, FILE *file ) 
{
   int count = 0;

   fprintf( file, "Vertex List:\n");

   while( list != NULL ) {
      fprintf(file, "\t");

      showLinSupportVertex( list->b, file );

      if( list->flags & IN_SET_C )
         fprintf( file, "( C " );
      else
         fprintf( file, "(   ");

      if( list->flags & IN_SET_E )
         fprintf( file, "E " );
      else
         fprintf( file, "  ");

      if( list->flags & IN_SET_E_TILDE )
         fprintf( file, "~E " );
      else
         fprintf( file, "   ");


      fprintf( file, ")\n");
      list = list->next;
      count++;
   }  /* while */

   fprintf( file, "There are %d vertices in this list.\n", count );

}  /* showVertexList */
/**********************************************************************/
VertexList 
purgeVertexList( VertexList list ) 
{
   VertexList trail_ptr, walk_ptr, temp_ptr;

   trail_ptr = walk_ptr = list;
   while( walk_ptr != NULL ) {
      if( walk_ptr->flags == DELETED ) {
         temp_ptr = walk_ptr;

         /* If this is first in list */
         if( temp_ptr == list ) {
            list = list->next;
            walk_ptr = list;
            XFREE( temp_ptr->b );
            XFREE( temp_ptr );
         }  /* If this is first in list */

         else {  /* this is not first in list */
            walk_ptr = walk_ptr->next;
            trail_ptr->next = walk_ptr;
            XFREE( temp_ptr->b );
            XFREE( temp_ptr );
         }  /* not first in list */
      } /* if remove this node */

      else {
         trail_ptr = walk_ptr;
         walk_ptr = walk_ptr->next;
      }
   }  /* while */

   return( list );
}  /* purgeVertexSet */
/**********************************************************************/
VertexList 
addVertex( VertexList vertex_list, double *vertex,
	   unsigned int flags, double Hv_value ) 
{
   int i;
   VertexList new_node;

   new_node = (VertexList) XMALLOC( sizeof( *new_node ));
   new_node->b = (double *) XMALLOC( gNumStates * sizeof( double ));

   for( i = 0; i < gNumStates; i++ )
      new_node->b[i] = vertex[i];
   
   new_node->flags = flags;
   new_node->Hv = Hv_value;
   new_node->err = HUGE_VAL;  /* to indicate not set */
   new_node->next = vertex_list;

   return( new_node );
}  /* addVertex */
/**********************************************************************/
void 
destroyVertexList( VertexList list ) 
{
   VertexList temp;

   while( list != NULL ) {
      temp = list;
      list = list->next;
      XFREE( temp->b );
      XFREE( temp );
   }  /* while */
}  /* destroyVertexList */
/**********************************************************************/
VertexList 
findVertices( VertexList vertex_list, 
	      AlphaList cur_alpha,
	      AlphaList new_alpha_list,
	      double epsilon ) 
{
   VertexList point;
   
   /* This loop will set of the relaxed region, get all of the
      vertices for the region.  Put them all in set C.
      */
   startVertexEnum( cur_alpha, new_alpha_list );
   while( getVertex( gTempBelief ) ) {
      
      gNumVertices++;

      if( (point = queryVertexList( vertex_list, gTempBelief, 
                                    epsilon )))
         point->flags |= IN_SET_C;
      else
         vertex_list = addVertex( vertex_list, gTempBelief, 
                                  IN_SET_C, 0.0 );
      
   }  /* while getVertex */

   endVertexEnum();
   
   return( vertex_list );
}  /* findVertices */
/**********************************************************************/
VertexList 
initVertexList(  ) 
{
  int i;
  VertexList 
     vertex_list = NULL;  /* This is the master list of vertices and
                            corresponds to all three of the vertex
                            sets that Cheng defines.  The flag field
                            will keep track of which is in which.  */

  /* We will just put one simplex corner point in the list.  It will
     go into sets E and C, but not \tilde{E}.  */
  gTempBelief[0] = 1.0;
  for( i = 1; i < gNumStates; i++ )
     gTempBelief[i] = 0.0;
  
  /* This routine will add the vertex to the list (will create 
     memory and copy vertex), with the initial flag to
     indicate it is in set C.
     */
  vertex_list = addVertex( vertex_list, gTempBelief, 
                          IN_SET_C, 0.0 );
     
  return( vertex_list );
}  /* initVertexList */
/**********************************************************************/
int 
calcHv( VertexList vertex_list, AlphaList **projection, double  epsilon ) 
{
   VertexList list;
   AlphaList dummy;

   /* We will go through the point list and for each point that is in
      set C determine Hv and store it in the list.  We also empty out
      the set C.  We will also move all points that are only in set C
      into set E before emptying it out.  This was originally done
      in two separate places in Cheng's formulation, but doing it
      once here becomes equivalent.
      */
   list = vertex_list;
   while( list != NULL) {
      
      if( list->flags & IN_SET_C ) {
         list->Hv = oneStepValue( list->b, projection,
                                 &dummy, epsilon );

         /* See if I can combine these two into a single boolean 
            expressioin.
            */
         list->flags -= IN_SET_C;  /* Clear the C set too */
         list->flags |= IN_SET_E;  /* In case it already isn't in 
                                      this set. */
      }
      
      list = list->next;
   }  /* while */
   
   return( 1 );
}  /* calcHv */
/**********************************************************************/
VertexList 
calcError( VertexList vertex_list, 
	   AlphaList new_alpha_list, 
	   double epsilon ) 
{
   double max_error = 0.0;
   VertexList max_point_ptr = NULL;
   VertexList list;
   AlphaList dummy_ptr;

   /* We now go through the list, and for each point in set E (i.e.,
      a point that hasn't been checked) we compute the current
      error for this point by taking the difference between its
      actual one-step value and the value our current set of
      vectors gives it.  We also keep track of the point with the 
      maximum error.
      */
   list = vertex_list;
   while( list != NULL) {
      
      if(( list->flags & IN_SET_E)
         && !(list->flags & IN_SET_E_TILDE )) {
      
         /* If we have no vectors in our current set of vectors
            then we set the error to be tremendous.
            */
         if( new_alpha_list->length != 0 )
            /* This assumes that list->Hv has already been calculated */
           list->err = list->Hv - bestVectorValue( new_alpha_list, 
                                                   list->b,
                                                   &dummy_ptr,
                                                   epsilon );
         else
            list->err = HUGE_VAL;

         if( list->err < epsilon )
            list->flags |= IN_SET_E_TILDE;      
         else 
            if ( list->err > max_error ) {
               max_error = list->err;
               max_point_ptr = list;
            }
      }  /* if not in \tilde{E} */
         
      list = list->next;
   }  /* while */
      
   /* If all points were less than the tolerable error (gLinSupportEpsilon)
      then we are finished with the algorithm, which we indicate by
      returning NULL.
      */
   return( max_point_ptr );
}  /* calcError */
/**********************************************************************/
AlphaList 
newSupportVertices( VertexList pi_hat, 
		    VertexList *vertex_list,
		    AlphaList **projection,
		    AlphaList new_alpha_list,
		    double epsilon ) 
{
  AlphaList cur_alpha;

  /* For the point sent in we construct the lexicographic max alpha
     vector that corresponds to that point.  This routine will put the
     vector in the list (if it is not there already) and return the
     pointer into the list for this vector.
     */
  cur_alpha = makeAlphaVector( new_alpha_list, projection, 
                               pi_hat->b, epsilon );
 
  /* zzz cur_alpha could be NULL if the vector already exists.  Why
     doesn't this routine handle this case??  Either add something to
     handle it or figure out why it could never be NULL. */

  /* This routine will find the vertices for the relaxed region of the
     support cur_alpha.  It will add all of these points into the
     vertex_list with the flag set to IN_SET_C, or if the point is
     already in gVertexList, just change the flag to indicate that it
     is also in set C.  
     */
  *vertex_list = findVertices( *vertex_list, cur_alpha, 
                               new_alpha_list, epsilon );

  /* Determine if pi_hat is in set C.  If in C then also put it
     into \tilde{E}.  If not in C, then delete from list. */
  if( pi_hat->flags & IN_SET_C )
     pi_hat->flags |= IN_SET_E_TILDE;
  else
     pi_hat->flags = DELETED;

  /* We could call purgeVertexSet() here to get rid of this one
     deleted node, but it will be called in the next step anyway,
     so let's save a traversal through the list and wait one step.
     */

  return( cur_alpha );
}  /* newSupportVertices */
/**********************************************************************/
VertexList 
calcNewError( VertexList vertex_list, AlphaList cur_alpha ) 
{
   VertexList temp_ptr;
   double new_err;
   int i;

   temp_ptr = vertex_list;
   while( temp_ptr != NULL ) {

      if(( temp_ptr->flags & IN_SET_E)
         && !(temp_ptr->flags & IN_SET_E_TILDE )) {

         /* This assumes that Hv has already been calculated.
          It will calculate Hv - dotProduct( alpha, b )
          */
         new_err = temp_ptr->Hv;
         for( i = 0; i < gNumStates; i++ )
            new_err -= cur_alpha->alpha[i] * temp_ptr->b[i];

         if( !( temp_ptr->flags & IN_SET_C)
            && ( new_err < temp_ptr->err ))
        temp_ptr->flags = DELETED;
      }
      temp_ptr = temp_ptr->next;
   }  /*  while */

   /* remove all vertices with the flag set to DELETED */
   vertex_list = purgeVertexList( vertex_list );

   return( vertex_list );
}  /* calcNewError */
/**********************************************************************/
AlphaList 
improveLinSupport( AlphaList **projection,
		   PomdpSolveParams param ) 
{
/*
  The main linear support algorithm routine for finding the
  Q-function represention for value iteration with POMDPs.  
*/
  AlphaList new_alpha_list;
  VertexList max_point;
  VertexList vertex_list = NULL;
  AlphaList cur_alpha;

  new_alpha_list = newAlphaList();
  
  vertex_list = initVertexList( );  /* step 0 */
  
  gNumVertices = 0;
  
  while( 1 ) {
    calcHv( vertex_list, projection, param->epsilon );  /* step 1 */
    
    /* If all points less than tolerable error, then we are done.
       (i.e., step 2)
    */
    if( (max_point = calcError( vertex_list, 
                                new_alpha_list,
                                param->epsilon )) == NULL )
      break;
    
    /* Find the vector for the point with the maximum error and
       generate the points for its relaxed region.  (i.e., step 3 )
    */
    cur_alpha = newSupportVertices( max_point, &vertex_list,
                                    projection, new_alpha_list,
                                    param->vertex_epsilon );
    
    /* See if this new vector allows us to remove points from 
       the list.  (i.e., step 4)
    */
    vertex_list = calcNewError( vertex_list, cur_alpha );
    
    /* Don't really need step 5, we do the set moving in step 1 now
       and so we simply go to the top of the loop (which happens
       to be step 1.
    */
  }  /* while */
  
  destroyVertexList( vertex_list );
  
  /* This has to go somewhere, but just pasted here for now. */
  fprintf(param->report_file, "VertEnum=%d ", gNumVertices );

  return ( new_alpha_list );
  
}  /* improveLinSupport */
/**********************************************************************/

