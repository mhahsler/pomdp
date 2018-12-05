
/*
 *<SOURCE_HEADER>
 *
 *  <NAME>
 *    vertex-enum.h
 *  </NAME>
 *  <AUTHOR>
 *    Anthony R. Cassandra
 *  </AUTHOR>
 *  <CREATE_DATE>
 *    
 *  </CREATE_DATE>
 *
 *  <RCS_KEYWORD>
 *    $RCSfile: vertex-enum.h,v $
 *    $Source: /u/cvs/proj/pomdp-solve/src/vertex-enum.h,v $
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
 *    Implements the vertex enumeration algorithm by T. Mattheiss.  This
 *    algorithm finds all vertices of a convex polytope as expressed by a
 *    series of linear contraints.  It requires the CPLEX linear programming
 *    package and its C-callable librrary.
 * 
 */

#ifndef VERTEX_ENUM_H
#define VERTEX_ENUM_H

/*******************************************************************/
/**************      USEFUL MNENOMIC CONSTANTS      ****************/
/*******************************************************************/

/* How many decimal places to show in print statements */
#define VERTEX_ENUM_DISPLAY_PRECISION             2

/* We need to be careful about comparisons of double precision 
   numbers.  The algorithm will use these as tolerances.  Note 
   that no numerical analysis went into determining these, they
   were just chosen on a hunch.
   */
#define PRECISION_TOLERANCE        0.0000000001
#define NEG_PRECISION_TOLERANCE   -0.0000000001

/* We want to have a value that indicates that the value of gCurCol
   doesn't correspond to any column of tableaux.  Making it a very 
   large number simplifies the code, since it becomes the same as the 
   case when we have bumped up gCurCol past a valid column number.
*/
#define BIG_COL_NUM                            999999

/* There are three types of variables that will ocurr in the linear
   program: There are the original problem variables, which will be
   called 'x'.  There is also the extra added variable that Mattheiss' 
   algorithm subscribes, which will be called 'y'.  Finally, there are
   all the slack variables that the LP stuff adds (one for each 
   constraint).  At Certain times we want to ask what type of variable
   something is and these three constants will be used for that
   purpose
*/
#define ORIGINAL_VARIABLE                1
#define ADDED_VARIABLE                   2
#define SLACK_VARIABLE                   3

/* There are two distinct ways we want to set the basis of a solution,
   with and without the added variable, 'y'.  These constants just give 
   a mnemonic way to do this (for readability)
*/
#define NOT_Y_IN_BASIS                   0
#define Y_IN_BASIS                       1

/*  These are the values CPLEX uses to indicate whether or not a variable
    is in the basis or not.  Note that the last two forms I do not fully 
    understand and only apply for original problem variables.  It seems
    that the 0 and 1 are the most common and in fact are the onbly ones for
    the slack variables.
*/
#define NON_BASIC                        0
#define BASIC                            1
#define BASIC_AT_UPPER_BOUND             2
#define NON_BASIC_AND_FREE               3

/*******************************************************************/
/**************             TYPEDEFS                ****************/
/*******************************************************************/

/* This list will be an ordered linked list using the 'y' value
   from greatest to least.  It will also be maintained so that
   each record has a unique list of basic_slacks.  Since we expect
   there to be a lot of records, the amount of insertions into
   the list will be significant.  Therefore searching this list
   is a prime area for optimization of the algorithm.  Setting up 
   some data structure tyo allow binary searches will be a big
   win.  The linked list was chosen because of its simplicity.
*/
typedef struct Record_Struct *Record_List;
struct Record_Struct {
   double y;      /* value of extra variable for this tableau */
   unsigned int *basic_slacks;   /* 
                           A bit vector consisting of
                           which slack variables are in the basis.
                           0 - means non-basic and 1 means basic. 
                           Bit vectors are used to speed up
                           the comparisons to see if an
                           entry already exixts in the list.  There
                           will be a bit for each constraint, with
                           1 meaning in basis and 0 meaning non-basic. 
                           */
   int used;       /* A flag indicating whether we have used this
                      record yet, (serached its tableau
                      */
   Record_List next;
};

/*******************************************************************/
/**************       EXTERNAL VARIABLES            ****************/
/*******************************************************************/


/*******************************************************************/
/**************       EXTERNAL FUNCTIONS            ****************/
/*******************************************************************/

/* This routine allocates the memory that will not change from one
   problem to the next.  These variables only depend upon the number
   of variables in the problem, which we assume will be the same for
   all instances that the algorithm is used.  */
extern int initVertexEnum( int num_orig_variables );

/* This frees the memory that was static for all the vertex
   enumeration constraint sets.  It undoes the allocation that the
   routine initMattheis() set up.  */
extern int cleanUpVertexEnum( );

/* This routine assumes that all the column related memory stuff has
   already been allocated (i.e., a call to initVertexEnum()).  It will
   allocate the row specific memory, set up the relaxed region in
   question (based upon item and list) and then start the Mattheiss
   algorithms so that calls to getVertex() can be made.  When all
   vertcies have been enumerated, then a call to endVertexEnum()
   should happen.  */
extern int startVertexEnum( AlphaList item, AlphaList list );

/* Undoes what startVertexEnum() did.  */
extern void endVertexEnum( );

/* Repeated calls to this routine will give the entire set of
   vertices.  This assumes that a call has been made to
   initVertexEnum() so that the LP and data structures are properly
   set up.  The routine returns 1 if a vertex is enumerated, 0 if all
   vertices have been enumerated, and -1 if there is not a valid LP
   prepared yet.  Upon success the vertex will be returned in 'b',
   which assumes the caller has allocated.  It also returns -1 if 'b'
   is NULL.  */
extern int getVertex( double *b );

#endif
