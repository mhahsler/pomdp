
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
 *    $RCSfile: index-list.h,v $
 *    $Source: /u/cvs/proj/pomdp-solve/src/index-list.h,v $
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

#ifndef INDEX_LIST_H
#define INDEX_LIST_H

#define INITIAL_CAPACITY   10

#include <stdio.h>

/**********************************************************************/
/********************       CONSTANTS       ***************************/
/**********************************************************************/

/**********************************************************************/
/********************   DEFAULT VALUES       **************************/
/**********************************************************************/

/**********************************************************************/
/********************    TYPEDEFS            **************************/
/**********************************************************************/

typedef struct IndexListStruct *IndexList;
struct IndexListStruct {

  int size;
  int capacity;

  int* element;

};

/**********************************************************************/
/********************   EXTERNAL VARIABLES   **************************/
/**********************************************************************/

/**********************************************************************/
/********************   EXTERNAL FUNCTIONS    *************************/
/**********************************************************************/


IndexList IL_new();

void IL_dispose( IndexList list );

void IL_resize( IndexList list );

void IL_append( IndexList list, int value );

int IL_sizeOf( IndexList list );
       
#endif

