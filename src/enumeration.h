
/*
 *<SOURCE_HEADER>
 *
 *  <NAME>
 *    enumeration.h
 *  </NAME>
 *  <AUTHOR>
 *    Anthony R. Cassandra
 *  </AUTHOR>
 *  <CREATE_DATE>
 *    July, 1998
 *  </CREATE_DATE>
 *
 *  <RCS_KEYWORD>
 *    $RCSfile: enumeration.h,v $
 *    $Source: /u/cvs/proj/pomdp-solve/src/enumeration.h,v $
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
#ifndef ENUMERATION_H
#define ENUMERATION_H

#include "params.h"

/**********************************************************************/
/********************       CONSTANTS       ***************************/
/**********************************************************************/

#define CMD_ARG_ENUM_PURGE_OPTION                "-enum_purge"

/* How to purge the set of vectors that are enumerated. */
#define DEFAULT_ENUM_PURGE_OPTION                purge_prune

/**********************************************************************/
/********************   EXTERNAL VARIABLES   **************************/
/**********************************************************************/

/* For keeping some statistics while doing the enumeration. */
extern int gNumVectorsEnum;

/**********************************************************************/
/********************   EXTERNAL FUNCTIONS    *************************/
/**********************************************************************/

extern void initEnumeration();

extern void cleanUpEnumeration( );
      
/* This is the routine to call to do the enumeration exact POMDP
  algorithm given a set of this epochs projection vectors.  */
extern AlphaList improveEnumeration( AlphaList **projection,
                                     PomdpSolveParams param );

#endif
