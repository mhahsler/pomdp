/*
  laspack-util.c

  Miscellaneous utilities for LASPACK data structures.

  *****
  Copyright 1998, Anthony R. Cassandra

                           All Rights Reserved
                           
  Permission to use, copy, modify, and distribute this software and its
  documentation for any purpose other than its incorporation into a
  commercial product is hereby granted without fee, provided that the
  above copyright notice appear in all copies and that both that
  copyright notice and this permission notice appear in supporting
  documentation.
  
  ANTHONY CASSANDRA DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
  INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR ANY
  PARTICULAR PURPOSE.  IN NO EVENT SHALL ANTHONY CASSANDRA BE LIABLE FOR
  ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  *****
*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

/* The LAPACK modules */
#include <laspack/errhandl.h>
#include <laspack/itersolv.h>
#include <laspack/lastypes.h>
#include <laspack/operats.h>
#include <laspack/precond.h>
#include <laspack/qmatrix.h>
#include <laspack/rtc.h>
#include <laspack/vector.h>

#include "global.h"
#include "laspack-util.h"

/**********************************************************************/
/********   Utility routines for LASPACK library     ******************/
/**********************************************************************/

/**********************************************************************/
void showVector( Vector *v ) {
  int i;

  printf( "Vector: %s\n", V_GetName( v ) );
  
  for ( i = 0; i < V_GetDim( v ); i++ )
      printf( "%.3lf ", V_GetCmp( v, i+1 ) );
  printf( "\n" );

} /* showVector */
/**********************************************************************/
void showQMatrix( QMatrix *M ) {
  int i, j;

  printf( "Matrix: %s\n", Q_GetName( M ) );
  
  for ( i = 0; i < Q_GetDim( M ); i++ ) {
    printf( "Row %d: ", i );
    for ( j = 0; j < Q_GetDim( M ); j++ )
      if ( Equal( Q_GetEl( M, i+1, j+1 ), 0.0, 1e-5 ))
        continue;
      else
        printf( "(%d) %.3lf ", j, Q_GetEl( M, i+1, j+1 ) );
    printf( "\n" );
  } /* for i */

}  /* showQMatrix */
/**********************************************************************/
