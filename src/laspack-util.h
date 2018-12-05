/*
  laspack-util.h

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
#ifndef LASPACK_UTIL_H
#define LASPACK_UTIL_H

/* The LAPACK modules */
#include <laspack/qmatrix.h>
#include <laspack/vector.h>

/* Arguments for the LASPACK iterative solver. */
#define ITER_SOLVE_ACCURACY                1e-5
#define JACOBI_PRECON_OMEGA                0.8
#define MAX_JACOBI_ITERATIONS              1000

extern void showVector( Vector *v );
extern void showQMatrix( QMatrix *M );
  
#endif
