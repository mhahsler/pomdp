#ifndef POMDP_MATH_H
#define POMDP_MATH_H

#include <Rcpp.h>
using namespace Rcpp;

// C++ interface
NumericVector round_stochastic_cpp(const NumericVector& x, int digits = 7);
NumericVector veccrossprod(const NumericMatrix& A, const NumericVector& b);
NumericVector vecprod(const NumericMatrix& A, const NumericVector& b);

// Internal
// check is z in contained in X
bool contains(IntegerVector X, int z);

#endif