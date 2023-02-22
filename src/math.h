#ifndef POMDP_MATH_H
#define POMDP_MATH_H

#include <Rcpp.h>
using namespace Rcpp;

// C++ interface
NumericVector round_stochastic_cpp(const NumericVector& x, int digits = 7);
NumericVector veccrossprod(const NumericMatrix& A, const NumericVector& b);
NumericVector vecprod(const NumericMatrix& A, const NumericVector& b);

#endif