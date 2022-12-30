#ifndef POMDP_MATH_H
#define POMDP_MATH_H

#include <Rcpp.h>
#include <numeric>

using namespace Rcpp;

NumericVector veccrossprod(const NumericMatrix& A, const NumericVector& b);
NumericVector vecprod(const NumericMatrix& A, const NumericVector& b);

#endif