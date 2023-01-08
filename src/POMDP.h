#ifndef POMDP_H
#define POMDP_H

#include <Rcpp.h>
using namespace Rcpp;

DataFrame reward_cpp(const NumericMatrix& belief, const NumericMatrix& alpha);
NumericVector update_belief_cpp(const List& model, const NumericVector& belief,
  int action, int observation, int digits);

#endif