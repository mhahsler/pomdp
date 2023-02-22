#ifndef POMDP_H
#define POMDP_H

#include <Rcpp.h>
#include "math.h"
#include "model.h"

using namespace Rcpp;

typedef List POMDP;
typedef List MDP;

// C++ interface
DataFrame reward_cpp(const NumericMatrix& belief, const NumericMatrix& alpha);
NumericVector update_belief_cpp(const List& model, const NumericVector& belief,
  int action, int observation, int digits);

#endif