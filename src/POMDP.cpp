#include <Rcpp.h>
#include <numeric>
#include "math.h"
#include "model.h"
#include "POMDP.h"

//#define DEBUG

using namespace Rcpp;

// NOTE: Episodes in time-dependent POMDPs are currently unsupported.
// NOTE: all are 0-based integer indices

// [[Rcpp::export]]
DataFrame reward_cpp(const NumericMatrix& belief, const NumericMatrix& alpha) {
  NumericVector rew(belief.nrow());
  IntegerVector pg_node(belief.nrow());
  
  for (R_xlen_t i = 0; i < rew.size(); ++i) {
    NumericVector rews = vecprod(alpha, belief(i, _ ));
    //rew[i] = max(rews);
    pg_node[i] = which_max(rews);
    rew[i] = rews[pg_node[i]];
  }
  
  // NOTE: we add 1 for R indexing of pg_node
  return DataFrame::create( Named("reward") = rew , _["pg_node"] = pg_node + 1);
}


// Updating the belief state: update for a single belief vector, one action, and one observation.
// $$b'(s') = \eta O(o | s',a) \sum_{s \in S} T(s' | s,a) b(s)$$
// $$\eta = 1/ \sum_{s' \in S}[ O(o | s',a) \sum_{s \in S} T(s' | s,a) b(s)]$$

// [[Rcpp::export]]
NumericVector update_belief_cpp(const List& model, const NumericVector& belief,
  int action, int observation, int digits = 7) {
  
  //NumericVector obs_v = observation_matrix(model, action)( _ , observation);
  //NumericVector obs_v = observation_matrix(model, action).column(observation);
  NumericMatrix::Column obs_v = observation_matrix(model, action).column(observation);
  NumericMatrix tr_v = transition_matrix(model, action);
  
  NumericVector new_belief = obs_v * veccrossprod(tr_v, belief);
  new_belief = new_belief / sum(new_belief);
  
  // round so we get fewer distinct belief states.
  new_belief = round_stochastic_cpp(new_belief, digits);
  
  return new_belief;
}

