#ifndef POMDP_MODEL_H
#define POMDP_MODEL_H

#include <Rcpp.h>
#include <numeric>

using namespace Rcpp;

// NOTE: Episode in time-dependent POMDPs are currently unsupported.

// Note: all indices are 0-based

inline bool is_solved(const List& model) { 
  return model.containsElementNamed("solution");
}

inline bool is_converged(const List& model) { 
  return as<LogicalVector>(as<List>(model["solution"])["converged"])[0];
}

inline NumericMatrix transition_matrix(const List& model, int action) {
  return as<NumericMatrix>(as<List>(model["transition_prob"])[action]);
}

inline double transition_prob(const List& model, int action, int start_state, int end_state) {
  return transition_matrix(model, action)(start_state, end_state);
}

inline NumericMatrix observation_matrix(const List& model, int action) {
  return as<NumericMatrix>(as<List>(model["observation_prob"])[action]);
}
  
inline double observation_prob(const List& model, int action, int end_state, int observation) {
  return observation_matrix(model, action)(end_state, observation);
}

inline NumericMatrix reward_matrix(const List& model, int action, int start_state) {
  return as<NumericMatrix>(as<List>(as<List>(model["reward"])[action])[start_state]);
}

inline double reward_val(const List& model, int action, int start_state, int end_state, int observation) {
    return reward_matrix(model, action, start_state)(end_state, observation);
  }  

inline NumericVector start_vector(const List& model) {
  return as<NumericVector>(model["start"]);
}  

inline CharacterVector get_states(const List& model) {
  return as<CharacterVector>(model["states"]);
}  

inline CharacterVector get_obs(const List& model) {
  return as<CharacterVector>(model["observations"]);
}  

inline CharacterVector get_actions(const List& model) {
  return as<CharacterVector>(model["actions"]);
}  

inline double get_discount(const List& model) {
  return model["discount"];
}  

// get pg and alpha epochs (in case of non converged policies)
// epochs start with 0
inline int get_pg_index_cpp(const List& model, int epoch) {
  List pg = as<List>(as<List>(model["solution"])["alpha"]);
    
  // (converged) infinite horizon POMDPs. We ignore epoch.
  if (pg.length() == 1)
    return 0;

  // regular epoch for finite/infinite horizon case
  if (epoch < 0 || epoch >= pg.length())
    stop("Epoch not available! POMDP model has only solutions for ", pg.length(), " epochs!");
          
  return epoch;
}
  
inline NumericMatrix get_alpha(const List& model, int epoch = 0) {
  if (!is_solved(model))
    stop("Unsolved POMDP model. No alpha vectors available");
  
  epoch = get_pg_index_cpp(model, epoch);
  return as<NumericMatrix>(as<List>(as<List>(model["solution"])["alpha"])[epoch]);
}  

inline DataFrame get_pg(const List& model, int epoch = 0) {
  if (!is_solved(model))
    stop("Unsolved POMDP model. No policy graph available");
  
  epoch = get_pg_index_cpp(model, epoch);
  return as<DataFrame>(as<List>(as<List>(model["solution"])["pg"])[epoch]);
}

  
  
// MDP section

// MDP has a different reward structure (no observations so it is a vector not a matrix)
inline NumericVector reward_vector_MDP(const List& model, int action, int start_state) {
  return as<NumericVector>(as<List>(as<List>(model["reward"])[action])[start_state]);
}

inline double reward_val_MDP(const List& model, int action, int start_state, int end_state) {
    return reward_vector_MDP(model, action, start_state)[end_state];
  }  

// returns the MDP policy as a vector. Index is the state index and the value is the action index.
inline IntegerVector get_policy_MDP(const List& model) {
  if (!is_solved(model))
    stop("Unsolved MDP model. No policy available");
    
  return as<IntegerVector>(as<List>(as<List>(as<List>(model["solution"])["policy"])[0])["action"]) - 1;
}
#endif