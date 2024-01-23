#ifndef POMDP_MODEL_H
#define POMDP_MODEL_H

#include <Rcpp.h>
#include <numeric>

#include "dgCMatrix.h"

using namespace Rcpp;

// C++ interface to access elements of a POMDP model

// NOTE: Episode in time-dependent POMDPs are currently unsupported.
// NOTE: All indices are 0-based.


// Access model information
inline bool is_solved(const List& model) { 
  return model.containsElementNamed("solution");
}

inline bool is_converged(const List& model) { 
  return as<LogicalVector>(as<List>(model["solution"])["converged"])[0];
}

// More accessors
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

// NA is inf, we return the sum of episode horizons for now
inline int get_horizon(const List& model) {
  NumericVector h = model["horizon"];
  if (!is_finite(h)[0]) 
    return R_NaInt;
  
  return (int) sum(h);
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


// Transitions & Observations
// Can be a dense matrix or a dgCMatrix
// Available functions are: x_matrix returns a dense matrix, x_prob returns double, and x_row returns a vector
inline NumericMatrix transition_matrix(const List& model, int action, int episode = -1) {
  RObject acts;
  if (episode >= 0)
    acts = as<List>(as<List>(model["transition_prob"])[episode])[action];
  else
    acts = as<List>(model["transition_prob"])[action];
  
  // dense matrix
  if (is<NumericMatrix>(acts)) 
    return as<NumericMatrix>(acts); 
  
  // dgCMatrix
  if (is<S4>(acts))
    return dgCMatrix(as<S4>(acts)).dense();
  
  stop("model needs to be normalized with normalize_POMDP().");
}

inline double transition_prob(const List& model, int action, int start_state, int end_state, int episode = -1) {
  RObject acts;
  if (episode >= 0)
    acts = as<List>(as<List>(model["transition_prob"])[episode])[action];
  else
    acts = as<List>(model["transition_prob"])[action];
  
  // dense matrix
  if (is<NumericMatrix>(acts)) 
    return as<NumericMatrix>(acts)(start_state, end_state); 
  
  // dgCMatrix
  if (is<S4>(acts))
    return dgCMatrix(as<S4>(acts)).at(start_state, end_state);
  
  stop("model needs to be normalized with normalize_POMDP().");
}

inline NumericVector transition_row(const List& model, int action, int start_state, int episode = -1) {
  RObject acts;
  if (episode >= 0)
    acts = as<List>(as<List>(model["transition_prob"])[episode])[action];
  else
    acts = as<List>(model["transition_prob"])[action];
  
  // dense matrix
  if (is<NumericMatrix>(acts)) 
    return as<NumericMatrix>(acts).row(start_state); 
  
  // dgCMatrix
  if (is<S4>(acts))
    return dgCMatrix(as<S4>(acts)).row(start_state);
  
  stop("model needs to be normalized with normalize_POMDP().");
}

inline NumericMatrix observation_matrix(const List& model, int action, int episode = -1) {
  RObject acts;
  if (episode >= 0)
    acts = as<List>(as<List>(model["observation_prob"])[episode])[action];
  else
    acts = as<List>(model["observation_prob"])[action];
  
  // dense matrix
  if (is<NumericMatrix>(acts)) 
    return as<NumericMatrix>(acts); 
  
  // dgCMatrix
  if (is<S4>(acts))
    return dgCMatrix(as<S4>(acts)).dense();
  
  stop("model needs to be normalized with normalize_POMDP().");
}

inline double observation_prob(const List& model, int action, int end_state, int observation, int episode = -1) {
  RObject acts;
  if (episode >= 0)
    acts = as<List>(as<List>(model["observation_prob"])[episode])[action];
  else
    acts = as<List>(model["observation_prob"])[action];
  
  // dense matrix
  if (is<NumericMatrix>(acts)) 
    return as<NumericMatrix>(acts)(end_state, observation); 
  
  // dgCMatrix
  if (is<S4>(acts))
    return dgCMatrix(as<S4>(acts)).at(end_state, observation);
  
  stop("model needs to be normalized with normalize_POMDP().");
}

inline NumericVector observation_row(const List& model, int action, int end_state, int episode = -1) {
  RObject acts;
  if (episode >= 0)
    acts = as<List>(as<List>(model["observation_prob"])[episode])[action];
  else
    acts = as<List>(model["observation_prob"])[action];
  
  // dense matrix
  if (is<NumericMatrix>(acts)) 
    return as<NumericMatrix>(acts).row(end_state); 
  
  // dgCMatrix
  if (is<S4>(acts))
    return dgCMatrix(as<S4>(acts)).row(end_state);
  
  stop("model needs to be normalized with normalize_POMDP().");
}


// Reward

// TODO add support for episodes

// Can be a dense matrix or a data.frame
// Available are reward_matrix and reward_val
inline NumericMatrix reward_matrix(const List& model, int action, int start_state, int episode = -1) {
  RObject reward = model["reward"];
  if (episode >= 0)
    reward = as<List>(reward)[episode];
  
  if (is<DataFrame>(reward)) {
    DataFrame df = as<DataFrame>(reward);
    IntegerVector actions = df[0], start_states = df[1], end_states = df[2], observations = df[3];
    NumericVector values = df[4]; 
    
    NumericMatrix rew(get_states(model).size(), get_obs(model).size());
    
    for (auto i = 0; i < df.nrows(); ++i) {
      if(
        (IntegerVector::is_na(actions[i]) || actions[i] == action) && 
        (IntegerVector::is_na(start_states[i]) || start_states[i] == start_state)) {
      
          if (IntegerVector::is_na(end_states[i]) &&
              IntegerVector::is_na(observations[i])) 
                  std::fill(rew.begin(), rew.end(), values[i]);
          else if (IntegerVector::is_na(end_states[i]))
                  rew(_ , observations[i]) = NumericVector(rew.rows(), values[i]);
          else if (IntegerVector::is_na(observations[i]))
                  rew(end_states[i], _) = NumericVector(rew.cols(), values[i]);
          else
                  rew(end_states[i], observations[i]) = values[i];
      }
    }
        
    return rew;
  }
  
  // it is a matrix
  return as<NumericMatrix>(as<List>(as<List>(reward)[action])[start_state]);
}


// Note: R_index does not apply to episode!!!
inline double reward_val(const List& model, int action, int start_state, int end_state, int observation,
  int episode = -1, bool R_index = FALSE) {
  RObject reward = model["reward"];
  if (episode >= 0)
    reward = as<List>(reward)[episode];
  
  // data.frame indices are 1-based!!!
  if (!R_index) {
    action++; start_state++; end_state++; observation++;
  }
  
  if (is<DataFrame>(reward)) {
    DataFrame df = as<DataFrame>(reward);
    // find the best matching entry
    IntegerVector actions = df[0], start_states = df[1], end_states = df[2], observations = df[3];
    NumericVector values = df[4]; 
    
    for (auto i = df.nrows()-1; i >= 0; --i) {
      if(
          (IntegerVector::is_na(actions[i]) || actions[i] == action) && 
          (IntegerVector::is_na(start_states[i]) || start_states[i] == start_state) &&
          (IntegerVector::is_na(end_states[i]) || end_states[i] == end_state) &&
          (IntegerVector::is_na(observations[i]) || observations[i] == observation)
        )
        return values[i];
        
    }
    return 0.0;
    
  }
    
  // it is not a data.frame so it must be a List of matrices
  return reward_matrix(model, action, start_state, episode)(end_state, observation);
}  


// terminal value
inline double terminal_val(const List& model, int state) {
  if (!model.containsElementNamed("terminal_values") || 
      model["terminal_values"] == R_NilValue)
    return 0.0;
  
  NumericVector terminal_values = model["terminal_values"];
  
  return terminal_values[state];
}



// MDP section

// MDP has a different reward structure without observations
// * in a data.frame observations is missing
// * as a matrix it is always a vector instead of a matrix!
inline NumericVector reward_vector_MDP(const List& model, int action, int start_state) {
  RObject reward = model["reward"];

  if (is<DataFrame>(reward)) {
    DataFrame df = as<DataFrame>(reward);
    IntegerVector actions = df[0], start_states = df[1], end_states = df[2];
    NumericVector values = df["value"]; 
    
    NumericVector rew(get_states(model).size());
    
    for (auto i = 0; i < df.nrows(); ++i) {
      if(
        (IntegerVector::is_na(actions[i]) || actions[i] == action) && 
          (IntegerVector::is_na(start_states[i]) || start_states[i] == start_state)) {
        
        if (IntegerVector::is_na(end_states[i])) 
          std::fill(rew.begin(), rew.end(), values[i]);
        else
          rew(end_states[i]) = values[i];
      }
    }
    
    return rew;
  }

  // remember it is just a vector not a matrix!
  return as<NumericVector>(as<List>(as<List>(reward)[action])[start_state]);
}

// Note: R_index does not apply to episode!!!
inline double reward_val_MDP(const List& model, int action, int start_state, int end_state,
                        bool R_index = FALSE) {
  RObject reward = model["reward"];
  
  // data.frame indices are 1-based!!!
  if (!R_index) {
    action++; start_state++; end_state++;
  }
  
  if (is<DataFrame>(reward)) {
    DataFrame df = as<DataFrame>(reward);
    // find the best matching entry
    IntegerVector actions = df[0], start_states = df[1], end_states = df[2];
    NumericVector values = df["value"]; 
    
    for (auto i = df.nrows()-1; i >= 0; --i) {
      if(
        (IntegerVector::is_na(actions[i]) || actions[i] == action) && 
          (IntegerVector::is_na(start_states[i]) || start_states[i] == start_state) &&
          (IntegerVector::is_na(end_states[i]) || end_states[i] == end_state)
      )
        return values[i];
      
    }
    return 0.0;
    
  }
  
  // it is not a data.frame so it must be a List of matrices
  return reward_vector_MDP(model, action, start_state)[end_state];
}  

// returns the MDP policy as a vector. Index is the state index and the value is the action index.
inline IntegerVector get_policy_MDP(const List& model) {
  if (!is_solved(model))
    stop("Unsolved MDP model. No policy available");
  
  return as<IntegerVector>(as<List>(as<List>(as<List>(model["solution"])["policy"])[0])["action"]) - 1;
}
#endif