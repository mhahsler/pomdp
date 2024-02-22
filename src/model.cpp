#include "model.h"

#include <Rcpp.h>
using namespace Rcpp;

// C++ interface to access elements of a POMDP model

// NOTE: Episode in time-dependent POMDPs are currently unsupported.
// NOTE: All indices are 0-based.

Environment pkg = Environment::namespace_env("pomdp");
Function R_transition_matrix = pkg["transition_matrix"];
Function R_observation_matrix = pkg["observation_matrix"];
Function R_reward_matrix = pkg["reward_matrix"];

// Access model information
bool is_solved(const List& model) { 
  return model.containsElementNamed("solution");
}

bool is_converged(const List& model) { 
  return as<LogicalVector>(as<List>(model["solution"])["converged"])[0];
}

// More accessors
NumericVector start_vector(const List& model) {
  //Environment pkg = Environment::namespace_env("pomdp");
  Function R_start_states = pkg["start_states"];
  return as<NumericVector>(R_start_states(model));
  //return as<NumericVector>(model["start"]);
}  

CharacterVector get_states(const List& model) {
  return as<CharacterVector>(model["states"]);
}  

LogicalVector absorbing_states(const List& model) {
  //Environment pkg = Environment::namespace_env("pomdp");
  Function R_absorbing_states = pkg["absorbing_states"];
  return R_absorbing_states(model);
}

CharacterVector get_obs(const List& model) {
  return as<CharacterVector>(model["observations"]);
}  

CharacterVector get_actions(const List& model) {
  return as<CharacterVector>(model["actions"]);
}  

double get_discount(const List& model) {
  return model["discount"];
} 

// NA is inf, we return the sum of episode horizons for now
int get_horizon(const List& model) {
  NumericVector h = model["horizon"];
  if (!is_finite(h)[0]) 
    return R_NaInt;
  
  return (int) sum(h);
}  


// get pg and alpha epochs (in case of non converged policies)
// epochs start with 0
int get_pg_index_cpp(const List& model, int epoch) {
  List pg = as<List>(as<List>(model["solution"])["alpha"]);
  
  // (converged) infinite horizon POMDPs. We ignore epoch.
  if (pg.length() == 1)
    return 0;
  
  // regular epoch for finite/infinite horizon case
  if (epoch < 0 || epoch >= pg.length())
    stop("Epoch not available! POMDP model has only solutions for ", pg.length(), " epochs!");
  
  return epoch;
}

NumericMatrix get_alpha(const List& model, int epoch) {
  if (!is_solved(model))
    stop("Unsolved POMDP model. No alpha vectors available");
  
  epoch = get_pg_index_cpp(model, epoch);
  return as<NumericMatrix>(as<List>(as<List>(model["solution"])["alpha"])[epoch]);
}  

DataFrame get_pg(const List& model, int epoch) {
  if (!is_solved(model))
    stop("Unsolved POMDP model. No policy graph available");
  
  epoch = get_pg_index_cpp(model, epoch);
  return as<DataFrame>(as<List>(as<List>(model["solution"])["pg"])[epoch]);
}


// Transitions & Observations
// Can be a dense matrix or a dgCMatrix
// Available functions are: x_matrix returns a dense matrix, x_prob returns double, and x_row returns a vector
NumericMatrix transition_matrix(const List& model, int action, int episode) {
  RObject acts;
  if (episode >= 0)
    acts = as<List>(model["transition_prob"])[episode];
  else
    acts = model["transition_prob"];

  /* too slow! 
  // function
  if (is<Function>(acts)) {
    return as<NumericMatrix>(R_transition_matrix(model, action + 1));
  }
  */
 
  // it is a list
  acts = as<List>(acts)[action];
 
  // dense matrix
  if (is<NumericMatrix>(acts)) 
    return as<NumericMatrix>(acts); 
 
  // dgCMatrix
  if (is<S4>(acts))
    return dgCMatrix(as<S4>(acts)).dense();
  
  // uniform/identity
  if (is<CharacterVector>(acts)) {
    int n_states = get_states(model).size();
    if (as<CharacterVector>(acts)[0] == "uniform") {
      NumericVector m(n_states * n_states, 1.0 / n_states); 
      m.attr("dim") = IntegerVector::create(n_states, n_states); 
      return NumericMatrix(m);
    }
    
    if (as<CharacterVector>(acts)[0] == "identity") {
      NumericMatrix m = NumericMatrix::diag(n_states, 1.0);
      return m;
    }
    
    stop("Unknown matrix specifier! Only 'identity' and 'uniform' are allowed.");
  }
  
  stop("transition_matrix: model needs to be normalized with normalize_POMDP().");
}

double transition_prob(const List& model, int action, int start_state, 
                       int end_state, int episode) {
  RObject acts;
  if (episode >= 0)
    acts = as<List>(model["transition_prob"])[episode];
  else
    acts = model["transition_prob"];
  
  /* too slow! 
  // function
  if (is<Function>(acts)) {
    return as<double>(R_transition_matrix(model, action + 1, start_state + 1, end_state + 1));
  }
  */
  
  // it is a list
  acts = as<List>(acts)[action];
  
  // dense matrix
  if (is<NumericMatrix>(acts)) 
    return as<NumericMatrix>(acts)(start_state, end_state); 
  
  // dgCMatrix
  if (is<S4>(acts))
    return dgCMatrix(as<S4>(acts)).at(start_state, end_state);
  
  // uniform/identity
  if (is<CharacterVector>(acts)) {
    int n_states = get_states(model).size();
    if (as<CharacterVector>(acts)[0] == "uniform")
      return (1.0 / n_states);
    
    if (as<CharacterVector>(acts)[0] == "identity") {
      if (start_state == end_state)
        return 1.0;
      else
        return 0.0;
    }
    
    stop("Unknown matrix specifier! Only 'identity' and 'uniform' are allowed.");
  }
  
  stop("transition_prob: model needs to be normalized with normalize_POMDP().");
}

NumericVector transition_row(const List& model, int action, int start_state, 
                             int episode) {
  RObject acts;
  if (episode >= 0)
    acts = as<List>(model["transition_prob"])[episode];
  else
    acts = model["transition_prob"];
  
  /* too slow! 
  // function
  if (is<Function>(acts)) {      
    return as<NumericVector>(R_transition_matrix(model, action + 1, start_state + 1));
  }
  */
  
  // it is a list
  acts = as<List>(acts)[action];
  
  // dense matrix
  if (is<NumericMatrix>(acts)) 
    return as<NumericMatrix>(acts).row(start_state); 
  
  // dgCMatrix
  if (is<S4>(acts))
    return dgCMatrix(as<S4>(acts)).row(start_state);
  
  // uniform/identity
  if (is<CharacterVector>(acts)) {
    int n_states = get_states(model).size();
    if (as<CharacterVector>(acts)[0] == "uniform") {
      NumericVector v(n_states, 1.0 / n_states);
      return v;
    }
    
    if (as<CharacterVector>(acts)[0] == "identity") {
      NumericVector v(n_states, 0.0);
      v[start_state] = 1.0;
      return v;
    }
    
    stop("Unknown matrix specifier! Only 'identity' and 'uniform' are allowed.");
  }
  
  stop("transition_row: model needs to be normalized with normalize_POMDP().");
}

NumericMatrix observation_matrix(const List& model, int action,
                                 int episode) {
  RObject acts;
  if (episode >= 0)
    acts = as<List>(model["observation_prob"])[episode];
  else
    acts = model["observation_prob"];
  
  /* too slow! 
  // function
  if (is<Function>(acts)) {
    return as<NumericMatrix>(R_observation_matrix(model, action + 1));
  }
  */
  
  // it is a list
  acts = as<List>(acts)[action];
  
  // dense matrix
  if (is<NumericMatrix>(acts)) 
    return as<NumericMatrix>(acts); 
  
  // dgCMatrix
  if (is<S4>(acts))
    return dgCMatrix(as<S4>(acts)).dense();
  
  // uniform
  if (is<CharacterVector>(acts)) {
    int n_states = get_states(model).size();
    int n_obs = get_obs(model).size();
    if (as<CharacterVector>(acts)[0] == "uniform") {
      NumericVector m(n_states * n_obs, 1.0 / n_obs); 
      m.attr("dim") = IntegerVector::create(n_states, n_obs); 
      return NumericMatrix(m);
    }

    stop("Unknown matrix specifier! Only 'uniform' are allowed.");
  }
  
  
  stop("observation_matrix: model needs to be normalized with normalize_POMDP().");
}

double observation_prob(const List& model, int action, int end_state, 
                        int observation, int episode) {
  RObject acts;
  if (episode >= 0)
    acts = as<List>(model["observation_prob"])[episode];
  else
    acts = model["observation_prob"];
  
  /* too slow! 
  // function
  if (is<Function>(acts)) {
    return as<double>(R_observation_matrix(model, action + 1,
                                                  end_state + 1, 
                                                  observation + 1));
  }
  */
  
  // it is a list
  acts = as<List>(acts)[action];
  
  // dense matrix
  if (is<NumericMatrix>(acts)) 
    return as<NumericMatrix>(acts)(end_state, observation); 
  
  // dgCMatrix
  if (is<S4>(acts))
    return dgCMatrix(as<S4>(acts)).at(end_state, observation);
  
  // uniform
  if (is<CharacterVector>(acts)) {
    int n_obs = get_obs(model).size();
    if (as<CharacterVector>(acts)[0] == "uniform") 
      return 1.0 / n_obs;
    
    stop("Unknown matrix specifier! Only 'uniform' are allowed.");
  }
  
  stop("observation_prob: model needs to be normalized with normalize_POMDP().");
}

NumericVector observation_row(const List& model, int action, int end_state, 
                              int episode) {
  RObject acts;
  if (episode >= 0)
    acts = as<List>(model["observation_prob"])[episode];
  else
    acts = model["observation_prob"];
  
  /* too slow! 
  // function
  if (is<Function>(acts)) {      
    return as<NumericVector>(R_observation_matrix(model, action + 1,
                                           end_state + 1));
  }
  */
  
  // it is a list
  acts = as<List>(acts)[action];
  
  // dense matrix
  if (is<NumericMatrix>(acts)) 
    return as<NumericMatrix>(acts).row(end_state); 
  
  // dgCMatrix
  if (is<S4>(acts))
    return dgCMatrix(as<S4>(acts)).row(end_state);
  
  // uniform
  if (is<CharacterVector>(acts)) {
    int n_obs = get_obs(model).size();
    if (as<CharacterVector>(acts)[0] == "uniform") {
      NumericVector v(n_obs, 1.0 / n_obs);
      return v;
    }
    
    stop("Unknown matrix specifier! Only 'uniform' are allowed.");
  }
  
  stop("observation_row: model needs to be normalized with normalize_POMDP().");
}


// Reward

// TODO add support for episodes

// Can be a dense matrix or a data.frame
// Available are reward_matrix and reward_val
NumericMatrix reward_matrix(const List& model, int action, int start_state, 
                            int episode) {
  RObject reward = model["reward"];
  if (episode >= 0)
    reward = as<List>(reward)[episode];
  
  /* too slow! 
  if (is<Function>(reward)) {
    return as<NumericMatrix>(R_reward_matrix(model, action + 1, start_state + 1));
  }
  */
  
  if (is<DataFrame>(reward)) {
    DataFrame df = as<DataFrame>(reward);
    IntegerVector actions = df[0], start_states = df[1], 
                  end_states = df[2], observations = df[3];
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
  
  reward = as<List>(as<List>(reward)[action])[start_state];
  // dense matrix
  if (is<NumericMatrix>(reward)) 
    return as<NumericMatrix>(reward); 
  
  // dgCMatrix
  if (is<S4>(reward))
    return dgCMatrix(as<S4>(reward)).dense();
  
  stop("reward_matrix: model needs to be normalized with normalize_POMDP().");
}


// Note: R_index does not apply to episode!!!
double reward_val(const List& model, int action, 
                         int start_state, int end_state, int observation,
                         int episode, bool R_index) {
  RObject reward = model["reward"];
  if (episode >= 0)
    reward = as<List>(reward)[episode];
  
  /* too slow! 
  // function
  if (is<Function>(reward)) {
    return as<double>(R_reward_matrix(model, action + 1,
                                           start_state + 1, 
                                           end_state + 1, 
                                           observation + 1));
  }
  */
  
  if (is<DataFrame>(reward)) {
    // factors in the data.frame are 1-based!!!
    if (!R_index) {
      action++; start_state++; end_state++; observation++;
    }
    
    DataFrame df = as<DataFrame>(reward);
    // find the best matching entry
    IntegerVector actions = df[0], start_states = df[1], end_states = df[2], 
                                        observations = df[3];
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
  
  // it is a list
  reward = as<List>(as<List>(reward)[action])[start_state];
 
  // dense matrix
  if (is<NumericMatrix>(reward)) 
    return as<NumericMatrix>(reward)(end_state, observation);
  
  // dgCMatrix
  if (is<S4>(reward))
    return dgCMatrix(as<S4>(reward)).at(end_state, observation);
  
  stop("reward_val: model needs to be normalized with normalize_POMDP().");
}  

// MDP has no observation!
NumericMatrix reward_matrix_MDP(const List& model, int action, int start_state, 
                                int episode) {
  RObject reward = model["reward"];
  if (episode >= 0)
    reward = as<List>(reward)[episode];
  
  /* too slow! 
  if (is<Function>(reward)) {
    return as<NumericMatrix>(R_reward_matrix(model, action + 1, start_state + 1));
  }
  */
  
  if (is<DataFrame>(reward)) {
    DataFrame df = as<DataFrame>(reward);
    IntegerVector actions = df[0], start_states = df[1], end_states = df[2];
    NumericVector values = df[3]; 
    
    NumericMatrix rew(get_states(model).size(), 1);
    
    for (auto i = 0; i < df.nrows(); ++i) {
      if(
        (IntegerVector::is_na(actions[i]) || actions[i] == action) && 
        (IntegerVector::is_na(start_states[i]) || start_states[i] == start_state)) {
      
          if (IntegerVector::is_na(end_states[i])) 
                  std::fill(rew.begin(), rew.end(), values[i]);
          else if (IntegerVector::is_na(end_states[i]))
                  rew(_ , 0) = NumericVector(rew.rows(), values[i]);
          else
                  rew(end_states[i], 0) = values[i];
      }
    }
        
    return rew;
  }
  
  // it is a matrix
  reward =  as<List>(as<List>(reward)[action])[start_state];
  if (is<NumericMatrix>(reward))
    return as<NumericMatrix>(reward);
  
  stop("reward_matrix_MDP: model needs to be normalized with normalize_POMDP().");
}


// Note: R_index does not apply to episode!!!
// Note: MDPs don't use observations (observations) use observation = 0!
double reward_val_MDP(const List& model, int action, 
                         int start_state, int end_state,
                         int episode, bool R_index) {
  RObject reward = model["reward"];
  if (episode >= 0)
    reward = as<List>(reward)[episode];
  
  /* too slow! 
  if (is<Function>(reward)) {
    return as<double>(R_reward_matrix(model, action + 1, start_state + 1,
                                      end_state + 1));
  }
  */
  
  if (is<DataFrame>(reward)) {
    // factors in the data.frame are 1-based!!!
    if (!R_index) {
      action++; start_state++; end_state++;
    }
    
    DataFrame df = as<DataFrame>(reward);
    // find the best matching entry
    IntegerVector actions = df[0], start_states = df[1], end_states = df[2];
    NumericVector values = df[3]; 
    
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
 
  // it is not a data.frame so it must be a list of a list of matrices
  reward =  as<List>(as<List>(reward)[action])[start_state];
  if (is<NumericMatrix>(reward))
    return as<NumericMatrix>(reward)(end_state, 0);
  
  stop("reward_val_MDP: model needs to be normalized with normalize_POMDP().");
}  

// terminal value
double terminal_val(const List& model, int state) {
  if (!model.containsElementNamed("terminal_values") || 
      model["terminal_values"] == R_NilValue)
    return 0.0;
  
  NumericVector terminal_values = model["terminal_values"];
  
  return terminal_values[state];
}

// returns the MDP policy as a vector. Index is the state index and the value is the action index.
IntegerVector get_policy_MDP(const List& model) {
  if (!is_solved(model))
    stop("Unsolved MDP model. No policy available");
  
  return as<IntegerVector>(as<List>(as<List>(as<List>(model["solution"])["policy"])[0])["action"]) - 1;
}

