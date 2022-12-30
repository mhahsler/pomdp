#include <Rcpp.h>
#include <numeric>
#include "math.h"
#include "model.h"

//#define DEBUG

using namespace Rcpp;


/*** R
library(pomdp)
Rcpp::sourceCpp("src/simulate_MDP.cpp", verb = TRUE)

data(Maze)
Maze_norm <- normalize_MDP(Maze)

# unsolved MDP
simulate_MDP_cpp(Maze_norm, 10, start_vector(Maze_norm), 10, 
  disc = .9, return_states = FALSE, epsilon = 1, verbose = TRUE)

simulate_MDP_cpp(Maze_norm, 10, start_vector(Maze_norm), 10, 
  disc = .9, return_states = TRUE, epsilon = 1, verbose = TRUE)
 
# solve MDP
sol <- solve_MDP(Maze_norm, discount = 1)
sol
policy(sol)
 
simulate_MDP_cpp(sol, 10, start_vector(sol), 20, 
  disc = 1, return_states = FALSE, epsilon = 0, verbose = TRUE)

simulate_MDP_cpp(sol, 10, start_vector(sol), 20, 
  disc = 1, return_states = TRUE, epsilon = 0, verbose = TRUE)
*/

// Note: all are 0-based integer indices
// epsilon -1 means 0 for solved models and 1 for unsolved models

// [[Rcpp::export]]
List simulate_MDP_cpp(const List& model,
  int n,
  const NumericVector& start,
  int horizon,
  double disc = 1.0,
  bool return_states = false,
  double epsilon = 1.0,
  bool verbose = false
  ) {

  bool solved = is_solved(model);
  
  //double disc = get_discount(model); 
  int nstates = get_states(model).size();
  int nactions = get_actions(model).size();
  
  // define current state s, action a
  int s, s_prev, a;
  double disc_pow; // used for discounting
  
  // allocate output
  NumericVector rews(n);
  IntegerVector action_cnt(nactions);
  action_cnt.names() = get_actions(model);
  IntegerVector state_cnt(nstates);
  state_cnt.names() = get_states(model);
  
  IntegerVector states;
  if (return_states)
    states = IntegerVector(n * horizon);
  int k = 0; // index in states
  
  if (verbose) {
    Rcout << "Simulating MDP trajectories.\n"
          << "- method: " << "C++ (cpp)" << "\n"
          << "- n: " << n << "\n"
          << "- horizon: " << horizon << "\n"
          << "- epsilon: " << epsilon << "\n"
          << "- discount factor: " << disc << "\n"
          << "- start state distribution: " << start << "\n\n";
  }
  
  if (!solved && epsilon != 1)
    stop("epsilon needs to be 1 to simulate unsolved MDPs!");
  
  // prepare policy for faster access
  IntegerVector pol;
  if (solved)
    pol = get_policy_MDP(model);
  
#ifdef DEBUG 
  Rcout << "Used policy: " << pol << "\n\n";
#endif
  
  // n replications
  for (int i = 0; i < n; ++i) {
#ifdef DEBUG 
      Rcout << "--- Replication " << i << " ---\n";
#endif
    rews[i] = 0.0;
    s = sample(nstates, 1, false, start, false)[0]; // last false if for 0-based index
    disc_pow = 1.0;
    
    // horizon epochs  
    for (int j = 0; j < horizon; ++j) {
#ifdef DEBUG 
      Rcout << "Epoch: " << j << "\n";
#endif
      // find action (if we have no solution then take a random action) and update state and sample obs
      
      if (epsilon != 0 && (epsilon == 1 || R::runif(0, 1) < epsilon)) {
        a = sample(nactions, 1, false, R_NilValue, false)[0];
      } else {
        // actions in model start index with 1! 
        a = pol[s];
      }
      
      // update state
      s_prev = s;
      NumericVector trans_v = transition_matrix(model, a)(s, _ );
      s = sample(nstates, 1, false, trans_v, false)[0];
      
      action_cnt[a]++;
      state_cnt[s]++;
      
      // reward
      rews[i] += reward_val_MDP(model, a, s_prev, s) * disc_pow;
      disc_pow *= disc;
      
      if (return_states)
        states[k++] = s + 1;
    }
  }
  
  // make states a factor
  states.attr("class") = "factor";
  states.attr("levels") = get_states(model);
 
  double m = mean(rews);
  
  List L = List::create(Named("avg_reward") = m,
    _["reward"] = rews,
    _["action_cnt"] = action_cnt,
    _["state_cnt"] = state_cnt,
    _["states"] = states
  );
  
  return L;
}