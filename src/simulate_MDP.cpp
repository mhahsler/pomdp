#include <Rcpp.h>
#include <numeric>
#include "math.h"
#include "model.h"

//#define DEBUG

using namespace Rcpp;

/*** R
library(pomdp)
data(Maze)
Maze_norm <- normalize_MDP(Maze, sparse = FALSE)
Maze_norm_sparse <- normalize_MDP(Maze, sparse = TRUE)

# unsolved MDP
pomdp:::simulate_MDP_cpp(Maze_norm, 2, start_vector(Maze_norm), 10, 
  disc = 1, return_trajectories = TRUE, epsilon = 1, verbose = TRUE)

pomdp:::simulate_MDP_cpp(Maze_norm_sparse, 2, start_vector(Maze_norm_sparse), 10, 
  disc = 1, return_trajectories = TRUE, epsilon = 1, verbose = TRUE)

# solve MDP
sol <- solve_MDP(Maze_norm)
sol
policy(sol)

pomdp:::simulate_MDP_cpp(sol, 1, start_vector(sol), 10, 
  disc = 1, return_trajectories = FALSE, epsilon = 0, verbose = TRUE)

pomdp:::simulate_MDP_cpp(sol, 10, start_vector(sol), 10, 
  disc = 1, return_trajectories = TRUE, epsilon = 0, verbose = TRUE)
*/

// Note: all are 0-based integer indices
// epsilon -1 means 0 for solved models and 1 for unsolved models

// [[Rcpp::export]]
List simulate_MDP_cpp(const List& model,
  const int n,
  const NumericVector& start,
  const int horizon,
  const double disc = 1.0,
  const bool return_trajectories = false,
  const double epsilon = 1.0,
  const bool verbose = false
) {
  
  const bool solved = is_solved(model);
  
  //double disc = get_discount(model); 
  const int nstates = get_states(model).size();
  const int nactions = get_actions(model).size();
  
  // absorbing states?
  LogicalVector absorbing = absorbing_states(model);
  // this is which (starting with 0)
  IntegerVector abs_states = seq_along(absorbing) - 1;
  abs_states = abs_states[absorbing];
   
  // define current state s, action a (everything is 0-based!)
  int s, s_prev, a;
  double disc_pow; // used for discounting
  
  // allocate output
  NumericVector rews(n);
  IntegerVector action_cnt(nactions);
  action_cnt.names() = get_actions(model);
  IntegerVector state_cnt(nstates);
  state_cnt.names() = get_states(model);
  
  // store trajectories
  std::vector<int> tr_episode;
  std::vector<int> tr_time;
  std::vector<int> tr_s;
  std::vector<int> tr_a;
  std::vector<double> tr_r;
  std::vector<int> tr_s_prime;
  
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
      double r = reward_val_MDP(model, a, s_prev, s);
      rews[i] += r * disc_pow;
      disc_pow *= disc;
      
      if (return_trajectories) {
       tr_episode.push_back(i + 1);
       tr_time.push_back(j);
       tr_s.push_back(s_prev + 1);
       tr_a.push_back(a + 1);
       tr_r.push_back(r);
       tr_s_prime.push_back(s + 1);
      }
      
      if (contains(abs_states, s))
        break;
      }
  }
  
  // create factors
  //states.attr("class") = "factor";
  //states.attr("levels") = get_states(model);
  
  DataFrame trajectories;
  
  if (return_trajectories) {
    IntegerVector s_v = IntegerVector(tr_s.begin(), tr_s.end());
    s_v.attr("class") = "factor";
    s_v.attr("levels") = get_states(model);
    
    IntegerVector s_prime_v = IntegerVector(tr_s_prime.begin(), tr_s_prime.end());
    s_prime_v.attr("class") = "factor";
    s_prime_v.attr("levels") = get_states(model);
    
    IntegerVector a_v = IntegerVector(tr_a.begin(), tr_a.end());
    a_v.attr("class") = "factor";
    a_v.attr("levels") = get_actions(model);
    
    trajectories = DataFrame::create(
      _["episode"] = tr_episode,
      _["time"] = tr_time,
      _["s"] = s_v,
      _["a"] = a_v,
      _["r"] = tr_r,
      _["s_prime"] = s_prime_v);
  }
  
  double m = mean(rews);
  
  List L = List::create(Named("avg_reward") = m,
    _["reward"] = rews,
    _["action_cnt"] = action_cnt,
    _["state_cnt"] = state_cnt,
    _["trajectories"] = trajectories
  );
  
  return L;
}