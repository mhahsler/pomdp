#include <Rcpp.h>
#include <numeric>
#include "math.h"
#include "model.h"
#include "POMDP.h"

//#define DEBUG

using namespace Rcpp;

// NOTE: Episodes in time-dependent POMDPs are currently not supported.
// NOTE: all are 0-based integer indices

// epsilon -1 means 0 for solved models and 1 for unsolved models

// [[Rcpp::export]]
List simulate_POMDP_cpp(const List& model,
  const int n,
  const NumericVector& belief,
  const int horizon,
  const double disc = 1.0,
  const bool return_beliefs = false,
  const bool return_trajectories = false,
  const double epsilon = 1.0,
  const int digits = 7,
  const bool verbose = false
) {
  const bool solved = is_solved(model);
  const bool converged = solved && is_converged(model);
  
  //double disc = get_discount(model); 
  const int nstates = get_states(model).size();
  const int nactions = get_actions(model).size();
  const int nobs = get_obs(model).size();
  
  // define current belief b, action a, observation o, and state s
  
  // allocate output
  NumericVector rews(n);
  IntegerVector action_cnt(nactions);
  action_cnt.names() = get_actions(model);
  IntegerVector state_cnt(nstates);
  state_cnt.names() = get_states(model);
  IntegerVector obs_cnt(nobs);
  obs_cnt.names() = get_obs(model);
  
  // store belief states
  NumericMatrix belief_states(0, 0);
  if (return_beliefs)
    belief_states = NumericMatrix(n * horizon, nstates);
  int k = 0; // index in belief_states
  
  // store trajectories
  std::vector<int> tr_episode;
  std::vector<int> tr_time;
  std::vector<int> tr_simulation_state;
  std::vector<int> tr_alpha_vector_id;
  std::vector<int> tr_a;
  std::vector<double> tr_r;
  
  if (verbose) {
    NumericVector print_belief = belief;
    std::string more = "";
    if (belief.size() > 10) {
      print_belief = head(belief, 10);
      more = " ...";
    }
    
    Rcout << "Simulating POMDP trajectories.\n"
          << "- method: " << "C++ (cpp)" << "\n"
          << "- n: " << n << "\n"
          << "- horizon: " << horizon << "\n"
          << "- epsilon: " << epsilon << "\n"
    //  if (dt)
    //    cat("- time-dependent:", length(dt_horizon), "episodes", "\n")
    
    << "- discount factor: " << disc << "\n"
    << "- starting belief: " << print_belief << more << "\n\n";
  }
  
  // start with converged values to be faster
  NumericMatrix alpha; 
  IntegerVector pg_actions; 
  
  if (solved) { 
    alpha = get_alpha(model, 0);
    //pg_actions = as<IntegerVector>(get_pg(model, 0)["action"]);
    pg_actions = as<IntegerVector>(get_pg(model, 0)[1]) - 1;
  } else{ 
    if (epsilon != 1)
      stop("epsilon needs to be 1 for unsolved models.");
  }
  
  // n replications
  for (int i = 0; i < n; ++i) {
    NumericVector b;
    int a, o, s, s_prev, alpha_vector_id;
    double r, disc_pow; // used for discounting
#ifdef DEBUG 
    Rcout << "--- Replication " << i << " ---\n";
#endif
    rews[i] = 0.0;
    b = Rcpp::clone(belief);
    s = sample(nstates, 1, false, b, false)[0]; // last false if for 0-based index
    disc_pow = 1.0;
    
    // horizon epochs  
    int j;
    for (j = 0; j < horizon; ++j) {
#ifdef DEBUG 
      Rcout << "Epoch: " << j << "\n";
      Rcout << "State: " << s << "\n";
#endif
      // find action (if we have no solution then take a random action) and update state and sample obs
      
      if (epsilon != 0 && (epsilon == 1 || R::runif(0, 1) < epsilon)) {
        a = sample(nactions, 1, false, R_NilValue, false)[0];
      } else {
        // actions in model start index with 1! 
        if (!converged) {
          if (!solved)
            stop("Model is not solved. No alpha vectors and policy available!");
          
          alpha = get_alpha(model, j);
          //pg_actions = get_pg(model, j)["actions"];
          pg_actions = as<IntegerVector>(get_pg(model, j)[1]) - 1;
        }
#ifdef DEBUG 
        Rcout << "alpha:\n" << alpha << "\n" 
              << "b: "<< b << "\n" 
              << "alpha %*% b: = " <<  vecprod(alpha, b) << "\n";
#endif
        alpha_vector_id = which_max(vecprod(alpha, b));
        a = pg_actions[alpha_vector_id];
        
#ifdef DEBUG 
        Rcout << "a: " << a <<  " (idx: " << alpha_vector_id <<  ")\n";
#endif
      }
      
      s_prev = s;
      //NumericVector trans_v = transition_matrix(model, a)(s, _ );
      NumericVector trans_v = transition_matrix(model, a).row(s);
      s = sample(nstates, 1, false, trans_v, false)[0];
      
#ifdef DEBUG 
      Rcout << "New state: " << s << "\n";
#endif
      
      //NumericVector obs_v = observation_matrix(model, a)(s, _ );
      NumericVector obs_v = observation_matrix(model, a).row(s);
      o = sample(nobs, 1, false, obs_v, false)[0];

#ifdef DEBUG 
      Rcout << "Observation: " << o << "\n";
#endif
      
      action_cnt[a]++;
      state_cnt[s]++;
      obs_cnt[o]++;
      
      //rews[i] += reward_matrix(model, a, s_prev)(s, o) * pow(disc, j);
      //rews[i] += reward_matrix(model, a, s_prev)(s, o) * disc_pow;  
      r = reward_val(model, a, s_prev, s, o);
      rews[i] += r * disc_pow;
      
#ifdef DEBUG 
      //Rcout << "reward: " << reward_matrix(model, a, s_prev)(s, o) << " (disc_pow: " << disc_pow << ")\n\n";
      Rcout << "reward: " << r << " (disc_pow: " << disc_pow << ")\n\n";
#endif
      
      disc_pow *= disc;
      
      b = update_belief_cpp(model, b, a, o, digits);
      
      //Rcout << "action: " << a << "\n"
      //      << "observation: " << o << "\n"
      //      << "reward " << reward_matrix(model, a, s_prev)(s, o) << "\n"
      //      << "new belief: " << b << "\n\n";
      
      if (return_beliefs) {
        belief_states(k++, _ ) = b;
      }
      
      if (return_trajectories) {
        tr_episode.push_back(i + 1);
        tr_time.push_back(j);
        tr_simulation_state.push_back(s_prev + 1);
        tr_alpha_vector_id.push_back(alpha_vector_id + 1);
        tr_a.push_back(a + 1);
        tr_r.push_back(r);
      }
      
    }
    
    // add terminal reward
    if (j == get_horizon(model)) {
      rews[i] += terminal_val(model, s) * disc_pow;
#ifdef DEBUG 
      Rcout << "Adding terminal value for state " << s << 
        "(" << terminal_val(model, s) << ")" << "\n";
#endif
    }  
  }
  
  if (return_beliefs)
    colnames(belief_states) = get_states(model);
 
  DataFrame trajectories;
 
  if (return_trajectories) {
   IntegerVector simulation_state_v = IntegerVector(tr_simulation_state.begin(), tr_simulation_state.end());
   simulation_state_v.attr("class") = "factor";
   simulation_state_v.attr("levels") = get_states(model);
   
   IntegerVector a_v = IntegerVector(tr_a.begin(), tr_a.end());
   a_v.attr("class") = "factor";
   a_v.attr("levels") = get_actions(model);
   
   trajectories = DataFrame::create(
     _["episode"] = tr_episode,
     _["time"] = tr_time,
     _["simulation_state"] = simulation_state_v,
     _["alpha_vector_id"] = tr_alpha_vector_id,
     _["a"] = a_v,
     _["r"] = tr_r
   );
  }
 
  double m = mean(rews);
  
  List L = List::create(Named("avg_reward") = m,
    _["reward"] = rews,
    _["action_cnt"] = action_cnt,
    _["state_cnt"] = state_cnt,
    _["obs_cnt"] = obs_cnt,
    _["belief_states"] = belief_states,
    _["trajectories"] = trajectories
  );
  
  return L;
}

