# pomdp 1.2.1 (xx/xx/2024)

## New Features
* read_POMDP gained parameter verbose to debug reading.
* solve_x check now that the model is of type x.
* Added some POMDP file examples.

## Bugfixes
* Improved read_POMDP and write_POMDP.

# pomdp 1.2.0 (04/02/2024)

## New Features
* Added functions to work with MDP policies (see ? MDP_policy_functions).
* Added MDP solver functions: Q-learning, Sarsa, and expected Sarsa.
* simulate_MDP() and simulate_POMDP() gained parameter return_trajectories.
* New functions `absorbing_states()` and `reachable_states()` for MDPs and POMDPs.
* Support for gridworlds (see ? gridworld).
* New datasets: Cliff_walking, Windy_gridworld, RussianTiger
* plot_transition_graph() now hides unavailable actions.
* Added actions() to find available actions (unavailable actions have a reward
  of -Inf).
* Added make_partially_observable() and make_fully_observable() to convert 
  between MDPs and POMDPs.

## Changes
* simulate_POMDP(): Better calculation of T for infinite-horizon problems.
* several functions are now generics with methods for POMDP and MDP.
* policy() lost the parameters alpha and action.
* policy() and value_function() and gained the parameter drop.
* regret(): renamed parameter belief to start. Regret is now available for MDPs.
* simulate_MDP() stops now at absorbing states.
* simulate_MDP_cpp() works now with sparse model representation.
* POMDP and MDP gained field for additional info.
* approx_MDP_policy_evaluation() is now called MDP_policy_evaluation() and gained
  parameter theta as an additional stopping criterion.
* rewrote all accessor code reward_matrix, transition_matrix, observation_matrix
  for better and faster access.
* normalize() gained parameters for more detailed normalization.
* POMDP() and MDP() lost normalize.
* model.h has now support for keywords in transition_prob and observation_prob.
* MDP2POMDP is now make_partially_observable().

## Bugfixes
* q_values_MDP(), solve_MDP(): Fixed reward representation issue.
* reward_val_cpp(): fixed observation matching bug.

# pomdp 1.1.3 (12/20/2023)

## New Features
* simulate_POMDP() and simulate_MDP() gained parameter delta_horizon and 
  calculates now the horizon for infinite-horizon problems.
* added add_policy() and several consistency checks.

## Changes
* Changed the action names for the Maze example to the names used in Russell 
  and Norvig's AIMA book. 
* POMDP lost the parameter max. Costs need to be specified as negative 
  rewards.

## Bugfixes
* simulate_POMDP() now adds terminal values.

# pomdp 1.1.2 (09/07/2023)

## Bugfixes
* Fixed memory access bug in model.h

# pomdp 1.1.1 (09/04/2023)

## Changes
* plot_policy_graph(): The parameter order has slightly changed; belief_col is now called state_col; 
    unreachable states are  now suppressed.
* policy() gained parameters alpha and action.
* color palettes are now exported.
* POMPD accessors gain parameter drop.
* POMDP constructor and read_POMDP gained parameter normalize and, by default, normalize
  the POMDP definition.

## New Features
* Large POMDP descriptions are now handled better by keeping the reward as a data.frame and
  supporting sparse matrices in the C++ code.
* New function value_function() to access alpha vectors.
* New function regret() to calculate the regret of a policy.
* transition_graph() to visualize the transition model.

# pomdp 1.1.0 (01/23/2023)

## New Features
* Added C++ (Rcpp) support. Speed up for simulate_POMDP, sample_belief_space, reward, ... 
* simulate_POMDP and sample_belief_space now have parallel (foreach) support.
* Sparse matrices from package Matrix for matrices with a density below 50%.
* Added support to parse matrices for POMDP files.
* Added model normalization.
* is_solved_POMDP(), is_converged_POMDP(), is_timedependent_POMDP(), and is_solved_MDP() are now exported.

## Changes
* accessors are now called now transition_val() and observation_val().
* simulate_POMDP() and simulate_MDP() now return a list.
* reimplemented round_stochastic() to improve speed.
* MDP policy now uses factors for actions.
* estimate_belief_for_nodes() now can also use trajectories to estimate beliefs faster.
* cleaned up the interface for episodes and epochs.


# pomdp 1.0.3 (05/18/2022)
* Fixed rounding issue on some architectures.

# pomdp 1.0.2 (05/17/2022)

* policy_graph() can now produce policy trees for finite-horizon problems and the initial belief can be specified.
* simulate_POMDP(): fixed bug with not using horizon.
* reward() and reward_node_action() have now been separated.
* sample_belief_space() gained method 'trajectories'.
* simulate_POMDP(): supports not epsilon-greedy policies.
* added x_prob() and x_val() functions to access individual parts of the matrices.
* fixed converged finite-horizon case. It now only returns the converged graph/alpha.
* we use not internally NA to represent * in the POMDP definition.
* actions, states and observations are now factors in most places.

# pomdp 1.0.1 (03/25/2022)

* Fixed rounding issue on some architectures.
* Fixed bug in write_POMDP() (reported by emile-pelletier-gc).
* estimate_belief_for_nodes() is now exposed and the code has been improved.

# pomdp 1.0.0 (02/23/2022)

* POMDP objects now have no list element model, but are the model list directly.
* moved pomdp-solve to package pomdpSolve.
* added solve_MDP().
* transition probability, observation probabilities and rewards can now
  be specified as a function.
* transition_matrix et al now can also return a function.
* Improved POMDP file writer.

# pomdp 0.99.3 (08/05/2021)

* moved Ternary and visNetwork to SUGGESTED.
* removed clang warning for lex scanners.

# pomdp 0.99.2 (05/14/2021)

## Bugfix
* Removed nonportable flag -C from Makefile.

# pomdp 0.99.1 (05/13/2021)

## New Features
* Added a wrapper for the sarsop library.

## Changes
* Improved error messages when accessing fields not parsed by read_POMDP.
* policy() no longer returns the graph, but just alphas and the optimal action.
* The maintainer is now mhahsler.

## Bugfix
* Resolved issues with factors for R 4.0. We now mostly use character instead of factors.
* States and actions as numbers are now handled correctly (reported by meeheal).
* Added spelling fixes by brianrice2.
* Fixed buffer overflow for filename parameters in pomdpsolve.

# pomdp 0.99.0 (05/04/2020)

## Changes
* Support finite-horizon POMDPs and store epochs.
* reward now looks at different epochs, calculates the optimal actions and the parameter names are improved.
* solve_POMDP not looks at convergence.
* solve_POMDP gained parameter terminal_values.
* solve_POMDP gained parameter discount to overwrite the discount rate specified in the model.
* solve_POMDP can now solve POMDPs with time-dependent transition probabilities, observation probabilities and reward structure. 
* solve_POMDP gained parameter grid in parameter list to specify a custom belief point grid for the grid method.
* write_POMDP and solve_POMDP gained parameter digits.
* added read_POMDP to read POMDP files. 
* plot for POMDP is now replaced by plot_policy_graph.
* added policy graph visualization with visNetwork.
* added plot_value_function.
* added function sample_belief_space to sample from the belief space.
* added function plot_belief_space.
* added function transition_matrix.
* added function observation_matrix.
* added function reward_matrix.
* POMDP model now also contains horizon and terminal_values.
* added MDP formulated as a POMDP.
* added policy function to extract a better readable policy.
* added update_belief.
* added simulate_POMDP.
* added round_stochastic.
* added optimal_action.

# pomdp 0.9.2 (12/06/2019)

## Changes
* solve_POMDP can now solve POMDP files.
* added helper functions O, R and T.
* improved plot.
* Added reward function.
* values argument is now called max.
* Fixed class structure. The central class is not POMDP with elements model and solution.

## Bugfix
* fixed warning for start = "uniform".
* fixed warning in C code for gcc10.

# pomdp 0.9.1-1 (05/14/2019)

## Bugfix
* fixed warning in mdp.c for gcc9.

# pomdp 0.9.1 (01/02/2019)

## Bugfix
* Fixed Warning in fg-params.c

## New Features
* New method transitions to extract the transition matrix from a POMDP.

# pomdp 0.9.0 (12/25/2018)

Initial CRAN release.
