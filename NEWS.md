# pomdp 1.0.1-1 (xx/xx/xxxx)

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
