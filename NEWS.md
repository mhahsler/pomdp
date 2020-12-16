# pomdp 0.99.0-1 (xx/xx/2020)

## Changes
* Added support for the SARSOP solver.
* Improved error messages when accessing fields not parsed by read_POMDP.

## Bugfix
* Resolved issues with factors for R 4.0. We now mostly use character instead of factors.
* States and actions as numbers are now handled correctly (reported by meeheal).
* Added spelling fixes by brianrice2. 

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
* added policy graph visualization with vizNetwork.
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
