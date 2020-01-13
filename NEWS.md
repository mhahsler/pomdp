# pomdp 0.9.2-1 (xx/xx/2019)

## Changes
* Many improvements to support finite-horizon POMDPs and store epochs.
* reward now looks at different epochs, calulates the optimal actions and the paramater names are improved.
* solve_POMDP not looks at convergence.
* solve_POMDP gained paramter terminal_values.
* solve_POMDP gained parameter discount to overwrite the discount rate specified in the model.
* added read_POMDP to read POMDP files. 
* added policy graph visualization with vizNetwork.
* added plot_value_function.
* added function sample_belief_space to sample from the belief space.
* added function plot_belief_space.
* added function transition_matrix.
* added function observation_matrix.


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
