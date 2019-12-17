# pomdp 0.9.2-1 (xx/xx/2019)

## Changes
* reward now looks at different epochs, calulates the optimal actions and the paramater names are improved.
* solve_POMDP not looks at convergence.
* solve_POMDP gained parameter discount to overwrite the discount rate specified in the model.
* added policy graph visualization with vizNetwork.
* Many improvements to support finite-horizon POMDPs and store epochs.


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
