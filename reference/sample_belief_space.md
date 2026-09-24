# Sample from the Belief Space

Sample points from belief space using a several sampling strategies.

## Usage

``` r
sample_belief_space(model, projection = NULL, n = 1000, method = "random", ...)
```

## Arguments

- model:

  a unsolved or solved
  [POMDP](http://michael.hahsler.net/pomdp/reference/POMDP.md).

- projection:

  Sample in a projected belief space. See
  [`projection()`](http://michael.hahsler.net/pomdp/reference/projection.md)
  for details.

- n:

  size of the sample. For trajectories, it is the number of
  trajectories.

- method:

  character string specifying the sampling strategy. Available are
  `"random"`, `"regular"`, and `"trajectories"`.

- ...:

  for the trajectory method, further arguments are passed on to
  [`simulate_POMDP()`](http://michael.hahsler.net/pomdp/reference/simulate_POMDP.md).
  Further arguments are ignored for the other methods.

## Value

Returns a matrix. Each row is a sample from the belief space.

## Details

The purpose of sampling from the belief space is to provide good
coverage or to sample belief points that are more likely to be
encountered (see trajectory method). The following sampling methods are
available:

- `'random'` samples uniformly sample from the projected belief space
  using the method described by Luc Devroye (1986). Sampling is be done
  in parallel after a foreach backend is registered.

- `'regular'` samples points using a regularly spaced grid. This method
  is only available for projections on 2 or 3 states.

- `"trajectories"` returns the belief states encountered in `n`
  trajectories of length `horizon` starting at the model's initial
  belief. Thus it returns `n` x `horizon` belief states and will contain
  duplicates. Projection is not supported for trajectories. Additional
  arguments can include the simulation `horizon` and the start `belief`
  which are passed on to
  [`simulate_POMDP()`](http://michael.hahsler.net/pomdp/reference/simulate_POMDP.md).

## References

Luc Devroye, Non-Uniform Random Variate Generation, Springer Verlag,
1986.

## See also

Other POMDP:
[`MDP2POMDP`](http://michael.hahsler.net/pomdp/reference/MDP2POMDP.md),
[`POMDP()`](http://michael.hahsler.net/pomdp/reference/POMDP.md),
[`accessors`](http://michael.hahsler.net/pomdp/reference/accessors.md),
[`actions()`](http://michael.hahsler.net/pomdp/reference/actions.md),
[`add_policy()`](http://michael.hahsler.net/pomdp/reference/add_policy.md),
[`plot_belief_space()`](http://michael.hahsler.net/pomdp/reference/plot_belief_space.md),
[`projection()`](http://michael.hahsler.net/pomdp/reference/projection.md),
[`reachable_and_absorbing`](http://michael.hahsler.net/pomdp/reference/reachable_and_absorbing.md),
[`regret()`](http://michael.hahsler.net/pomdp/reference/regret.md),
[`simulate_POMDP()`](http://michael.hahsler.net/pomdp/reference/simulate_POMDP.md),
[`solve_POMDP()`](http://michael.hahsler.net/pomdp/reference/solve_POMDP.md),
[`solve_SARSOP()`](http://michael.hahsler.net/pomdp/reference/solve_SARSOP.md),
[`transition_graph()`](http://michael.hahsler.net/pomdp/reference/transition_graph.md),
[`update_belief()`](http://michael.hahsler.net/pomdp/reference/update_belief.md),
[`value_function()`](http://michael.hahsler.net/pomdp/reference/value_function.md),
[`write_POMDP()`](http://michael.hahsler.net/pomdp/reference/write_POMDP.md)

## Author

Michael Hahsler

## Examples

``` r
data("Tiger")

# random sampling can be done in parallel after registering a backend.
# doparallel::registerDoParallel()

sample_belief_space(Tiger, n = 5)
#>      tiger-left tiger-right
#> [1,]  0.7240369   0.2759631
#> [2,]  0.4813570   0.5186430
#> [3,]  0.3178013   0.6821987
#> [4,]  0.5500549   0.4499451
#> [5,]  0.5327876   0.4672124
sample_belief_space(Tiger, n = 5, method = "regular")
#>      tiger-left tiger-right
#> [1,]       0.00        1.00
#> [2,]       0.25        0.75
#> [3,]       0.50        0.50
#> [4,]       0.75        0.25
#> [5,]       1.00        0.00
sample_belief_space(Tiger, n = 1, horizon = 5, method = "trajectories")
#>      tiger-left tiger-right
#> [1,]  0.5000000   0.5000000
#> [2,]  0.5000000   0.5000000
#> [3,]  0.1500000   0.8500000
#> [4,]  0.0302013   0.9697987
#> [5,]  0.5000000   0.5000000

# sample, determine the optimal action and calculate the expected reward for a solved POMDP
# Note: check.names = FALSE is used to preserve the `-` for the state names in the dataframe.
sol <- solve_POMDP(Tiger)
samp <- sample_belief_space(sol, n = 5, method = "regular")
data.frame(samp, action = optimal_action(sol,  belief = samp), 
  reward = reward(sol, belief = samp), check.names = FALSE)
#>   tiger-left tiger-right     action    reward
#> 1       0.00        1.00  open-left 11.450079
#> 2       0.25        0.75     listen  2.174128
#> 3       0.50        0.50     listen  1.933439
#> 4       0.75        0.25     listen  2.174128
#> 5       1.00        0.00 open-right 11.450079
  
# sample from a 3 state problem
data(Three_doors)
Three_doors
#> POMDP, list - 3-Door Tiger Problem
#>   Discount factor: 0.75
#>   Horizon: Inf epochs
#>   Size: 3 states / 4 actions / 3 obs.
#>   Start: uniform
#>   Solved: FALSE
#> 
#>   List components: ‘name’, ‘discount’, ‘horizon’, ‘states’, ‘actions’,
#>     ‘observations’, ‘transition_prob’, ‘observation_prob’, ‘reward’,
#>     ‘start’, ‘terminal_values’, ‘info’

sample_belief_space(Three_doors, n = 5)
#>      tiger-left tiger-center tiger-right
#> [1,]  0.4022891   0.36401638  0.23369452
#> [2,]  0.3081257   0.62468412  0.06719019
#> [3,]  0.6866004   0.05401954  0.25938009
#> [4,]  0.6998776   0.13503909  0.16508328
#> [5,]  0.4036538   0.24263614  0.35371010
sample_belief_space(Three_doors, n = 5, projection = c(`tiger-left` = .1))
#>      tiger-left tiger-center tiger-right
#> [1,]        0.1   0.04891462   0.8510854
#> [2,]        0.1   0.36526581   0.5347342
#> [3,]        0.1   0.34979086   0.5502091
#> [4,]        0.1   0.13820147   0.7617985
#> [5,]        0.1   0.38057917   0.5194208

if ("Ternary" %in% installed.packages()) {
sample_belief_space(Three_doors, n = 9, method = "regular")
sample_belief_space(Three_doors, n = 9, method = "regular", projection = c(`tiger-left` = .1))
}
#>       tiger-left tiger-center tiger-right
#>  [1,]        0.1       0.0000      0.9000
#>  [2,]        0.1       0.1125      0.7875
#>  [3,]        0.1       0.2250      0.6750
#>  [4,]        0.1       0.3375      0.5625
#>  [5,]        0.1       0.4500      0.4500
#>  [6,]        0.1       0.5625      0.3375
#>  [7,]        0.1       0.6750      0.2250
#>  [8,]        0.1       0.7875      0.1125
#>  [9,]        0.1       0.9000      0.0000

sample_belief_space(Three_doors, n = 1, horizon = 5, method = "trajectories")
#>      tiger-left tiger-center tiger-right
#> [1,]  0.3333334    0.3333333   0.3333333
#> [2,]  0.3333334    0.3333333   0.3333333
#> [3,]  0.3333334    0.3333333   0.3333333
#> [4,]  0.1875000    0.6250000   0.1875000
#> [5,]  0.3333334    0.3333333   0.3333333
```
