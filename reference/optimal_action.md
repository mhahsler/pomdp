# Optimal action for a belief

Determines the optimal action for a policy (solved POMDP) for a given
belief at a given epoch.

## Usage

``` r
optimal_action(model, belief = NULL, epoch = 1)
```

## Arguments

- model:

  a solved [POMDP](http://michael.hahsler.net/pomdp/reference/POMDP.md).

- belief:

  The belief (probability distribution over the states) as a vector or a
  matrix with multiple belief states as rows. If `NULL`, then the
  initial belief of the model is used.

- epoch:

  what epoch of the policy should be used. Use 1 for converged policies.

## Value

The name of the optimal action.

## See also

Other policy:
[`estimate_belief_for_nodes()`](http://michael.hahsler.net/pomdp/reference/estimate_belief_for_nodes.md),
[`plot_belief_space()`](http://michael.hahsler.net/pomdp/reference/plot_belief_space.md),
[`plot_policy_graph()`](http://michael.hahsler.net/pomdp/reference/plot_policy_graph.md),
[`policy()`](http://michael.hahsler.net/pomdp/reference/policy.md),
[`policy_graph()`](http://michael.hahsler.net/pomdp/reference/policy_graph.md),
[`projection()`](http://michael.hahsler.net/pomdp/reference/projection.md),
[`reward()`](http://michael.hahsler.net/pomdp/reference/reward.md),
[`solve_POMDP()`](http://michael.hahsler.net/pomdp/reference/solve_POMDP.md),
[`solve_SARSOP()`](http://michael.hahsler.net/pomdp/reference/solve_SARSOP.md),
[`value_function()`](http://michael.hahsler.net/pomdp/reference/value_function.md)

## Author

Michael Hahsler

## Examples

``` r
data("Tiger")
Tiger
#> POMDP, list - Tiger Problem
#>   Discount factor: 0.75
#>   Horizon: Inf epochs
#>   Size: 2 states / 3 actions / 2 obs.
#>   Start: uniform
#>   Solved: FALSE
#> 
#>   List components: ‘name’, ‘discount’, ‘horizon’, ‘states’, ‘actions’,
#>     ‘observations’, ‘transition_prob’, ‘observation_prob’, ‘reward’,
#>     ‘start’, ‘terminal_values’, ‘info’

sol <- solve_POMDP(model = Tiger)

# these are the states
sol$states
#> [1] "tiger-left"  "tiger-right"

# belief that tiger is to the left
optimal_action(sol, c(1, 0))
#> [1] open-right
#> Levels: listen open-left open-right
optimal_action(sol, "tiger-left")
#> [1] open-right
#> Levels: listen open-left open-right

# belief that tiger is to the right
optimal_action(sol, c(0, 1))
#> [1] open-left
#> Levels: listen open-left open-right
optimal_action(sol, "tiger-right")
#> [1] open-left
#> Levels: listen open-left open-right

# belief is 50/50
optimal_action(sol, c(.5, .5))
#> [1] listen
#> Levels: listen open-left open-right
optimal_action(sol, "uniform")
#> [1] listen
#> Levels: listen open-left open-right

# the POMDP is converged, so all epoch give the same result.
optimal_action(sol, "tiger-right", epoch = 10)
#> [1] open-left
#> Levels: listen open-left open-right
```
