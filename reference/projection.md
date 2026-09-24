# Defining a Belief Space Projection

High dimensional belief spaces can be projected to lower dimension. This
is useful for visualization and to analyze the belief space and value
functions. This definition is used by functions like
[`plot_belief_space()`](http://michael.hahsler.net/pomdp/reference/plot_belief_space.md),
[`plot_value_function()`](http://michael.hahsler.net/pomdp/reference/value_function.md),
and
[`sample_belief_space()`](http://michael.hahsler.net/pomdp/reference/sample_belief_space.md).

## Usage

``` r
projection(x = NULL, model)
```

## Arguments

- x:

  specification of the projection (see Details section).

- model:

  a [POMDP](http://michael.hahsler.net/pomdp/reference/POMDP.md).

## Value

a canonical description of the projection.

## Details

The belief space is \$n-1\$ dimensional, were \$n\$ is the number of
states. Note: it is n-1 dimensional since the probabilities need to add
up to 1. A projection fixes the belief value for a set of states. For
example, for a 4-state POMDP (s1, s2, s3, s4), we can project the belief
space on s1 and s2 by holding s3 and s4 constant which is represented by
the vector `c(s1 = NA, s2 = NA, s3 = 0, s4 = .1)`. We use `NA` to
represent that the values are not fixed and the value that the other
dimensions are held constant.

We provide several ways to specify a projection:

- A vector with values for all dimensions. `NA`s are used for the
  dimension projected on. This is the canonical form used in this
  package. Example: `c(NA, NA, 0, .1)`

- A named vector with just the dimensions held constant. Example:
  `c(s3 = 0, s4 = .1)`

- A vector of state names to project on. All other dimensions are held
  constant at 0. Example: `c("s1", "s2")`

- A vector with indices of the states to project on. All other
  dimensions are held constant at 0. Example: `c(1, 2)`

## See also

Other policy:
[`estimate_belief_for_nodes()`](http://michael.hahsler.net/pomdp/reference/estimate_belief_for_nodes.md),
[`optimal_action()`](http://michael.hahsler.net/pomdp/reference/optimal_action.md),
[`plot_belief_space()`](http://michael.hahsler.net/pomdp/reference/plot_belief_space.md),
[`plot_policy_graph()`](http://michael.hahsler.net/pomdp/reference/plot_policy_graph.md),
[`policy()`](http://michael.hahsler.net/pomdp/reference/policy.md),
[`policy_graph()`](http://michael.hahsler.net/pomdp/reference/policy_graph.md),
[`reward()`](http://michael.hahsler.net/pomdp/reference/reward.md),
[`solve_POMDP()`](http://michael.hahsler.net/pomdp/reference/solve_POMDP.md),
[`solve_SARSOP()`](http://michael.hahsler.net/pomdp/reference/solve_SARSOP.md),
[`value_function()`](http://michael.hahsler.net/pomdp/reference/value_function.md)

Other POMDP:
[`MDP2POMDP`](http://michael.hahsler.net/pomdp/reference/MDP2POMDP.md),
[`POMDP()`](http://michael.hahsler.net/pomdp/reference/POMDP.md),
[`accessors`](http://michael.hahsler.net/pomdp/reference/accessors.md),
[`actions()`](http://michael.hahsler.net/pomdp/reference/actions.md),
[`add_policy()`](http://michael.hahsler.net/pomdp/reference/add_policy.md),
[`plot_belief_space()`](http://michael.hahsler.net/pomdp/reference/plot_belief_space.md),
[`reachable_and_absorbing`](http://michael.hahsler.net/pomdp/reference/reachable_and_absorbing.md),
[`regret()`](http://michael.hahsler.net/pomdp/reference/regret.md),
[`sample_belief_space()`](http://michael.hahsler.net/pomdp/reference/sample_belief_space.md),
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
model <- POMDP(
 states = 4,
 actions = 2,
 observations = 2,
 transition_prob = list("identity","identity"),
 observation_prob = list("uniform","uniform"),
 reward = rbind(R_(value = 1))
)

projection(NULL, model = model)
#> s1 s2 s3 s4 
#> NA NA NA NA 
projection(1:2, model = model)
#> s1 s2 s3 s4 
#> NA NA  0  0 
projection(c("s2", "s3"), model = model)
#> s1 s2 s3 s4 
#>  0 NA NA  0 
projection(c(1,4), model = model)
#> s1 s2 s3 s4 
#> NA  0  0 NA 
projection(c(s2 = .4, s3 = .2), model = model)
#>  s1  s2  s3  s4 
#>  NA 0.4 0.2  NA 
projection(c(s1 = .1, s2 = NA, s3 = NA, s4 = .3), model = model)
#>  s1  s2  s3  s4 
#> 0.1  NA  NA 0.3 
```
