# Value Function

Extracts the value function from a solved model. Extracts the alpha
vectors describing the value function. This is similar to
[`policy()`](http://michael.hahsler.net/pomdp/reference/policy.md) which
in addition returns the action prescribed by the solution.

## Usage

``` r
value_function(model, drop = TRUE)

plot_value_function(
  model,
  projection = NULL,
  epoch = 1,
  ylim = NULL,
  legend = TRUE,
  col = NULL,
  lwd = 1,
  lty = 1,
  ylab = "Value",
  ...
)
```

## Arguments

- model:

  a solved [POMDP](http://michael.hahsler.net/pomdp/reference/POMDP.md)
  or [MDP](http://michael.hahsler.net/pomdp/reference/MDP.md).

- drop:

  logical; drop the list for converged, epoch-independent value
  functions.

- projection:

  Sample in a projected belief space. See
  [`projection()`](http://michael.hahsler.net/pomdp/reference/projection.md)
  for details.

- epoch:

  the epoch whose value function should be plotted. Use 1 for converged
  policies.

- ylim:

  the y limits of the plot.

- legend:

  logical; show the actions in the visualization?

- col:

  plotting colors.

- lwd:

  line width.

- lty:

  line type.

- ylab:

  label for the y-axis.

- ...:

  additional arguments passed on to
  [`graphics::plot()`](https://rdrr.io/r/graphics/plot.default.html), or
  [`graphics::barplot()`](https://rdrr.io/r/graphics/barplot.html)\`.

## Value

the function as a matrix with alpha vectors as rows.

## Details

Plots the value function of a POMDP solution as a line plot. The
solution is projected on two states (i.e., the belief for the other
states is held constant at zero). The value function can also be
visualized using
[`plot_belief_space()`](http://michael.hahsler.net/pomdp/reference/plot_belief_space.md).

## See also

Other policy:
[`estimate_belief_for_nodes()`](http://michael.hahsler.net/pomdp/reference/estimate_belief_for_nodes.md),
[`optimal_action()`](http://michael.hahsler.net/pomdp/reference/optimal_action.md),
[`plot_belief_space()`](http://michael.hahsler.net/pomdp/reference/plot_belief_space.md),
[`plot_policy_graph()`](http://michael.hahsler.net/pomdp/reference/plot_policy_graph.md),
[`policy()`](http://michael.hahsler.net/pomdp/reference/policy.md),
[`policy_graph()`](http://michael.hahsler.net/pomdp/reference/policy_graph.md),
[`projection()`](http://michael.hahsler.net/pomdp/reference/projection.md),
[`reward()`](http://michael.hahsler.net/pomdp/reference/reward.md),
[`solve_POMDP()`](http://michael.hahsler.net/pomdp/reference/solve_POMDP.md),
[`solve_SARSOP()`](http://michael.hahsler.net/pomdp/reference/solve_SARSOP.md)

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
[`sample_belief_space()`](http://michael.hahsler.net/pomdp/reference/sample_belief_space.md),
[`simulate_POMDP()`](http://michael.hahsler.net/pomdp/reference/simulate_POMDP.md),
[`solve_POMDP()`](http://michael.hahsler.net/pomdp/reference/solve_POMDP.md),
[`solve_SARSOP()`](http://michael.hahsler.net/pomdp/reference/solve_SARSOP.md),
[`transition_graph()`](http://michael.hahsler.net/pomdp/reference/transition_graph.md),
[`update_belief()`](http://michael.hahsler.net/pomdp/reference/update_belief.md),
[`write_POMDP()`](http://michael.hahsler.net/pomdp/reference/write_POMDP.md)

Other MDP: [`MDP()`](http://michael.hahsler.net/pomdp/reference/MDP.md),
[`MDP2POMDP`](http://michael.hahsler.net/pomdp/reference/MDP2POMDP.md),
[`MDP_policy_functions`](http://michael.hahsler.net/pomdp/reference/MDP_policy_functions.md),
[`accessors`](http://michael.hahsler.net/pomdp/reference/accessors.md),
[`actions()`](http://michael.hahsler.net/pomdp/reference/actions.md),
[`add_policy()`](http://michael.hahsler.net/pomdp/reference/add_policy.md),
[`gridworld`](http://michael.hahsler.net/pomdp/reference/gridworld.md),
[`reachable_and_absorbing`](http://michael.hahsler.net/pomdp/reference/reachable_and_absorbing.md),
[`regret()`](http://michael.hahsler.net/pomdp/reference/regret.md),
[`simulate_MDP()`](http://michael.hahsler.net/pomdp/reference/simulate_MDP.md),
[`solve_MDP()`](http://michael.hahsler.net/pomdp/reference/solve_MDP.md),
[`transition_graph()`](http://michael.hahsler.net/pomdp/reference/transition_graph.md)

## Author

Michael Hahsler

## Examples

``` r
data("Tiger")
sol <- solve_POMDP(Tiger)
sol
#> POMDP, list - Tiger Problem
#>   Discount factor: 0.75
#>   Horizon: Inf epochs
#>   Size: 2 states / 3 actions / 2 obs.
#>   Start: uniform
#>   Solved:
#>     Method: ‘grid’
#>     Solution converged: TRUE
#>     # of alpha vectors: 5
#>     Total expected reward: 1.933439
#> 
#>   List components: ‘name’, ‘discount’, ‘horizon’, ‘states’, ‘actions’,
#>     ‘observations’, ‘transition_prob’, ‘observation_prob’, ‘reward’,
#>     ‘start’, ‘info’, ‘solution’

# value function for the converged solution
value_function(sol)
#>      tiger-left tiger-right
#> [1,] -98.549921   11.450079
#> [2,] -10.854299    6.516937
#> [3,]   1.933439    1.933439
#> [4,]   6.516937  -10.854299
#> [5,]  11.450079  -98.549921

plot_value_function(sol, ylim = c(0,20))


## finite-horizon problem
sol <- solve_POMDP(model = Tiger, horizon = 3, discount = 1,
  method = "enum")
sol
#> POMDP, list - Tiger Problem
#>   Discount factor: 1
#>   Horizon: 3 epochs
#>   Size: 2 states / 3 actions / 2 obs.
#>   Start: uniform
#>   Solved:
#>     Method: ‘enum’
#>     Solution converged: FALSE
#>     # of alpha vectors: 15
#>     Total expected reward: 2.720000
#> 
#>   List components: ‘name’, ‘discount’, ‘horizon’, ‘states’, ‘actions’,
#>     ‘observations’, ‘transition_prob’, ‘observation_prob’, ‘reward’,
#>     ‘start’, ‘info’, ‘solution’

# inspect the value function for all epochs
value_function(sol)
#> [[1]]
#>      tiger-left tiger-right
#> [1,]  -102.0000      8.0000
#> [2,]   -30.4725      7.7525
#> [3,]    -5.2275      4.9475
#> [4,]     2.7200      2.7200
#> [5,]     4.9475     -5.2275
#> [6,]     7.7525    -30.4725
#> [7,]     8.0000   -102.0000
#> 
#> [[2]]
#>      tiger-left tiger-right
#> [1,]    -101.00        9.00
#> [2,]     -16.85        7.35
#> [3,]      -2.00       -2.00
#> [4,]       7.35      -16.85
#> [5,]       9.00     -101.00
#> 
#> [[3]]
#>      tiger-left tiger-right
#> [1,]       -100          10
#> [2,]         -1          -1
#> [3,]         10        -100
#> 

plot_value_function(sol, epoch = 1, ylim = c(-5, 25))

plot_value_function(sol, epoch = 2, ylim = c(-5, 25))

plot_value_function(sol, epoch = 3, ylim = c(-5, 25))


if (FALSE) { # \dontrun{
# using ggplot2 to plot the value function for epoch 3
library(ggplot2)
pol <- policy(sol)
ggplot(pol[[3]]) +
 geom_segment(aes(x = 0, y = `tiger-left`, xend = 1, yend = `tiger-right`, color = action)) +
 coord_cartesian(ylim = c(-5, 15)) + ylab("Value") + xlab("Belief space")
} # }
```
