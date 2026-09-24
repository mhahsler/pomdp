# Convert between MDPs and POMDPs

Convert an MDP into a POMDP by adding an observation model or a POMDP
into an MDP by making the states observable.

## Usage

``` r
make_partially_observable(x, observations = NULL, observation_prob = NULL)

make_fully_observable(x)
```

## Arguments

- x:

  a `MDP` object.

- observations:

  a character vector specifying the names of the available observations.

- observation_prob:

  Specifies the observation probabilities (see
  [POMDP](http://michael.hahsler.net/pomdp/reference/POMDP.md) for
  details).

## Value

a `MDP` or a `POMDP` object.

## Details

`make_partially_observable()` adds an observation model to an MDP. If no
observations and observation probabilities are provided, then an
observation for each state is created with identity observation
matrices. This means we have a fully observable model encoded as a
POMDP.

`make_fully_observable()` removes the observation model from a POMDP and
returns an MDP.

## See also

Other MDP: [`MDP()`](http://michael.hahsler.net/pomdp/reference/MDP.md),
[`MDP_policy_functions`](http://michael.hahsler.net/pomdp/reference/MDP_policy_functions.md),
[`accessors`](http://michael.hahsler.net/pomdp/reference/accessors.md),
[`actions()`](http://michael.hahsler.net/pomdp/reference/actions.md),
[`add_policy()`](http://michael.hahsler.net/pomdp/reference/add_policy.md),
[`gridworld`](http://michael.hahsler.net/pomdp/reference/gridworld.md),
[`reachable_and_absorbing`](http://michael.hahsler.net/pomdp/reference/reachable_and_absorbing.md),
[`regret()`](http://michael.hahsler.net/pomdp/reference/regret.md),
[`simulate_MDP()`](http://michael.hahsler.net/pomdp/reference/simulate_MDP.md),
[`solve_MDP()`](http://michael.hahsler.net/pomdp/reference/solve_MDP.md),
[`transition_graph()`](http://michael.hahsler.net/pomdp/reference/transition_graph.md),
[`value_function()`](http://michael.hahsler.net/pomdp/reference/value_function.md)

Other POMDP:
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
[`value_function()`](http://michael.hahsler.net/pomdp/reference/value_function.md),
[`write_POMDP()`](http://michael.hahsler.net/pomdp/reference/write_POMDP.md)

## Author

Michael Hahsler

## Examples

``` r
# Turn the Maze MDP into a partially observable problem.
# Here each state has an observation, so it is still a fully observable problem
# encoded as a POMDP.
data("Maze")
Maze
#> MDP, list - Stuart Russell's 3x4 Maze
#>   Discount factor: 1
#>   Horizon: Inf epochs
#>   Size: 11 states / 4 actions
#>   Start: 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0
#> 
#>   List components: ‘name’, ‘discount’, ‘horizon’, ‘states’, ‘actions’,
#>     ‘transition_prob’, ‘reward’, ‘info’, ‘start’

Maze_POMDP <- make_partially_observable(Maze)
Maze_POMDP
#> POMDP, list - Stuart Russell's 3x4 Maze
#>   Discount factor: 1
#>   Horizon: Inf epochs
#>   Size: 11 states / 4 actions / 11 obs.
#>   Start: 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0
#>   Solved: FALSE
#> 
#>   List components: ‘name’, ‘discount’, ‘horizon’, ‘states’, ‘actions’,
#>     ‘transition_prob’, ‘reward’, ‘info’, ‘start’, ‘observations’,
#>     ‘observation_prob’

sol <- solve_POMDP(Maze_POMDP)
policy(sol)
#>      s(1,1)    s(2,1)    s(3,1)    s(1,2)    s(3,2)    s(1,3)     s(2,3)
#> 1 0.8065582 0.7609332 0.7109332 0.8228082 0.6953082 0.8520548  0.6811416
#> 2 0.8171832 0.8015582 0.7453082 0.8671832 0.6559189 0.9210274  0.7002740
#> 3 0.8515582 0.7609332 0.6709329 0.9078082 0.6201933 0.9578082 -0.6470777
#>      s(3,3) s(1,4) s(2,4)     s(3,4) action
#> 1 0.6514155      0      0  0.4279249   left
#> 2 0.6325425      0      0 -0.7000660     up
#> 3 0.4375073      0      0  0.2491308  right
simulate_POMDP(sol, n = 1, horizon = 100, return_trajectories = TRUE)$trajectories
#>    episode time simulation_state alpha_vector_id     a      o     r
#> 1        1    0           s(3,1)               2    up s(2,1) -0.04
#> 2        1    1           s(2,1)               2    up s(1,1) -0.04
#> 3        1    2           s(1,1)               3 right s(2,1) -0.04
#> 4        1    3           s(2,1)               2    up s(2,1) -0.04
#> 5        1    4           s(2,1)               2    up s(1,1) -0.04
#> 6        1    5           s(1,1)               3 right s(2,1) -0.04
#> 7        1    6           s(2,1)               2    up s(1,1) -0.04
#> 8        1    7           s(1,1)               3 right s(1,2) -0.04
#> 9        1    8           s(1,2)               3 right s(1,2) -0.04
#> 10       1    9           s(1,2)               3 right s(1,2) -0.04
#> 11       1   10           s(1,2)               3 right s(1,3) -0.04
#> 12       1   11           s(1,3)               3 right s(1,4)  1.00

# Make the Tiger POMDP fully observable
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

Tiger_MDP <- make_fully_observable(Tiger)
Tiger_MDP
#> MDP, list - Tiger Problem
#>   Discount factor: 0.75
#>   Horizon: Inf epochs
#>   Size: 2 states / 3 actions
#>   Start: uniform
#> 
#>   List components: ‘name’, ‘discount’, ‘horizon’, ‘states’, ‘actions’,
#>     ‘transition_prob’, ‘reward’, ‘start’, ‘terminal_values’, ‘info’

sol <- solve_MDP(Tiger_MDP)
policy(sol)
#>         state        U     action
#> 1  tiger-left 39.99048 open-right
#> 2 tiger-right 39.99048  open-left
# The result is not exciting since we can observe where the tiger is!
```
