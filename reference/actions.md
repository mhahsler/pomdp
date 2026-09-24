# Available Actions

Determine the set of actions available in a state.

## Usage

``` r
actions(x, state)
```

## Arguments

- x:

  a `MDP` pr `POMDP` object.

- state:

  a character vector of length one specifying the state.

## Value

a character vector with the available actions.

a vector with the available actions.

## Details

Unavailable actions are modeled here a actions that have an immediate
reward of `-Inf` in the reward function.

## See also

Other MDP: [`MDP()`](http://michael.hahsler.net/pomdp/reference/MDP.md),
[`MDP2POMDP`](http://michael.hahsler.net/pomdp/reference/MDP2POMDP.md),
[`MDP_policy_functions`](http://michael.hahsler.net/pomdp/reference/MDP_policy_functions.md),
[`accessors`](http://michael.hahsler.net/pomdp/reference/accessors.md),
[`add_policy()`](http://michael.hahsler.net/pomdp/reference/add_policy.md),
[`gridworld`](http://michael.hahsler.net/pomdp/reference/gridworld.md),
[`reachable_and_absorbing`](http://michael.hahsler.net/pomdp/reference/reachable_and_absorbing.md),
[`regret()`](http://michael.hahsler.net/pomdp/reference/regret.md),
[`simulate_MDP()`](http://michael.hahsler.net/pomdp/reference/simulate_MDP.md),
[`solve_MDP()`](http://michael.hahsler.net/pomdp/reference/solve_MDP.md),
[`transition_graph()`](http://michael.hahsler.net/pomdp/reference/transition_graph.md),
[`value_function()`](http://michael.hahsler.net/pomdp/reference/value_function.md)

Other POMDP:
[`MDP2POMDP`](http://michael.hahsler.net/pomdp/reference/MDP2POMDP.md),
[`POMDP()`](http://michael.hahsler.net/pomdp/reference/POMDP.md),
[`accessors`](http://michael.hahsler.net/pomdp/reference/accessors.md),
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
data(RussianTiger)

# The normal actions are "listen", "open-left", and "open-right".
# In the state "done" only the action "nothing" is available. 

actions(RussianTiger, state = "tiger-left")
#> [1] "listen"     "open-left"  "open-right"
actions(RussianTiger, state = "tiger-right")
#> [1] "listen"     "open-left"  "open-right"
actions(RussianTiger, state = "done")
#> [1] "nothing"
```
