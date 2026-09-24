# Reachable and Absorbing States

Find reachable and absorbing states in the transition model.

## Usage

``` r
reachable_states(x, states = NULL)

absorbing_states(x, states = NULL)

remove_unreachable_states(x)
```

## Arguments

- x:

  a `MDP` pr `POMDP` object.

- states:

  a character vector specifying the names of the states to be checked.
  `NULL` checks all states.

## Value

`reachable_states()` returns a logical vector indicating if the states
are reachable.

`absorbing_states()` returns a logical vector indicating if the states
are absorbing (terminal).

the model with all unreachable states removed

## Details

The function `reachable_states()` checks if states are reachable using
the transition model.

The function `absorbing_states()` checks if a state or a set of states
are absorbing (terminal states) with a zero reward (or `-Inf` for
unavailable actions). If no states are specified (`states = NULL`), then
all model states are checked. This information can be used in
simulations to end an episode.

The function `remove_unreachable_states()` simplifies a model by
removing unreachable states.

## See also

Other MDP: [`MDP()`](http://michael.hahsler.net/pomdp/reference/MDP.md),
[`MDP2POMDP`](http://michael.hahsler.net/pomdp/reference/MDP2POMDP.md),
[`MDP_policy_functions`](http://michael.hahsler.net/pomdp/reference/MDP_policy_functions.md),
[`accessors`](http://michael.hahsler.net/pomdp/reference/accessors.md),
[`actions()`](http://michael.hahsler.net/pomdp/reference/actions.md),
[`add_policy()`](http://michael.hahsler.net/pomdp/reference/add_policy.md),
[`gridworld`](http://michael.hahsler.net/pomdp/reference/gridworld.md),
[`regret()`](http://michael.hahsler.net/pomdp/reference/regret.md),
[`simulate_MDP()`](http://michael.hahsler.net/pomdp/reference/simulate_MDP.md),
[`solve_MDP()`](http://michael.hahsler.net/pomdp/reference/solve_MDP.md),
[`transition_graph()`](http://michael.hahsler.net/pomdp/reference/transition_graph.md),
[`value_function()`](http://michael.hahsler.net/pomdp/reference/value_function.md)

Other POMDP:
[`MDP2POMDP`](http://michael.hahsler.net/pomdp/reference/MDP2POMDP.md),
[`POMDP()`](http://michael.hahsler.net/pomdp/reference/POMDP.md),
[`accessors`](http://michael.hahsler.net/pomdp/reference/accessors.md),
[`actions()`](http://michael.hahsler.net/pomdp/reference/actions.md),
[`add_policy()`](http://michael.hahsler.net/pomdp/reference/add_policy.md),
[`plot_belief_space()`](http://michael.hahsler.net/pomdp/reference/plot_belief_space.md),
[`projection()`](http://michael.hahsler.net/pomdp/reference/projection.md),
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
data(Maze)

gridworld_matrix(Maze, what = "label")
#>      [,1]    [,2] [,3] [,4]      
#> [1,] ""      ""   ""   "Goal: +1"
#> [2,] ""      "X"  ""   "-1"      
#> [3,] "Start" ""   ""   ""        

# the states marked with +1 and -1 are absorbing
absorbing_states(Maze)
#> s(1,1) s(2,1) s(3,1) s(1,2) s(3,2) s(1,3) s(2,3) s(3,3) s(1,4) s(2,4) s(3,4) 
#>  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE   TRUE   TRUE  FALSE 
which(absorbing_states(Maze))
#> s(1,4) s(2,4) 
#>      9     10 

# all states in the model are reachable
reachable_states(Maze)
#> s(1,1) s(2,1) s(3,1) s(1,2) s(3,2) s(1,3) s(2,3) s(3,3) s(1,4) s(2,4) s(3,4) 
#>   TRUE   TRUE   TRUE   TRUE   TRUE   TRUE   TRUE   TRUE   TRUE   TRUE   TRUE 
which(!reachable_states(Maze))
#> named integer(0)
```
