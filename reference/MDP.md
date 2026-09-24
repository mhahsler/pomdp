# Define an MDP Problem

Defines all the elements of a finite state-space MDP problem.

## Usage

``` r
MDP(
  states,
  actions,
  transition_prob,
  reward,
  discount = 0.9,
  horizon = Inf,
  start = "uniform",
  info = NULL,
  name = NA
)

is_solved_MDP(x, stop = FALSE)
```

## Arguments

- states:

  a character vector specifying the names of the states.

- actions:

  a character vector specifying the names of the available actions.

- transition_prob:

  Specifies the transition probabilities between states.

- reward:

  Specifies the rewards dependent on action, states and observations.

- discount:

  numeric; discount rate between 0 and 1.

- horizon:

  numeric; Number of epochs. `Inf` specifies an infinite horizon.

- start:

  Specifies in which state the MDP starts.

- info:

  A list with additional information.

- name:

  a string to identify the MDP problem.

- x:

  a `MDP` object.

- stop:

  logical; stop with an error.

## Value

An object of class MDP containing the model specification.
[`solve_MDP()`](http://michael.hahsler.net/pomdp/reference/solve_MDP.md)
reads the object and adds a list element called `'solution'`.

## Details

Markov decision processes (MDPs) are discrete-time stochastic control
processes with completely observable states. Here, we implement MDPs
with a finite state space, similar to
[POMDP](http://michael.hahsler.net/pomdp/reference/POMDP.md) models, but
without the observation model. The `'observations'` column in the reward
specification is always missing.

[`make_partially_observable()`](http://michael.hahsler.net/pomdp/reference/MDP2POMDP.md)
reformulates an MDP as a POMDP by adding an observation model with one
observation per state that reveals the current state. This is achieved
by adding identity observation probability matrices.

More details on specifying the model components can be found in the
documentation for
[POMDP](http://michael.hahsler.net/pomdp/reference/POMDP.md).

## See also

Other MDP:
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
[`transition_graph()`](http://michael.hahsler.net/pomdp/reference/transition_graph.md),
[`value_function()`](http://michael.hahsler.net/pomdp/reference/value_function.md)

Other MDP_examples:
[`Cliff_walking`](http://michael.hahsler.net/pomdp/reference/Cliff_walking.md),
[`DynaMaze`](http://michael.hahsler.net/pomdp/reference/DynaMaze.md),
[`Maze`](http://michael.hahsler.net/pomdp/reference/Maze.md),
[`Windy_gridworld`](http://michael.hahsler.net/pomdp/reference/Windy_gridworld.md)

## Author

Michael Hahsler

## Examples

``` r
# Michael's Sleepy Tiger Problem is like the POMDP Tiger problem, but
# has completely observable states because the tiger is sleeping in front
# of the door. This makes the problem an MDP.

STiger <- MDP(
  name = "Michael's Sleepy Tiger Problem",
  discount = .9,

  states = c("tiger-left" , "tiger-right"),
  actions = c("open-left", "open-right", "do-nothing"),
  start = "uniform",

  # opening a door resets the problem
  transition_prob = list(
    "open-left" =  "uniform",
    "open-right" = "uniform",
    "do-nothing" = "identity"),

  # the reward helper R_() expects: action, start.state, end.state, observation, value
  reward = rbind(
    R_("open-left",  "tiger-left",  value = -100),
    R_("open-left",  "tiger-right", value =   10),
    R_("open-right", "tiger-left",  value =   10),
    R_("open-right", "tiger-right", value = -100),
    R_("do-nothing",                value =    0)
  )
)

STiger
#> MDP, list - Michael's Sleepy Tiger Problem
#>   Discount factor: 0.9
#>   Horizon: Inf epochs
#>   Size: 2 states / 3 actions
#>   Start: uniform
#> 
#>   List components: ‘name’, ‘discount’, ‘horizon’, ‘states’, ‘actions’,
#>     ‘transition_prob’, ‘reward’, ‘info’, ‘start’

sol <- solve_MDP(STiger)
sol
#> MDP, list - Michael's Sleepy Tiger Problem
#>   Discount factor: 0.9
#>   Horizon: Inf epochs
#>   Size: 2 states / 3 actions
#>   Start: uniform
#>   Solved:
#>     Method: ‘value iteration’
#>     Solution converged: TRUE
#> 
#>   List components: ‘name’, ‘discount’, ‘horizon’, ‘states’, ‘actions’,
#>     ‘transition_prob’, ‘reward’, ‘info’, ‘start’, ‘solution’

policy(sol)
#>         state       U     action
#> 1  tiger-left 99.9906 open-right
#> 2 tiger-right 99.9906  open-left
plot_value_function(sol)


# convert the MDP into a POMDP and solve
STiger_POMDP <- make_partially_observable(STiger)
sol2 <- solve_POMDP(STiger_POMDP)
sol2
#> POMDP, list - Michael's Sleepy Tiger Problem
#>   Discount factor: 0.9
#>   Horizon: Inf epochs
#>   Size: 2 states / 3 actions / 2 obs.
#>   Start: uniform
#>   Solved:
#>     Method: ‘grid’
#>     Solution converged: TRUE
#>     # of alpha vectors: 3
#>     Total expected reward: 90.000000
#> 
#>   List components: ‘name’, ‘discount’, ‘horizon’, ‘states’, ‘actions’,
#>     ‘transition_prob’, ‘reward’, ‘info’, ‘start’, ‘observations’,
#>     ‘observation_prob’, ‘solution’

policy(sol2)
#>   tiger-left tiger-right     action
#> 1        -10         100  open-left
#> 2         90          90 do-nothing
#> 3        100         -10 open-right
plot_value_function(sol2, ylim = c(80, 120))
```
