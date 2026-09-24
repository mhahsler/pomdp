# Define a POMDP Problem

Defines all the elements of a POMDP problem including the discount rate,
the set of states, the set of actions, the set of observations, the
transition probabilities, the observation probabilities, and rewards.

## Usage

``` r
POMDP(
  states,
  actions,
  observations,
  transition_prob,
  observation_prob,
  reward,
  discount = 0.9,
  horizon = Inf,
  terminal_values = NULL,
  start = "uniform",
  info = NULL,
  name = NA
)

is_solved_POMDP(x, stop = FALSE, message = "")

is_timedependent_POMDP(x)

epoch_to_episode(x, epoch)

is_converged_POMDP(x, stop = FALSE, message = "")

O_(action = NA, end.state = NA, observation = NA, probability)

T_(action = NA, start.state = NA, end.state = NA, probability)

R_(action = NA, start.state = NA, end.state = NA, observation = NA, value)
```

## Arguments

- states:

  a character vector specifying the names of the states. Note that state
  names have to start with a letter.

- actions:

  a character vector specifying the names of the available actions. Note
  that action names have to start with a letter.

- observations:

  a character vector specifying the names of the observations. Note that
  observation names have to start with a letter.

- transition_prob:

  Specifies action-dependent transition probabilities between states.
  See Details section.

- observation_prob:

  Specifies the probability that an action/state combination produces an
  observation. See Details section.

- reward:

  Specifies the rewards structure dependent on action, states and
  observations. See Details section.

- discount:

  numeric; discount factor between 0 and 1.

- horizon:

  numeric; Number of epochs. `Inf` specifies an infinite horizon.

- terminal_values:

  a vector with the terminal values for each state or a matrix
  specifying the terminal rewards via a terminal value function (e.g.,
  the alpha component produced by
  [`solve_POMDP()`](http://michael.hahsler.net/pomdp/reference/solve_POMDP.md)).
  A single 0 specifies that all terminal values are zero.

- start:

  Specifies the initial belief state of the agent. A vector with the
  probability for each state is supplied. Also the string `'uniform'`
  (default) can be used. The belief is used to calculate the total
  expected cumulative reward. It is also used by some solvers. See
  Details section for more information.

- info:

  A list with additional information.

- name:

  a string to identify the POMDP problem.

- x:

  a POMDP.

- stop:

  logical; stop with an error.

- message:

  a error message to be displayed displayed

- epoch:

  integer; an epoch that should be converted to the corresponding
  episode in a time-dependent POMDP.

- action, start.state, end.state, observation, probability, value:

  Values used in the helper functions `O_()`, `R_()`, and `T_()` to
  create an entry for `observation_prob`, `reward`, or `transition_prob`
  above, respectively. The default value `'*"'` matches any
  action/state/observation.

## Value

An object of class POMDP containing the model specification.
[`solve_POMDP()`](http://michael.hahsler.net/pomdp/reference/solve_POMDP.md)
reads the object and adds a list element named `'solution'`.

## Details

In the following we use the following notation. The POMDP is a 7-duple:

\\(S,A,T,R, \Omega ,O, \gamma)\\.

\\S\\ is the set of states; \\A\\ is the set of actions; \\T\\ are the
conditional transition probabilities between states; \\R\\ is the reward
function; \\\Omega\\ is the set of observations; \\O\\ are the
conditional observation probabilities; and \\\gamma\\ is the discount
factor. We will use lower case letters to represent a member of a set,
e.g., \\s\\ is a specific state. To refer to the size of a set we will
use cardinality, e.g., the number of actions is \\\|A\|\\.

Note that the observation model is in the literature often also denoted
by the letter \\Z\\.

**Names used for mathematical symbols in code**

- \\S, s, s'\\: `'states', start.state', 'end.state'`

- \\A, a\\: `'actions', 'action'`

- \\\Omega, o\\: `'observations', 'observation'`

State names, actions and observations can be specified as strings or
index numbers (e.g., `start.state` can be specified as the index of the
state in `states`). For the specification as data.frames below, `NA` can
be used to mean any `start.state`, `end.state`, `action` or
`observation`. Note that some POMDP solvers and the POMDP file format
use `'*'` for this purpose.

The specification below map to the format used by pomdp-solve (see
<http://www.pomdp.org>).

**Specification of transition probabilities: \\T(s' \| s, a)\\**

Transition probability to transition to state \\s'\\ from given state
\\s\\ and action \\a\\. The transition probabilities can be specified in
the following ways:

- A data.frame with columns exactly like the arguments of `T_()`. You
  can use [`rbind()`](https://rdrr.io/r/base/cbind.html) with helper
  function `T_()` to create this data frame. Probabilities can be
  specified multiple times and the definition that appears last in the
  data.frame will take effect.

- A named list of matrices, one for each action. Each matrix is square
  with rows representing start states \\s\\ and columns representing end
  states \\s'\\. Instead of a matrix, also the strings `'identity'` or
  `'uniform'` can be specified.

- A function with the same arguments as `T_()`, but no default values
  that returns the transition probability.

**Specification of observation probabilities: \\O(o \| a, s')\\**

The POMDP specifies the probability for each observation \\o\\ given an
action \\a\\ and that the system transitioned to the end state \\s'\\.
These probabilities can be specified in the following ways:

- A data frame with columns named exactly like the arguments of `O_()`.
  You can use [`rbind()`](https://rdrr.io/r/base/cbind.html) with helper
  function `O_()` to create this data frame. Probabilities can be
  specified multiple times and the definition that appears last in the
  data.frame will take effect.

- A named list of matrices, one for each action. Each matrix has rows
  representing end states \\s'\\ and columns representing an observation
  \\o\\. Instead of a matrix, also the string `'uniform'` can be
  specified.

- A function with the same arguments as `O_()`, but no default values
  that returns the observation probability.

**Specification of the reward function: \\R(a, s, s', o)\\**

The reward function can be specified in the following ways:

- A data frame with columns named exactly like the arguments of `R_()`.
  You can use [`rbind()`](https://rdrr.io/r/base/cbind.html) with helper
  function `R_()` to create this data frame. Rewards can be specified
  multiple times and the definition that appears last in the data.frame
  will take effect.

- A list of lists. The list levels are `'action'` and `'start.state'`.
  The list elements are matrices with rows representing end states
  \\s'\\ and columns representing an observation \\o\\.

- A function with the same arguments as `R_()`, but no default values
  that returns the reward.

To avoid overflow problems with rewards, reward values should stay well
within the range of `[-1e10, +1e10]`. `-Inf` can be used as the reward
for unavailable actions and will be translated into a large negative
reward for solvers that only support finite reward values.

**Start Belief**

The initial belief state of the agent is a distribution over the states.
It is used to calculate the total expected cumulative reward printed
with the solved model. The function
[`reward()`](http://michael.hahsler.net/pomdp/reference/reward.md) can
be used to calculate rewards for any belief.

Some methods use this belief to decide which belief states to explore
(e.g., the finite grid method).

Options to specify the start belief state are:

- A probability distribution over the states. That is, a vector of
  \\\|S\|\\ probabilities, that add up to \\1\\.

- The string `"uniform"` for a uniform distribution over all states.

- An integer in the range \\1\\ to \\n\\ to specify the index of a
  single starting state.

- A string specifying the name of a single starting state.

The default initial belief is a uniform distribution over all states.

**Convergence**

A infinite-horizon POMDP needs to converge to provide a valid value
function and policy.

A finite-horizon POMDP may also converging to a infinite horizon
solution if the horizon is long enough.

**Time-dependent POMDPs**

Time dependence of transition probabilities, observation probabilities
and reward structure can be modeled by considering a set of **episodes**
representing **epoch** with the same settings. The length of each
episode is specified as a vector for `horizon`, where the length is the
number of episodes and each value is the length of the episode in
epochs. Transition probabilities, observation probabilities and/or
reward structure can contain a list with the values for each episode.
The helper function `epoch_to_episode()` converts an epoch to the
episode it belongs to.

## References

pomdp-solve website: <http://www.pomdp.org>

## See also

Other POMDP:
[`MDP2POMDP`](http://michael.hahsler.net/pomdp/reference/MDP2POMDP.md),
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

Other POMDP_examples:
[`POMDP_example_files`](http://michael.hahsler.net/pomdp/reference/POMDP_example_files.md),
[`RussianTiger`](http://michael.hahsler.net/pomdp/reference/RussianTiger.md),
[`Tiger`](http://michael.hahsler.net/pomdp/reference/Tiger.md)

## Author

Hossein Kamalzadeh, Michael Hahsler

## Examples

``` r
## Defining the Tiger Problem (it is also available via data(Tiger), see ? Tiger)

Tiger <- POMDP(
  name = "Tiger Problem",
  discount = 0.75,
  states = c("tiger-left" , "tiger-right"),
  actions = c("listen", "open-left", "open-right"),
  observations = c("tiger-left", "tiger-right"),
  start = "uniform",

  transition_prob = list(
    "listen" =     "identity",
    "open-left" =  "uniform",
    "open-right" = "uniform"
  ),

  observation_prob = list(
    "listen" = rbind(c(0.85, 0.15),
                     c(0.15, 0.85)),
    "open-left" =  "uniform",
    "open-right" = "uniform"
  ),

  # the reward helper expects: action, start.state, end.state, observation, value
  # missing arguments default to NA which matches any value (often denoted as * in POMDPs).
  reward = rbind(
    R_("listen",                    value =   -1),
    R_("open-left",  "tiger-left",  value = -100),
    R_("open-left",  "tiger-right", value =   10),
    R_("open-right", "tiger-left",  value =   10),
    R_("open-right", "tiger-right", value = -100)
  )
)

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

### Defining the Tiger problem using functions

trans_f <- function(action, start.state, end.state) {
  if(action == 'listen')
    if(end.state == start.state) return(1)
    else return(0)

  return(1/2) ### all other actions have a uniform distribution
}

obs_f <- function(action, end.state, observation) {
  if(action == 'listen')
    if(end.state == observation) return(0.85)
  else return(0.15)

  return(1/2)
}

rew_f <- function(action, start.state, end.state, observation) {
  if(action == 'listen') return(-1)
  if(action == 'open-left' && start.state == 'tiger-left') return(-100)
  if(action == 'open-left' && start.state == 'tiger-right') return(10)
  if(action == 'open-right' && start.state == 'tiger-left') return(10)
  if(action == 'open-right' && start.state == 'tiger-right') return(-100)
  stop('Not possible')
}

Tiger_func <- POMDP(
  name = "Tiger Problem",
  discount = 0.75,
  states = c("tiger-left" , "tiger-right"),
  actions = c("listen", "open-left", "open-right"),
  observations = c("tiger-left", "tiger-right"),
  start = "uniform",
  transition_prob = trans_f,
  observation_prob = obs_f,
  reward = rew_f
)

Tiger_func
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

# Defining a Time-dependent version of the Tiger Problem called Scared Tiger

# The tiger reacts normally for 3 epochs (goes randomly two one
# of the two doors when a door was opened). After 3 epochs he gets
# scared and when a door is opened then he always goes to the other door.

# specify the horizon for each of the two different episodes
Tiger_time_dependent <- Tiger
Tiger_time_dependent$name <- "Scared Tiger Problem"
Tiger_time_dependent$horizon <- c(normal_tiger = 3, scared_tiger = 3)
Tiger_time_dependent$transition_prob <- list(
  normal_tiger = list(
    "listen" = "identity",
    "open-left" = "uniform",
    "open-right" = "uniform"),
  scared_tiger = list(
    "listen" = "identity",
    "open-left" = rbind(c(0, 1), c(0, 1)),
    "open-right" = rbind(c(1, 0), c(1, 0))
  )
)
```
