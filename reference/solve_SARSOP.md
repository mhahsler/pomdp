# Solve a POMDP Problem using SARSOP

This function uses the C++ implementation of the SARSOP algorithm by
Kurniawati, Hsu and Lee (2008) interfaced in package sarsop to solve
infinite horizon problems that are formulated as partially observable
Markov decision processes (POMDPs). The result is an optimal or
approximately optimal policy.

## Usage

``` r
solve_SARSOP(
  model,
  horizon = Inf,
  discount = NULL,
  terminal_values = NULL,
  method = "sarsop",
  digits = 7,
  parameter = NULL,
  verbose = FALSE
)
```

## Arguments

- model:

  a POMDP problem specification created with
  [`POMDP()`](http://michael.hahsler.net/pomdp/reference/POMDP.md).
  Alternatively, a POMDP file or the URL for a POMDP file can be
  specified.

- horizon:

  SARSOP only supports `Inf`.

- discount:

  discount factor in range \\\[0, 1\]\\. If `NULL`, then the discount
  factor specified in `model` will be used.

- terminal_values:

  `NULL`. SARSOP does not use terminal values.

- method:

  string; there is only one method available called `"sarsop"`.

- digits:

  precision used when writing POMDP files (see
  [`write_POMDP()`](http://michael.hahsler.net/pomdp/reference/write_POMDP.md)).

- parameter:

  a list with parameters passed on to the function
  [`sarsop::pomdpsol()`](https://rdrr.io/pkg/sarsop/man/appl.html) in
  package sarsop.

- verbose:

  logical, if set to `TRUE`, the function provides the output of the
  solver in the R console.

## Value

The solver returns an object of class POMDP which is a list with the
model specifications (`'model'`), the solution (`'solution'`), and the
solver output (`'solver_output'`).

## References

Carl Boettiger, Jeroen Ooms and Milad Memarzadeh (2020). sarsop:
Approximate POMDP Planning Software. R package version 0.6.6.
https://CRAN.R-project.org/package=sarsop

H. Kurniawati, D. Hsu, and W.S. Lee (2008). SARSOP: Efficient
point-based POMDP planning by approximating optimally reachable belief
spaces. In Proc. Robotics: Science and Systems.

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
[`value_function()`](http://michael.hahsler.net/pomdp/reference/value_function.md)

Other solver:
[`solve_MDP()`](http://michael.hahsler.net/pomdp/reference/solve_MDP.md),
[`solve_POMDP()`](http://michael.hahsler.net/pomdp/reference/solve_POMDP.md)

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
[`transition_graph()`](http://michael.hahsler.net/pomdp/reference/transition_graph.md),
[`update_belief()`](http://michael.hahsler.net/pomdp/reference/update_belief.md),
[`value_function()`](http://michael.hahsler.net/pomdp/reference/value_function.md),
[`write_POMDP()`](http://michael.hahsler.net/pomdp/reference/write_POMDP.md)

## Author

Michael Hahsler

## Examples

``` r
if (FALSE) { # \dontrun{
# Solving the simple infinite-horizon Tiger problem with SARSOP
# You need to install package "sarsop"
data("Tiger")
Tiger

sol <- solve_SARSOP(model = Tiger)
sol

# look at solver output
sol$solver_output

# policy (value function (alpha vectors), optimal action and observation dependent transitions)
policy(sol)

# value function
plot_value_function(sol, ylim = c(0,20))

# plot the policy graph
plot_policy_graph(sol)

# reward of the optimal policy
reward(sol)

# Solve a bundled POMDP file. The timeout is set to 10 seconds.
file <- system.file("examples/shuttle_95.POMDP", package = "pomdp")
sol <- solve_SARSOP(file, parameter = list(timeout = 10))
sol
} # }
```
