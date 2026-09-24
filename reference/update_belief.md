# Belief Update

Update the belief given a taken action and observation.

## Usage

``` r
update_belief(
  model,
  belief = NULL,
  action = NULL,
  observation = NULL,
  episode = 1,
  digits = 7,
  drop = TRUE
)
```

## Arguments

- model:

  a [POMDP](http://michael.hahsler.net/pomdp/reference/POMDP.md) object.

- belief:

  the current belief state. Defaults to the start belief state specified
  in the model or "uniform".

- action:

  the taken action. Can also be a vector of multiple actions or, if
  missing, then all actions are evaluated.

- observation:

  the received observation. Can also be a vector of multiple
  observations or, if missing, then all observations are evaluated.

- episode:

  Use transition and observation matrices for the given episode for
  time-dependent POMDPs (see
  [POMDP](http://michael.hahsler.net/pomdp/reference/POMDP.md)).

- digits:

  round decimals.

- drop:

  logical; drop the result to a vector if only a single belief state is
  returned.

## Value

The updated belief state as a named vector. If `action` or
`observations` is a vector with multiple elements or is missing, then a
matrix with all resulting belief states is returned.

## Details

Update the belief state \\b\\ (`belief`) with an action \\a\\ and
observation \\o\\ using the update \\b' \leftarrow \tau(b, a, o)\\
defined so that

\$\$b'(s') = \eta O(o \| s',a) \sum\_{s \in S} T(s' \| s,a) b(s)\$\$

where \\\eta = 1/ \sum\_{s' \in S}\[ O(o \| s',a) \sum\_{s \in S} T(s'
\| s,a) b(s)\]\\ normalizes the new belief state so the probabilities
add up to one.

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
[`sample_belief_space()`](http://michael.hahsler.net/pomdp/reference/sample_belief_space.md),
[`simulate_POMDP()`](http://michael.hahsler.net/pomdp/reference/simulate_POMDP.md),
[`solve_POMDP()`](http://michael.hahsler.net/pomdp/reference/solve_POMDP.md),
[`solve_SARSOP()`](http://michael.hahsler.net/pomdp/reference/solve_SARSOP.md),
[`transition_graph()`](http://michael.hahsler.net/pomdp/reference/transition_graph.md),
[`value_function()`](http://michael.hahsler.net/pomdp/reference/value_function.md),
[`write_POMDP()`](http://michael.hahsler.net/pomdp/reference/write_POMDP.md)

## Author

Michael Hahsler

## Examples

``` r
data(Tiger)

update_belief(c(.5,.5), model = Tiger)
#>                        tiger-left tiger-right
#> listen+tiger-left            0.85        0.15
#> open-left+tiger-left         0.50        0.50
#> open-right+tiger-left        0.50        0.50
#> listen+tiger-right           0.15        0.85
#> open-left+tiger-right        0.50        0.50
#> open-right+tiger-right       0.50        0.50
#> attr(,"order")
#>       action observation
#> 1     listen  tiger-left
#> 2  open-left  tiger-left
#> 3 open-right  tiger-left
#> 4     listen tiger-right
#> 5  open-left tiger-right
#> 6 open-right tiger-right
update_belief(c(.5,.5), action = "listen", observation = "tiger-left", model = Tiger)
#>  tiger-left tiger-right 
#>        0.85        0.15 
#> attr(,"order")
#>   action observation
#> 1 listen  tiger-left
update_belief(c(.15,.85), action = "listen", observation = "tiger-right", model = Tiger)
#>  tiger-left tiger-right 
#>   0.0302013   0.9697987 
#> attr(,"order")
#>   action observation
#> 1 listen tiger-right
```
