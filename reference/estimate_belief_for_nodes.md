# Estimate the Belief for Policy Graph Nodes

Estimate a belief for each alpha vector (segment of the value function)
which represents a node in the policy graph.

## Usage

``` r
estimate_belief_for_nodes(
  x,
  method = "auto",
  belief = NULL,
  verbose = FALSE,
  ...
)
```

## Arguments

- x:

  object of class
  [POMDP](http://michael.hahsler.net/pomdp/reference/POMDP.md)
  containing a solved and converged POMDP problem.

- method:

  character string specifying the estimation method. Methods include
  `"auto"`, reuse `"solver_points"`, follow `"trajectories"`, sample
  `"random_sample"` or `"regular_sample"`. Auto uses solver points if
  available and follows trajectories otherwise.

- belief:

  start belief used for method trajectories. `NULL` uses the start
  belief specified in the model.

- verbose:

  logical; show which method is used.

- ...:

  parameters are passed on to
  [`sample_belief_space()`](http://michael.hahsler.net/pomdp/reference/sample_belief_space.md)
  or the code that follows trajectories.

## Value

returns a list with matrices with a belief for each policy graph node.
The list elements are the epochs and converged solutions only have a
single element.

## Details

`estimate_belief_for_nodes()` can estimate the belief in several ways:

1.  **Use belief points explored by the solver.** Some solvers return
    explored belief points. We assign the belief points to the nodes and
    average each nodes belief.

2.  **Follow trajectories** (breadth first) till all policy graph nodes
    have been visited and return the encountered belief. This
    implementation returns the first (i.e., shallowest) belief point
    that is encountered is used and no averaging is performed. parameter
    `n` can be used to limit the number of nodes searched.

3.  **Sample a large set** of possible belief points, assigning them to
    the nodes and then averaging the belief over the points assigned to
    each node. This will return a central belief for the node.
    Additional parameters like `method` and the sample size `n` are
    passed on to
    [`sample_belief_space()`](http://michael.hahsler.net/pomdp/reference/sample_belief_space.md).
    If no belief point is generated for a segment, then a warning is
    produced. In this case, the number of sampled points can be
    increased.

**Notes:**

- Each method may return a different answer. The only thing that is
  guaranteed is that the returned belief falls in the range where the
  value function segment is maximal.

- If some nodes not belief points are sampled, or the node is not
  reachable from the initial belief, then a vector with all `NaN`s will
  be returned with a warning.

## See also

Other policy:
[`optimal_action()`](http://michael.hahsler.net/pomdp/reference/optimal_action.md),
[`plot_belief_space()`](http://michael.hahsler.net/pomdp/reference/plot_belief_space.md),
[`plot_policy_graph()`](http://michael.hahsler.net/pomdp/reference/plot_policy_graph.md),
[`policy()`](http://michael.hahsler.net/pomdp/reference/policy.md),
[`policy_graph()`](http://michael.hahsler.net/pomdp/reference/policy_graph.md),
[`projection()`](http://michael.hahsler.net/pomdp/reference/projection.md),
[`reward()`](http://michael.hahsler.net/pomdp/reference/reward.md),
[`solve_POMDP()`](http://michael.hahsler.net/pomdp/reference/solve_POMDP.md),
[`solve_SARSOP()`](http://michael.hahsler.net/pomdp/reference/solve_SARSOP.md),
[`value_function()`](http://michael.hahsler.net/pomdp/reference/value_function.md)

## Examples

``` r
data("Tiger")

# Infinite horizon case with converged solution
sol <- solve_POMDP(model = Tiger, method = "grid")
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

# default method auto uses the belief points used in the algorithm (if available).
estimate_belief_for_nodes(sol, verbose = TRUE)
#> Using method ‘solver_points’ 
#> [[1]]
#>    tiger-left tiger-right
#> 1 0.003349418 0.996650582
#> 2 0.150000000 0.850000000
#> 3 0.500000000 0.500000000
#> 4 0.850000000 0.150000000
#> 5 0.996650582 0.003349418
#> 

# use belief points obtained from trajectories  
estimate_belief_for_nodes(sol, method = "trajectories", verbose = TRUE)
#> Using method ‘trajectories’ 
#> Using policy graph trajectories to find beliefs for nodes.
#> 
#> Found 1 of 5 
#> Found 2 of 5 
#> Found 3 of 5 
#> Found 4 of 5 
#> Found 5 of 5 
#> [[1]]
#>      tiger-left tiger-right
#> [1,]  0.0302013   0.9697987
#> [2,]  0.1500000   0.8500000
#> [3,]  0.5000000   0.5000000
#> [4,]  0.8500000   0.1500000
#> [5,]  0.9697987   0.0302013
#> 

# use a random uniform sample 
estimate_belief_for_nodes(sol, method = "random", verbose = TRUE)
#> Using method ‘random_sample’ 
#> [[1]]
#>   tiger-left tiger-right
#> 1 0.02402645  0.97597355
#> 2 0.15932706  0.84067294
#> 3 0.49944976  0.50055024
#> 4 0.84287830  0.15712170
#> 5 0.97966899  0.02033101
#> 

# Finite horizon example with three epochs. 
sol <- solve_POMDP(model = Tiger, horizon = 3)
#> Warning: Value function (alpha vectors) may not be valid for a finite horizon unconverged solution of method 'grid' with neg. rewards!
sol
#> POMDP, list - Tiger Problem
#>   Discount factor: 0.75
#>   Horizon: 3 epochs
#>   Size: 2 states / 3 actions / 2 obs.
#>   Start: uniform
#>   Solved:
#>     Method: ‘grid’
#>     Solution converged: FALSE
#>     # of alpha vectors: 13
#>     Total expected reward: -167.845000
#> 
#>   List components: ‘name’, ‘discount’, ‘horizon’, ‘states’, ‘actions’,
#>     ‘observations’, ‘transition_prob’, ‘observation_prob’, ‘reward’,
#>     ‘start’, ‘info’, ‘solution’
estimate_belief_for_nodes(sol)
#> [[1]]
#>    tiger-left tiger-right
#> 1 0.003349418 0.996650582
#> 2 0.150000000 0.850000000
#> 3 0.500000000 0.500000000
#> 4 0.850000000 0.150000000
#> 5 0.996650582 0.003349418
#> 
#> [[2]]
#>    tiger-left tiger-right
#> 1 0.003349418 0.996650582
#> 2 0.150000000 0.850000000
#> 3 0.500000000 0.500000000
#> 4 0.850000000 0.150000000
#> 5 0.996650582 0.003349418
#> 
#> [[3]]
#>    tiger-left tiger-right
#> 1 0.003349418 0.996650582
#> 2 0.500000000 0.500000000
#> 3 0.996650582 0.003349418
#> 
```
