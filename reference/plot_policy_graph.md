# POMDP Plot Policy Graphs

The function plots the POMDP policy graph for converged POMDP solution
and the policy tree for a finite-horizon solution.

## Usage

``` r
plot_policy_graph(
  x,
  belief = NULL,
  engine = c("igraph", "visNetwork"),
  show_belief = TRUE,
  state_col = NULL,
  legend = TRUE,
  simplify_observations = TRUE,
  remove_unreachable_nodes = TRUE,
  ...
)

curve_multiple_directed(graph, start = 0.3)
```

## Arguments

- x:

  object of class
  [POMDP](http://michael.hahsler.net/pomdp/reference/POMDP.md)
  containing a solved and converged POMDP problem.

- belief:

  the initial belief is used to mark the initial belief state in the
  graph of a converged solution and to identify the root node in a
  policy graph for a finite-horizon solution. If `NULL` then the belief
  is taken from the model definition.

- engine:

  The plotting engine to be used.

- show_belief:

  logical; show estimated belief proportions as a pie chart or color in
  each node?

- state_col:

  colors used to represent the belief over states in each node. Only
  used if `show_belief` is `TRUE`.

- legend:

  logical; display a legend for colors used belief proportions?

- simplify_observations:

  combine parallel observation arcs into a single arc.

- remove_unreachable_nodes:

  logical; remove nodes that are not reachable from the start state?
  Currently only implemented for policy trees for unconverged
  finite-time horizon POMDPs.

- ...:

  parameters are passed on to
  [`policy_graph()`](http://michael.hahsler.net/pomdp/reference/policy_graph.md),
  [`estimate_belief_for_nodes()`](http://michael.hahsler.net/pomdp/reference/estimate_belief_for_nodes.md)
  and the functions they use. Also, plotting options are passed on to
  the plotting engine
  [`igraph::plot.igraph()`](https://r.igraph.org/reference/plot.igraph.html)
  or
  [`visNetwork::visIgraph()`](https://rdrr.io/pkg/visNetwork/man/visNetwork-igraph.html).

- graph:

  The input graph.

- start:

  The curvature at the two extreme edges.

## Value

Invisibly returns what the plotting engine returns.

## Details

The policy graph returned by
[`policy_graph()`](http://michael.hahsler.net/pomdp/reference/policy_graph.md)
can be directly plotted. `plot_policy_graph()` uses
[`policy_graph()`](http://michael.hahsler.net/pomdp/reference/policy_graph.md)
to get the policy graph and produces an improved visualization (a
legend, tree layout for finite-horizon solutions, better edge curving,
etc.). It also offers an interactive visualization using
[`visNetwork::visIgraph()`](https://rdrr.io/pkg/visNetwork/man/visNetwork-igraph.html).

Each policy graph node is represented by an alpha vector specifying a
hyperplane segment. The convex hull of the set of hyperplanes represents
the value function. The policy specifies for each node an optimal action
which is printed together with the node ID inside the node. The arcs are
labeled with observations. Infinite-horizon converged solutions form a
single policy graph. For a finite-horizon solution, a policy tree is
produced. The levels of the tree and the first number in the node label
represent the epochs.

For better visualization, we provide a few features:

- Show Belief, belief color and legend: A pie chart (or the color) in
  each node can be used represent an example of the belief that the
  agent has if it is in this node. This can help with interpreting the
  policy graph. The belief is obtained by calling
  [`estimate_belief_for_nodes()`](http://michael.hahsler.net/pomdp/reference/estimate_belief_for_nodes.md).

- Simplify observations: In some cases, two observations can lead to the
  same node resulting in two parallel edges. These edges can be
  collapsed into one labels with the observations.

- Remove unreachable nodes: Many algorithms produce unused policy graph
  nodes which can be filtered to produce a smaller tree structure of
  actually used nodes. Non-converged policies depend on the initial
  belief and if an initial belief is specified, then different nodes
  will be filtered and the tree will look different.

These improvements can be disabled using parameters.

### Auxiliary function

`curve_multiple_directed()` is a helper function for plotting igraph
graphs similar to
[`igraph::curve_multiple()`](https://r.igraph.org/reference/curve_multiple.html)
but it also adds curvature to parallel edges that point in opposite
directions.

## See also

Other policy:
[`estimate_belief_for_nodes()`](http://michael.hahsler.net/pomdp/reference/estimate_belief_for_nodes.md),
[`optimal_action()`](http://michael.hahsler.net/pomdp/reference/optimal_action.md),
[`plot_belief_space()`](http://michael.hahsler.net/pomdp/reference/plot_belief_space.md),
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

### Policy graphs for converged solutions
sol <- solve_POMDP(model = Tiger)
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

policy_graph(sol)
#> IGRAPH c8bf20f D--- 5 10 -- 
#> + attr: label (v/c), id (v/n), action (v/c), size (v/n), label (e/c),
#> | observation (e/c), arrow.size (e/n)
#> + edges from c8bf20f:
#>  [1] 1->3 2->3 3->4 4->5 5->3 1->3 2->1 3->2 4->3 5->3

## visualization
plot_policy_graph(sol)


## use a different graph layout (circle and manual; needs igraph)
library("igraph")
#> 
#> Attaching package: ‘igraph’
#> The following objects are masked from ‘package:stats’:
#> 
#>     decompose, spectrum
#> The following object is masked from ‘package:base’:
#> 
#>     union
plot_policy_graph(sol, layout = layout.circle)

plot_policy_graph(sol, layout = rbind(c(1,1), c(1,-1), c(0,0), c(-1,-1), c(-1,1)), margin = .2)

plot_policy_graph(sol,
  layout = rbind(c(1,0), c(.5,0), c(0,0), c(-.5,0), c(-1,0)), rescale = FALSE,
  vertex.size = 15, edge.curved = 2,
  main = "Tiger Problem")


## hide labels, beliefs and legend
plot_policy_graph(sol, show_belief = FALSE, edge.label = NA, vertex.label = NA, legend = FALSE)


## custom larger vertex labels (A, B, ...)
plot_policy_graph(sol,
  vertex.label = LETTERS[1:nrow(policy(sol))],
  vertex.size = 60,
  vertex.label.cex = 2,
  edge.label.cex = .7,
  vertex.label.color = "white")


## plotting the igraph object directly
pg <- policy_graph(sol, show_belief = TRUE, 
  simplify_observations = TRUE, remove_unreachable_nodes = TRUE)

## (e.g., using a tree layout)
plot(pg, layout = layout_as_tree(pg, root = 3, mode = "out"))


## change labels (abbreviate observations and use only actions to label the vertices)
plot(pg,
  edge.label = abbreviate(E(pg)$label),
  vertex.label = V(pg)$action,
  vertex.size = 20)


## use action to color vertices (requires a graph without a belief pie chart) 
##    and color edges to represent observations.
pg <- policy_graph(sol, show_belief = FALSE, 
  simplify_observations = TRUE, remove_unreachable_nodes = TRUE)

plot(pg,
  vertex.label = NA,
  vertex.color = factor(V(pg)$action),
  vertex.size = 20,
  edge.color = factor(E(pg)$observation),
  edge.curved = .1
  )

acts <- levels(factor(V(pg)$action))
legend("topright", legend = acts, title = "action",
  col = igraph::categorical_pal(length(acts)), pch = 15)
obs <- levels(factor(E(pg)$observation))
legend("bottomright", legend = obs, title = "observation",
  col = igraph::categorical_pal(length(obs)), lty = 1) 


## plot interactive graphs using the visNetwork library.
## Note: the pie chart representation is not available, but colors are used instead.
plot_policy_graph(sol, engine = "visNetwork")

{"x":{"nodes":{"id":[1,2,3,4,5],"label":["1   \nopen-left","2   \nlisten","3 - initial belief\nlisten","4   \nlisten","5   \nopen-right"],"action":["open-left","listen","listen","listen","open-right"],"shape":["pie","pie","pie","pie","pie"],"color":["#377DB7","#506FA0","#8D4C6A","#CA2933","#E31A1C"],"pie":[[0.00334941754142986,0.9966505824585702],[0.15,0.85],[0.5,0.5],[0.85,0.15],[0.9966505824585702,0.00334941754142986]],"pie.color":[["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"]],"size":[44.72135954999579,44.72135954999579,44.72135954999579,44.72135954999579,44.72135954999579],"title":["<b>node id:<\/b> 1 <br>  <b>action:<\/b> open-left <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.003 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.997 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 2 <br>  <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.15 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.85 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 3 <br>  <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.5 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.5 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 4 <br>  <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.85 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.15 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 5 <br>  <b>action:<\/b> open-right <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.997 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.003 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>"],"x":[-1,-0.7534388180451913,-0.002030010723336972,0.7560008160579024,1],"y":[0.9932668765995634,-1,-0.1350296300951241,-0.997176340972698,1]},"edges":{"from":[1,2,2,3,3,4,4,5],"to":[3,1,3,2,4,3,5,3],"label":["tiger-left/\ntiger-right","tiger-right","tiger-left","tiger-right","tiger-left","tiger-right","tiger-left","tiger-left/\ntiger-right"],"observation":["tiger-left/\ntiger-right","tiger-right","tiger-left","tiger-right","tiger-left","tiger-right","tiger-left","tiger-left/\ntiger-right"],"arrow.size":[0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot","physics":false},"manipulation":{"enabled":false},"edges":{"smooth":{"type":"continuous"},"arrows":"to"},"physics":{"stabilization":false}},"groups":null,"width":null,"height":null,"idselection":{"enabled":true,"style":"width: 150px; height: 26px","useLabels":true,"main":"Select by id"},"byselection":{"enabled":false,"style":"width: 150px; height: 26px","multiple":false,"hideColor":"rgba(200,200,200,0.5)","highlight":false},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)","igraphlayout":{"type":"square"},"highlight":{"enabled":true,"hoverNearest":false,"degree":0,"algorithm":"all","hideColor":"rgba(200,200,200,0.5)","labelOnly":true},"collapse":{"enabled":false,"fit":false,"resetHighlight":true,"clusterOptions":null,"keepCoord":true,"labelSuffix":"(cluster)"}},"evals":[],"jsHooks":[]}
## add smooth edges and a layout (note, engine can be abbreviated)
plot_policy_graph(sol, engine = "visNetwork", layout = "layout_in_circle", smooth = TRUE)

{"x":{"nodes":{"id":[1,2,3,4,5],"label":["1   \nopen-left","2   \nlisten","3 - initial belief\nlisten","4   \nlisten","5   \nopen-right"],"action":["open-left","listen","listen","listen","open-right"],"shape":["pie","pie","pie","pie","pie"],"color":["#377DB7","#506FA0","#8D4C6A","#CA2933","#E31A1C"],"pie":[[0.00334941754142986,0.9966505824585702],[0.15,0.85],[0.5,0.5],[0.85,0.15],[0.9966505824585702,0.00334941754142986]],"pie.color":[["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"]],"size":[44.72135954999579,44.72135954999579,44.72135954999579,44.72135954999579,44.72135954999579],"title":["<b>node id:<\/b> 1 <br>  <b>action:<\/b> open-left <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.003 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.997 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 2 <br>  <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.15 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.85 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 3 <br>  <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.5 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.5 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 4 <br>  <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.85 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.15 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 5 <br>  <b>action:<\/b> open-right <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.997 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.003 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>"],"x":[1,0.2360679774997898,-0.9999999999999998,-1,0.2360679774997898],"y":[0,1,0.6180339887498951,-0.6180339887498947,-1]},"edges":{"from":[1,2,2,3,3,4,4,5],"to":[3,1,3,2,4,3,5,3],"label":["tiger-left/\ntiger-right","tiger-right","tiger-left","tiger-right","tiger-left","tiger-right","tiger-left","tiger-left/\ntiger-right"],"observation":["tiger-left/\ntiger-right","tiger-right","tiger-left","tiger-right","tiger-left","tiger-right","tiger-left","tiger-left/\ntiger-right"],"arrow.size":[0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot","physics":false},"manipulation":{"enabled":false},"edges":{"smooth":true,"arrows":"to"},"physics":{"stabilization":false}},"groups":null,"width":null,"height":null,"idselection":{"enabled":true,"style":"width: 150px; height: 26px","useLabels":true,"main":"Select by id"},"byselection":{"enabled":false,"style":"width: 150px; height: 26px","multiple":false,"hideColor":"rgba(200,200,200,0.5)","highlight":false},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)","igraphlayout":{"type":"square"},"highlight":{"enabled":true,"hoverNearest":false,"degree":0,"algorithm":"all","hideColor":"rgba(200,200,200,0.5)","labelOnly":true},"collapse":{"enabled":false,"fit":false,"resetHighlight":true,"clusterOptions":null,"keepCoord":true,"labelSuffix":"(cluster)"}},"evals":[],"jsHooks":[]}

### Policy trees for finite-horizon solutions
sol <- solve_POMDP(model = Tiger, horizon = 4, method = "incprune")

policy_graph(sol)
#> IGRAPH a3bb937 DN-- 26 46 -- 
#> + attr: layout (g/n), name (v/c), id (v/c), epoch (v/n), action (v/c),
#> | size (v/n), label (e/c), observation (e/c), arrow.size (e/n)
#> + edges from a3bb937 (vertex names):
#>  [1] 1-1\nopen-left ->2-5\nlisten     1-2\nlisten    ->2-5\nlisten    
#>  [3] 1-3\nlisten    ->2-5\nlisten     1-4\nlisten    ->2-6\nlisten    
#>  [5] 1-5\nlisten    ->2-7\nlisten     1-6\nlisten    ->2-7\nlisten    
#>  [7] 1-7\nlisten    ->2-8\nlisten     1-8\nlisten    ->2-9\nopen-right
#>  [9] 1-9\nopen-right->2-5\nlisten     2-1\nopen-left ->3-3\nlisten    
#> [11] 2-2\nlisten    ->3-2\nlisten     2-3\nlisten    ->3-3\nlisten    
#> [13] 2-4\nlisten    ->3-3\nlisten     2-5\nlisten    ->3-4\nlisten    
#> + ... omitted several edges

plot_policy_graph(sol)

# Note: the first number in the node id is the epoch.

# plot the policy tree for an initial belief of 90% that the tiger is to the left
plot_policy_graph(sol, belief = c(0.9, 0.1))


# Plotting a larger graph (see ? igraph.plotting for plotting options)
sol <- solve_POMDP(model = Tiger, horizon = 10, method = "incprune")

plot_policy_graph(sol, edge.arrow.size = .1,
  vertex.label.cex = .5, edge.label.cex = .5)


plot_policy_graph(sol, engine = "visNetwork")

{"x":{"nodes":{"id":["1-15\nlisten","2-23\nlisten","2-7\nlisten","3-12\nlisten","3-23\nopen-right","3-1\nopen-left","4-11\nlisten","4-17\nlisten","4-5\nlisten","5-9\nlisten","5-14\nlisten","5-17\nopen-right","5-1\nopen-left","6-8\nlisten","5-4\nlisten","6-12\nlisten","6-15\nopen-right","6-1\nopen-left","7-5\nlisten","6-4\nlisten","7-8\nlisten","7-9\nopen-right","7-1\nopen-left","8-5\nlisten","7-2\nlisten","8-7\nlisten","8-9\nopen-right","8-1\nopen-left","9-3\nlisten","8-3\nlisten","9-4\nlisten","9-5\nopen-right","9-1\nopen-left","10-2\nlisten","9-2\nlisten","10-3\nopen-right","10-1\nopen-left"],"epoch":[1,2,2,3,3,3,4,4,4,5,5,5,5,6,5,6,6,6,7,6,7,7,7,8,7,8,8,8,9,8,9,9,9,10,9,10,10],"action":["listen","listen","listen","listen","open-right","open-left","listen","listen","listen","listen","listen","open-right","open-left","listen","listen","listen","open-right","open-left","listen","listen","listen","open-right","open-left","listen","listen","listen","open-right","open-left","listen","listen","listen","open-right","open-left","listen","listen","open-right","open-left"],"shape":["pie","pie","pie","pie","pie","pie","pie","pie","pie","pie","pie","pie","pie","pie","pie","pie","pie","pie","pie","pie","pie","pie","pie","pie","pie","pie","pie","pie","pie","pie","pie","pie","pie","pie","pie","pie","pie"],"color":["#8D4C6A","#CA2933","#506FA0","#8D4C6A","#DE1D20","#3C7AB3","#8D4C6A","#CA2933","#506FA0","#8D4C6A","#CA2933","#DE1D20","#377DB7","#8D4C6A","#506FA0","#CA2933","#DE1D20","#377DB7","#8D4C6A","#506FA0","#CA2933","#DE1D20","#377DB7","#8D4C6A","#506FA0","#CA2933","#DE1D20","#377DB7","#8D4C6A","#506FA0","#CA2933","#DE1D20","#377DB7","#8D4C6A","#506FA0","#DE1D20","#377DB7"],"pie":[[0.5,0.5],[0.85,0.15],[0.15,0.85],[0.5,0.5],[0.9697987,0.0302013],[0.0302013,0.9697987],[0.5,0.5],[0.85,0.15],[0.1499998,0.8500002],[0.5,0.5],[0.85,0.15],[0.9697987,0.0302013],[0.0009689,0.9990310999999999],[0.5,0.5],[0.15,0.85],[0.85,0.15],[0.9697987,0.0302013],[0.0001711,0.9998289],[0.5,0.5],[0.15,0.85],[0.85,0.15],[0.9697987,0.0302013],[3.02e-05,0.9999698],[0.5,0.5],[0.15,0.85],[0.85,0.15],[0.9697987,0.0302013],[5.3e-06,0.9999947],[0.5,0.5],[0.15,0.85],[0.85,0.15],[0.9697987,0.0302013],[9e-07,0.9999991],[0.5,0.5],[0.15,0.85],[0.9697987,0.0302013],[2e-07,0.9999998]],"pie.color":[["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"],["#E41A1C","#377EB8"]],"size":[16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573,16.43989873053573],"title":["<b>node id:<\/b> 1-15 <br> <b>epoch:<\/b> 1 <br> <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.5 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.5 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 2-23 <br> <b>epoch:<\/b> 2 <br> <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.85 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.15 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 2-7 <br> <b>epoch:<\/b> 2 <br> <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.15 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.85 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 3-12 <br> <b>epoch:<\/b> 3 <br> <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.5 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.5 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 3-23 <br> <b>epoch:<\/b> 3 <br> <b>action:<\/b> open-right <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.97 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.03 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 3-1 <br> <b>epoch:<\/b> 3 <br> <b>action:<\/b> open-left <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.03 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.97 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 4-11 <br> <b>epoch:<\/b> 4 <br> <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.5 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.5 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 4-17 <br> <b>epoch:<\/b> 4 <br> <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.85 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.15 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 4-5 <br> <b>epoch:<\/b> 4 <br> <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.15 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.85 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 5-9 <br> <b>epoch:<\/b> 5 <br> <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.5 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.5 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 5-14 <br> <b>epoch:<\/b> 5 <br> <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.85 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.15 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 5-17 <br> <b>epoch:<\/b> 5 <br> <b>action:<\/b> open-right <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.97 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.03 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 5-1 <br> <b>epoch:<\/b> 5 <br> <b>action:<\/b> open-left <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.001 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.999 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 6-8 <br> <b>epoch:<\/b> 6 <br> <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.5 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.5 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 5-4 <br> <b>epoch:<\/b> 5 <br> <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.15 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.85 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 6-12 <br> <b>epoch:<\/b> 6 <br> <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.85 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.15 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 6-15 <br> <b>epoch:<\/b> 6 <br> <b>action:<\/b> open-right <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.97 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.03 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 6-1 <br> <b>epoch:<\/b> 6 <br> <b>action:<\/b> open-left <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 1 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 7-5 <br> <b>epoch:<\/b> 7 <br> <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.5 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.5 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 6-4 <br> <b>epoch:<\/b> 6 <br> <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.15 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.85 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 7-8 <br> <b>epoch:<\/b> 7 <br> <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.85 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.15 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 7-9 <br> <b>epoch:<\/b> 7 <br> <b>action:<\/b> open-right <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.97 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.03 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 7-1 <br> <b>epoch:<\/b> 7 <br> <b>action:<\/b> open-left <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 1 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 8-5 <br> <b>epoch:<\/b> 8 <br> <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.5 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.5 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 7-2 <br> <b>epoch:<\/b> 7 <br> <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.15 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.85 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 8-7 <br> <b>epoch:<\/b> 8 <br> <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.85 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.15 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 8-9 <br> <b>epoch:<\/b> 8 <br> <b>action:<\/b> open-right <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.97 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.03 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 8-1 <br> <b>epoch:<\/b> 8 <br> <b>action:<\/b> open-left <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 1 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 9-3 <br> <b>epoch:<\/b> 9 <br> <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.5 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.5 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 8-3 <br> <b>epoch:<\/b> 8 <br> <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.15 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.85 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 9-4 <br> <b>epoch:<\/b> 9 <br> <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.85 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.15 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 9-5 <br> <b>epoch:<\/b> 9 <br> <b>action:<\/b> open-right <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.97 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.03 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 9-1 <br> <b>epoch:<\/b> 9 <br> <b>action:<\/b> open-left <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 1 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 10-2 <br> <b>epoch:<\/b> 10 <br> <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.5 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.5 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 9-2 <br> <b>epoch:<\/b> 9 <br> <b>action:<\/b> listen <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.15 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.85 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 10-3 <br> <b>epoch:<\/b> 10 <br> <b>action:<\/b> open-right <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0.97 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 0.03 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>","<b>node id:<\/b> 10-1 <br> <b>epoch:<\/b> 10 <br> <b>action:<\/b> open-left <p> <table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   <\/th>\n   <th style=\"text-align:right;\"> belief <\/th>\n  <\/tr>\n <\/thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-left <\/td>\n   <td style=\"text-align:right;\"> 0 <\/td>\n  <\/tr>\n  <tr>\n   <td style=\"text-align:left;\"> tiger-right <\/td>\n   <td style=\"text-align:right;\"> 1 <\/td>\n  <\/tr>\n<\/tbody>\n<\/table>"],"x":[0.7557565789473684,0.5115131578947369,1,0.2335526315789473,0.7894736842105263,1,0.7894736842105263,-0.006578947368421018,0.4736842105263157,-0.2763157894736842,0.6842105263157894,0.263157894736842,0.4736842105263157,0.263157894736842,0.8947368421052631,-0.5,0.6842105263157894,0.8947368421052631,-0.736842105263158,-0.05263157894736847,0.1578947368421053,-0.2631578947368421,-0.05263157894736847,-0.2631578947368421,0.368421052631579,-0.8947368421052632,0.1578947368421053,0.368421052631579,-1,-0.5789473684210527,-0.368421052631579,-0.7894736842105263,-0.5789473684210527,-1,-0.1578947368421053,-0.368421052631579,-0.1578947368421053],"y":[-1,-0.7777777777777778,-0.7777777777777778,-0.5555555555555556,-0.5555555555555556,-0.5555555555555556,-0.3333333333333334,-0.3333333333333334,-0.3333333333333334,-0.1111111111111112,-0.1111111111111112,-0.1111111111111112,-0.1111111111111112,0.1111111111111112,-0.1111111111111112,0.1111111111111112,0.1111111111111112,0.1111111111111112,0.3333333333333333,0.1111111111111112,0.3333333333333333,0.3333333333333333,0.3333333333333333,0.5555555555555556,0.3333333333333333,0.5555555555555556,0.5555555555555556,0.5555555555555556,0.7777777777777777,0.5555555555555556,0.7777777777777777,0.7777777777777777,0.7777777777777777,1,0.7777777777777777,1,1],"label":["1-15\nlisten","2-23\nlisten","2-7\nlisten","3-12\nlisten","3-23\nopen-right","3-1\nopen-left","4-11\nlisten","4-17\nlisten","4-5\nlisten","5-9\nlisten","5-14\nlisten","5-17\nopen-right","5-1\nopen-left","6-8\nlisten","5-4\nlisten","6-12\nlisten","6-15\nopen-right","6-1\nopen-left","7-5\nlisten","6-4\nlisten","7-8\nlisten","7-9\nopen-right","7-1\nopen-left","8-5\nlisten","7-2\nlisten","8-7\nlisten","8-9\nopen-right","8-1\nopen-left","9-3\nlisten","8-3\nlisten","9-4\nlisten","9-5\nopen-right","9-1\nopen-left","10-2\nlisten","9-2\nlisten","10-3\nopen-right","10-1\nopen-left"]},"edges":{"from":["1-15\nlisten","1-15\nlisten","2-23\nlisten","2-23\nlisten","2-7\nlisten","2-7\nlisten","3-12\nlisten","3-12\nlisten","3-23\nopen-right","3-1\nopen-left","4-11\nlisten","4-11\nlisten","4-17\nlisten","4-17\nlisten","4-5\nlisten","4-5\nlisten","5-9\nlisten","5-9\nlisten","5-14\nlisten","5-14\nlisten","5-17\nopen-right","5-1\nopen-left","6-8\nlisten","6-8\nlisten","5-4\nlisten","5-4\nlisten","6-12\nlisten","6-12\nlisten","6-15\nopen-right","6-1\nopen-left","7-5\nlisten","7-5\nlisten","6-4\nlisten","6-4\nlisten","7-8\nlisten","7-8\nlisten","7-9\nopen-right","7-1\nopen-left","8-5\nlisten","8-5\nlisten","7-2\nlisten","7-2\nlisten","8-7\nlisten","8-7\nlisten","8-9\nopen-right","8-1\nopen-left","9-3\nlisten","8-3\nlisten","8-3\nlisten","9-4\nlisten","9-4\nlisten","9-5\nopen-right","9-1\nopen-left","9-2\nlisten","9-2\nlisten"],"to":["2-23\nlisten","2-7\nlisten","3-12\nlisten","3-23\nopen-right","3-12\nlisten","3-1\nopen-left","4-17\nlisten","4-5\nlisten","4-11\nlisten","4-11\nlisten","5-14\nlisten","5-4\nlisten","5-9\nlisten","5-17\nopen-right","5-9\nlisten","5-1\nopen-left","6-12\nlisten","6-4\nlisten","6-8\nlisten","6-15\nopen-right","6-8\nlisten","6-8\nlisten","7-8\nlisten","7-2\nlisten","6-8\nlisten","6-1\nopen-left","7-5\nlisten","7-9\nopen-right","7-5\nlisten","7-5\nlisten","8-7\nlisten","8-3\nlisten","7-5\nlisten","7-1\nopen-left","8-5\nlisten","8-9\nopen-right","8-5\nlisten","8-5\nlisten","9-4\nlisten","9-2\nlisten","8-5\nlisten","8-1\nopen-left","9-3\nlisten","9-5\nopen-right","9-3\nlisten","9-3\nlisten","10-2\nlisten","9-3\nlisten","9-1\nopen-left","10-2\nlisten","10-3\nopen-right","10-2\nlisten","10-2\nlisten","10-2\nlisten","10-1\nopen-left"],"label":["tiger-left","tiger-right","tiger-right","tiger-left","tiger-left","tiger-right","tiger-left","tiger-right","tiger-left/\ntiger-right","tiger-left/\ntiger-right","tiger-left","tiger-right","tiger-right","tiger-left","tiger-left","tiger-right","tiger-left","tiger-right","tiger-right","tiger-left","tiger-left/\ntiger-right","tiger-left/\ntiger-right","tiger-left","tiger-right","tiger-left","tiger-right","tiger-right","tiger-left","tiger-left/\ntiger-right","tiger-left/\ntiger-right","tiger-left","tiger-right","tiger-left","tiger-right","tiger-right","tiger-left","tiger-left/\ntiger-right","tiger-left/\ntiger-right","tiger-left","tiger-right","tiger-left","tiger-right","tiger-right","tiger-left","tiger-left/\ntiger-right","tiger-left/\ntiger-right","tiger-left/\ntiger-right","tiger-left","tiger-right","tiger-right","tiger-left","tiger-left/\ntiger-right","tiger-left/\ntiger-right","tiger-left","tiger-right"],"observation":["tiger-left","tiger-right","tiger-right","tiger-left","tiger-left","tiger-right","tiger-left","tiger-right","tiger-left/\ntiger-right","tiger-left/\ntiger-right","tiger-left","tiger-right","tiger-right","tiger-left","tiger-left","tiger-right","tiger-left","tiger-right","tiger-right","tiger-left","tiger-left/\ntiger-right","tiger-left/\ntiger-right","tiger-left","tiger-right","tiger-left","tiger-right","tiger-right","tiger-left","tiger-left/\ntiger-right","tiger-left/\ntiger-right","tiger-left","tiger-right","tiger-left","tiger-right","tiger-right","tiger-left","tiger-left/\ntiger-right","tiger-left/\ntiger-right","tiger-left","tiger-right","tiger-left","tiger-right","tiger-right","tiger-left","tiger-left/\ntiger-right","tiger-left/\ntiger-right","tiger-left/\ntiger-right","tiger-left","tiger-right","tiger-right","tiger-left","tiger-left/\ntiger-right","tiger-left/\ntiger-right","tiger-left","tiger-right"],"arrow.size":[0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot","physics":false},"manipulation":{"enabled":false},"edges":{"smooth":{"type":"continuous"},"arrows":"to"},"physics":{"stabilization":false}},"groups":null,"width":null,"height":null,"idselection":{"enabled":true,"style":"width: 150px; height: 26px","useLabels":true,"main":"Select by id"},"byselection":{"enabled":false,"style":"width: 150px; height: 26px","multiple":false,"hideColor":"rgba(200,200,200,0.5)","highlight":false},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)","igraphlayout":{"type":"square"},"highlight":{"enabled":true,"hoverNearest":false,"degree":0,"algorithm":"all","hideColor":"rgba(200,200,200,0.5)","labelOnly":true},"collapse":{"enabled":false,"fit":false,"resetHighlight":true,"clusterOptions":null,"keepCoord":true,"labelSuffix":"(cluster)"}},"evals":[],"jsHooks":[]}
```
