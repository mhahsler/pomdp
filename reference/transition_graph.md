# Transition Graph

Returns the transition model as an igraph object.

## Usage

``` r
transition_graph(
  x,
  action = NULL,
  episode = NULL,
  epoch = NULL,
  state_col = NULL,
  simplify_transitions = TRUE,
  remove_unavailable_actions = TRUE
)

plot_transition_graph(
  x,
  action = NULL,
  episode = NULL,
  epoch = NULL,
  state_col = NULL,
  simplify_transitions = TRUE,
  main = NULL,
  ...
)
```

## Arguments

- x:

  object of class
  [POMDP](http://michael.hahsler.net/pomdp/reference/POMDP.md) or
  [MDP](http://michael.hahsler.net/pomdp/reference/MDP.md).

- action:

  the name or id of an action or a set of actions. Bey default the
  transition model for all actions is returned.

- episode, epoch:

  Episode or epoch used for time-dependent POMDPs. Epochs are internally
  converted to the episode using the model horizon.

- state_col:

  colors used to represent the states.

- simplify_transitions:

  logical; combine parallel transition arcs into a single arc.

- remove_unavailable_actions:

  logical; don't show arrows for unavailable actions.

- main:

  a main title for the plot.

- ...:

  further arguments are passed on to
  [`igraph::plot.igraph()`](https://r.igraph.org/reference/plot.igraph.html).

## Value

The transition model as an igraph object.

## Details

The transition model of a POMDP/MDP is a Markov Chain. This function
extracts the transition model as an igraph object.

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
[`update_belief()`](http://michael.hahsler.net/pomdp/reference/update_belief.md),
[`value_function()`](http://michael.hahsler.net/pomdp/reference/value_function.md),
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
[`value_function()`](http://michael.hahsler.net/pomdp/reference/value_function.md)

## Examples

``` r
data("Tiger")

g <- transition_graph(Tiger)
g
#> IGRAPH cac2cd9 DN-- 2 4 -- 
#> + attr: name (v/c), color (v/c), label (e/c)
#> + edges from cac2cd9 (vertex names):
#> [1] tiger-left ->tiger-left  tiger-left ->tiger-right tiger-right->tiger-left 
#> [4] tiger-right->tiger-right

plot_transition_graph(Tiger)

plot_transition_graph(Tiger, vertex.size = 20, 
                      edge.label.cex = .5, edge.arrow.size = .5, margin = .5)

plot_transition_graph(Tiger, vertex.size = 60, 
                      edge.label = NA, edge.arrow.size = .5, 
                      layout = rbind(c(-1,0), c(+1,0)), rescale = FALSE)


## Plot an individual graph for each actions and use a manual layout.
for (a in Tiger$actions) {
 plot_transition_graph(Tiger, action = a, 
                        layout = rbind(c(-1,0), c(+1,0)), rescale = FALSE,
                        main = paste("action:", a))
}




## Plot using the igraph library
library(igraph)
plot(g)


# plot with a fixed layout and curved edges
plot(g,
 layout = rbind(c(-1, 0), c(1, 0)), rescale = FALSE,
 edge.curved = curve_multiple_directed(g, .8),
 edge.loop.angle = -pi / 4,
 vertex.size = 60
 )


## Use visNetwork (if installed)
if(require(visNetwork)) {

g_vn <- toVisNetworkData(g)
nodes <- g_vn$nodes
edges <- g_vn$edges

# add manual layout
nodes$x <- c(-1, 1) * 200
nodes$y <- 0

visNetwork(nodes, edges)  %>%
  visNodes(physics = FALSE) %>%
  visEdges(smooth = list(type = "curvedCW", roundness = .6), arrows = "to")
}
#> Loading required package: visNetwork

{"x":{"nodes":{"id":["tiger-left","tiger-right"],"color":["#E41A1C","#377EB8"],"label":["tiger-left","tiger-right"],"x":[-200,200],"y":[0,0]},"edges":{"from":["tiger-left","tiger-left","tiger-right","tiger-right"],"to":["tiger-left","tiger-right","tiger-left","tiger-right"],"label":["listen/\nopen-left (0.5)/\nopen-right (0.5)","open-left (0.5)/\nopen-right (0.5)","open-left (0.5)/\nopen-right (0.5)","listen/\nopen-left (0.5)/\nopen-right (0.5)"]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot","physics":false},"manipulation":{"enabled":false},"edges":{"arrows":"to","smooth":{"type":"curvedCW","roundness":0.6}}},"groups":null,"width":null,"height":null,"idselection":{"enabled":false},"byselection":{"enabled":false},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)"},"evals":[],"jsHooks":[]}
```
