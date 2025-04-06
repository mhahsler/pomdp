library(pomdp)

gw <- gridworld_init(dim = c(1,9), absorbing_states = "s(1,5)")
gw


## the observation is how far from the goal away we are.
observations <- paste0("o", 0:5)
observation_func <- function(action, end.state, observation) {
  rc <- gridworld_s2rc(end.state)
    as.integer(paste0("o", as.character(abs(rc[2]-5))) == observation)
}

reward <- rbind(R_(value = -1), 
                R_(end.state = "s(1,5)", value = 10)
                ) 
                #R_(action = "up", value = -Inf),
                #R_(action = "down", value = -Inf))


line_maze <- POMDP(states = gw$states, 
                   #actions = c(gw$actions, "nothing"), 
                   #actions = gw$actions, 
                   actions = c("right", "left"), 
                   observations = observations, 
                   transition_prob = gw$transition_prob, 
                   observation_prob = observation_func, 
                   reward = reward,
                   info = gw$info
)





line_maze



col <- hcl.colors(9, palette = "Blue-Red")

plot_transition_graph(line_maze, 
                      layout = cbind(seq(-1, 1, length.out = 9), 0), rescale = FALSE,
                      edge.arrow.size = .2, vertex.color = col)

gridworld_plot_transition_graph(line_maze, vertex.size = 100, vertex.color = col)
gridworld_plot_transition_graph(line_maze, vertex.size = 100)


sol <- solve_POMDP(line_maze, method = "incprune")
sol <- solve_POMDP(line_maze, parameter = list(fg_points = 10000))

policy(sol)

plot_policy_graph(sol, edge.arrow.size =.2, state_col = col)
plot_policy_graph(sol, edge.arrow.size =.2, state_col = col, remove_unreachable_nodes = FALSE)

simulate_POMDP(normalize_POMDP(sol), n = 5, return_trajectories = TRUE)



################# start here

library(pomdp)

# single start / no restart
lm <- gridworld_maze_MDP(dim = c(1,9), start = "s(1,2)", 
                         goal = "s(1,5)", restart = FALSE, name = "Line Maze")
lm$actions <- c("left", "right")

#plot_transition_graph(lm, layout = cbind(seq(-1, 1, length.out = length(lm$states)), 0), 
#                      rescale = FALSE, edge.arrow.size = .2)

gridworld_plot_transition_graph(lm, vertex.size = 70)

s <- solve_MDP(lm)
policy(s)
gridworld_plot_policy(s)


# single start / restart
lm <- gridworld_maze_MDP(dim = c(1,9), start = "s(1,2)", 
                         goal = "s(1,5)", restart = TRUE, name = "Line Maze")
lm$actions <- c("left", "right", "restart")

plot_transition_graph(lm, edge.arrow.size = .2)

s <- solve_MDP(lm)
policy(s)
gridworld_plot_policy(s)

lm <- gridworld_maze_MDP(dim = c(1,9), start = "uniform", 
                         goal = "s(1,5)", restart = FALSE, name = "Line Maze")
lm$actions <- c("left", "right")


plot_transition_graph(lm)

plot_transition_graph(lm, edge.arrow.size = .2, layout = cbind(seq(-1, 1, length.out = length(lm$states)), 0), 
                      rescale = FALSE)

plot_transition_graph(lm, edge.arrow.size = .2)

gridworld_plot_transition_graph(lm, vertex.size = 80)

s <- solve_MDP(lm)
policy(s)
gridworld_plot_policy(s)

# uniform / restart
lm <- gridworld_maze_MDP(dim = c(1,9), start = "uniform", 
                         goal = "s(1,5)", restart = TRUE, name = "Line Maze")
# remove unused actions!
lm$actions <- c("left", "right", "restart")


plot_transition_graph(lm, layout = cbind(seq(-1, 1, length.out = length(lm$states)), 0), 
                      rescale = FALSE, edge.arrow.size = .2)

plot_transition_graph(lm, edge.arrow.size = .2, layout = igraph::layout.circle)


s <- solve_MDP(lm)
policy(s)
gridworld_plot_policy(s)


# q-learning
s <- solve_MDP(lm, method = "q", horizon = 100, N = 100)
policy(s)

s <- solve_MDP(lm, method = "sarsa", horizon = 100, N = 100)
policy(s)
s$solution

s <- solve_MDP(lm, method = "exp", horizon = 100, N = 100)
policy(s)



#Make this into a POMDP


# line_maze <- POMDP(states = lm$states, 
#                    actions = c("left", "right"),
#                    start = lm$start,
#                    transition_prob = lm$transition_prob, 
#                    reward = reward,
#                    observations = observations, 
#                    observation_prob = observation_func, 
#                    info = lm$info,
#                    name = "Line Maze"
# )


lm_POMDP <- make_partially_observable(lm, observations = observations, observation_prob = observation_func)

gridworld_plot_transition_graph(lm_POMDP, vertex.size = 100)

sol <- solve_POMDP(lm_POMDP)
