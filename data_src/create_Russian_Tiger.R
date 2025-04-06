library(pomdp)

RussianTiger <- POMDP(
  name = "Russian Tiger Problem",
  discount = 1,
  states = c("tiger-left" , "tiger-right", "done"),
  actions = c("listen", "open-left", "open-right", "nothing"),
  observations = c("tiger-left", "tiger-right", "done"),
  start = c(.5, .5, 0),
  
  transition_prob = list(
    "listen" =     "identity", 
    "open-left" =  rbind(c(0, 0, 1),
                         c(0, 0, 1),
                         c(0, 0, 1)),
    "open-right" = rbind(c(0, 0, 1),
                         c(0, 0, 1),
                         c(0, 0, 1)),
    "nothing" =     "identity"
    ),
  
  observation_prob = list(
    "listen" = rbind(c(0.85, 0.15, 0), 
                     c(0.15, 0.85, 0),
                     c(1/2,   1/2,    0 )),
    "open-left" =  rbind(c(0, 0, 1),
                         c(0, 0, 1),
                         c(0, 0, 1)),
    "open-right" = rbind(c(0, 0, 1),
                         c(0, 0, 1),
                         c(0, 0, 1)),
    "nothing" =  rbind(c(0, 0, 1),
                       c(0, 0, 1),
                       c(0, 0, 1))
  ),
  
  # the reward helper expects: action, start.state, end.state, observation, value
  
  
  reward = rbind(
    R_("listen",                                    value = -1),
    R_("nothing",                                   value = -Inf),
    R_(              start.state = "done",        value = -Inf),
    R_("nothing",    start.state = "done",        value = 0),
    R_("open-left",  start.state = "tiger-left",  value = -1000),
    R_("open-right", start.state = "tiger-right", value = -1000),
    R_("open-left",  start.state = "tiger-right", value = 10),
    R_("open-right", start.state = "tiger-left",  value = 10)
  )
)


RussianTiger

which(absorbing_states(RussianTiger))


plot_transition_graph(RussianTiger)
sol <- solve_POMDP(RussianTiger)
policy(sol)

plot_policy_graph(sol)

simulate_POMDP(sol, n = 2, return_beliefs = TRUE, 
               return_trajectories = TRUE, engine = 'r', horizon = 100)

### TODO
#simulate_POMDP(sol, n = 2, return_beliefs = TRUE, 
#               return_trajectories = TRUE)


save(RussianTiger, file = "data/RussianTiger.rda")


