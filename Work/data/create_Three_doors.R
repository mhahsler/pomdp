library(pomdp)
Three_doors <- POMDP(
  name = "3-Door Tiger Problem",
  
  discount = 0.75,
  
  states = c("tiger-left" , "tiger-center", "tiger-right"),
  actions = c("listen", "open-left", "open-center", "open-right"),
  observations = c("tiger-left", "tiger-center", "tiger-right"),
  
  start = "uniform",
  
  transition_prob = list(
    "listen" =     "identity", 
    "open-left" =  "uniform", 
    "open-center" =  "uniform", 
    "open-right" = "uniform"),
  
  observation_prob = list(
    # state x observation
    "listen" = rbind(
      c(0.80, 0.15, 0.05), 
      c(0.25, 0.50, 0.25),
      c(0.05, 0.15, 0.80)
      ),
    "open-left" =  "uniform",
    "open-center" =  "uniform",
    "open-right" = "uniform"),
  
  # the rew helper expects: action, start.state, end.state, observation, value
  reward = rbind(
    R_("listen",     "*",           "*", "*", -1  ),
    R_("open-left",  "*",           "*", "*", 10),
    R_("open-center","*",           "*", "*", 10  ),
    R_("open-right", "*",           "*", "*", 10  ),
    R_("open-left",  "tiger-left",  "*", "*", -100),
    R_("open-center","tiger-center","*", "*", -100),
    R_("open-right", "tiger-right", "*", "*", -100)
  )
)

Three_doors
sol <- solve_POMDP(Three_doors)

plot(sol)
reward(sol)

plot_belief_space(sol, projection = 1:3, n = 10000)

save(Three_doors, file = "data/Three_doors.rda")

