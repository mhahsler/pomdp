library(pomdp)
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
    "open-right" = "uniform"),
  
  observation_prob = list(
    "listen" = rbind(c(0.85, 0.15), 
      c(0.15, 0.85)),
    "open-left" =  "uniform",
    "open-right" = "uniform"),
  
  # the rew helper expects: action, start.state, end.state, observation, value
  reward = rbind(
    R_("listen",     "*",           "*", "*", -1  ),
    R_("open-left",  "tiger-left",  "*", "*", -100),
    R_("open-left",  "tiger-right", "*", "*", 10  ),
    R_("open-right", "tiger-left",  "*", "*", 10  ),
    R_("open-right", "tiger-right", "*", "*", -100)
  )
)


Tiger

save(Tiger, file = "data/Tiger.rda")

