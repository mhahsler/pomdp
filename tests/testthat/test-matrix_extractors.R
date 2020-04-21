library("testthat")
library("pomdp")

context("matrix extractors")

# make sure extractrs reorder acording to actions and observations...

Tiger <- POMDP(
  name = "Tiger Problem",
  
  discount = 0.75,
  
  states = c("tiger-left" , "tiger-right"),
  actions = c("listen", "open-left", "open-right"),
  observations = c("tiger-left", "tiger-right"),
  
  start = "uniform",

  # this is of order
  transition_prob = list(
    "open-left" =  "uniform", 
    "listen" =     "identity", 
    "open-right" = rbind(c(.5, .5), c(.5, .5))),
  
  observation_prob = list(
    "open-right" = "uniform",
    "listen" = rbind(c(0.85, 0.15), 
      c(0.15, 0.85)),
    "open-left" =  "uniform"),
  
  # the reward helper expects: action, start.state, end.state, observation, value
  reward = rbind(
    R_("listen",                    v =   -1),
    R_("open-left",  "tiger-left",  v = -100),
    R_("open-left",  "tiger-right", v =   10),
    R_("open-right", "tiger-left",  v =   10),
    R_("open-right", "tiger-right", v = -100)
  )
)

Tiger

tm <- transition_matrix(Tiger)
expect_equal(names(tm), as.character(Tiger$model$actions))

om <- observation_matrix(Tiger)
expect_equal(names(om), as.character(Tiger$model$actions))

rew <- reward_matrix(Tiger)
expect_equal(names(rew), as.character(Tiger$model$actions))
expect_equal(names(rew[[1]]), as.character(Tiger$model$states))


Tiger <- POMDP(
  name = "Tiger Problem",
  
  discount = 0.75,
  
  states = c("tiger-left" , "tiger-right"),
  actions = c("listen", "open-left", "open-right"),
  observations = c("tiger-left", "tiger-right"),
  
  start = "uniform",

  # this is of order
  transition_prob = list(
     "uniform", 
      "identity", 
    rbind(c(.5, .5), c(.5, .5))),
  
  observation_prob = list(
     "uniform",
     rbind(c(0.85, 0.15), 
      c(0.15, 0.85)),
     "uniform"),
  
  # the reward helper expects: action, start.state, end.state, observation, value
  reward = rbind(
    R_("listen",                    v =   -1),
    R_("open-left",  "tiger-left",  v = -100),
    R_("open-left",  "tiger-right", v =   10),
    R_("open-right", "tiger-left",  v =   10),
    R_("open-right", "tiger-right", v = -100)
  )
)

Tiger

tm <- transition_matrix(Tiger)
expect_equal(names(tm), as.character(Tiger$model$actions))

om <- observation_matrix(Tiger)
expect_equal(names(om), as.character(Tiger$model$actions))

rew <- reward_matrix(Tiger)
expect_equal(names(rew), as.character(Tiger$model$actions))
expect_equal(names(rew[[1]]), as.character(Tiger$model$states))

