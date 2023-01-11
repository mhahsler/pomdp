library("testthat")
library("pomdp")

## context("matrix extractors")

# make sure extractors reorder according to actions and observations...
data(Tiger)

tm <- transition_matrix(Tiger)
expect_equal(names(tm), as.character(Tiger$actions))

om <- observation_matrix(Tiger)
expect_equal(names(om), as.character(Tiger$actions))

rew <- reward_matrix(Tiger)
expect_equal(names(rew), as.character(Tiger$actions))
expect_equal(names(rew[[1]]), as.character(Tiger$states))


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

# Check functions
trans_f <- function(action, start.state, end.state) {
  ## listen has an identity matrix
  if(action == 'listen')
    if(end.state == start.state) return(1)
  else return(0)
  
  # other actions have a uniform distribution
  return(1/2)
}

obs_f <- function(action, end.state, observation) {
  if(action == 'listen')
    if(end.state == observation) return(0.85)
    else return(0.15)
  
  return(1/2)
}

rew_f <- function(action, start.state, end.state, observation) {
  if(action == 'listen') return(-1)
  if(action == 'open-left' && start.state == 'tiger-left') return(-100)
  if(action == 'open-left' && start.state == 'tiger-right') return(10)
  if(action == 'open-right' && start.state == 'tiger-left') return(10)
  if(action == 'open-right' && start.state == 'tiger-right') return(-100)
  stop('Not possible')
}

Tiger_func <- POMDP(
  name = "Tiger Problem",
  discount = 0.75,
  states = c("tiger-left" , "tiger-right"),
  actions = c("listen", "open-left", "open-right"),
  observations = c("tiger-left", "tiger-right"),
  start = "uniform",
  
  transition_prob = trans_f,
  observation_prob = obs_f,
  reward = rew_f
)

tm_func <- transition_matrix(Tiger_func)
expect_equal(tm_func, tm)

om_func <- observation_matrix(Tiger_func)
expect_equal(om_func, om)

rew_func <- reward_matrix(Tiger_func)
expect_equal(rew_func, rew)


## check for factor translation in data.frames (pre R 4.0)
objPOMDP <- POMDP(
  name = "Check Factors",
  discount = 0.8, 
  states = c("A-first-state", "A-plus-1-state", "A-plus-2-state"), 
  actions = c("Start", "Pause", "End"),
  observations = c("A-first-state", "A-plus-1-state", "A-plus-2-state"),
  horizon = 4, 
  start = c(0.8, 0.1, 0.1), 
  
  reward = rbind(
    R_("Start","A-plus-1-state", v = -100),
    R_("Start","A-plus-2-state", v = -125),
    R_("Start","A-first-state", v = 75),
    R_("Pause", v = -15),
    R_("End", v = -45)
  ),
  
  transition_prob = rbind(
    T_("Start", "A-plus-1-state", "A-plus-1-state", 0.8), #(2, 2)
    T_("Start", "A-plus-1-state", "A-plus-2-state", 0.05), #(2, 3)
    T_("Start", "A-plus-1-state", "A-first-state", 0.15), #(2, 1)
    T_("Start", "A-first-state", "A-plus-1-state", 0.5), #(1, 2)
    T_("Start", "A-first-state", "A-plus-2-state", 0.1), #(1, 3)
    T_("Start", "A-first-state", "A-first-state", 0.4), #(1, 1)
    T_("Start", "A-plus-2-state", "A-plus-1-state", 0.3), #(3, 2)
    T_("Start", "A-plus-2-state", "A-plus-2-state", 0.55), #(3, 3)
    T_("Start", "A-plus-2-state", "A-first-state", 0.15), #(3, 1)
    
    T_("Pause", "A-plus-1-state", "A-plus-1-state", 0.8), #(2, 2)
    T_("Pause", "A-plus-1-state", "A-plus-2-state", 0.05), #(2, 3)
    T_("Pause", "A-plus-1-state", "A-first-state", 0.15), #(2, 1)
    T_("Pause", "A-first-state", "A-plus-1-state", 0.5), #(1, 2)
    T_("Pause", "A-first-state", "A-plus-2-state", 0.1), #(1, 3)
    T_("Pause", "A-first-state", "A-first-state", 0.4), #(1, 1)
    T_("Pause", "A-plus-2-state", "A-plus-1-state", 0.3), #(3, 2)
    T_("Pause", "A-plus-2-state", "A-plus-2-state", 0.55), #(3, 3)
    T_("Pause", "A-plus-2-state", "A-first-state", 0.15), #(3, 1)
    
    T_("End", "A-plus-1-state", "A-plus-1-state", 0.8), #(2, 2)
    T_("End", "A-plus-1-state", "A-plus-2-state", 0.05), #(2, 3)
    T_("End", "A-plus-1-state", "A-first-state", 0.15), #(2, 1)
    T_("End", "A-first-state", "A-plus-1-state", 0.5), #(1, 2)
    T_("End", "A-first-state", "A-plus-2-state", 0.1), #(1, 3)
    T_("End", "A-first-state", "A-first-state", 0.4), #(1, 1)
    T_("End", "A-plus-2-state", "A-plus-1-state", 0.3), #(3, 2)
    T_("End", "A-plus-2-state", "A-plus-2-state", 0.55), #(3, 3)
    T_("End", "A-plus-2-state", "A-first-state", 0.15) #(3, 1)
  ),
  
  observation_prob = rbind(
    O_("Start", "A-plus-1-state", "A-plus-1-state", 0.8), #(2, 2)
    O_("Start", "A-plus-1-state", "A-plus-2-state", 0.2), #(2, 3)
    O_("Start", "A-first-state", "A-plus-1-state", 0.5), #(1, 2)
    O_("Start", "A-first-state", "A-first-state", 0.5), #(1, 1)
    O_("Start", "A-plus-2-state", "A-plus-1-state", 0.1), #(3, 2)
    O_("Start", "A-plus-2-state", "A-plus-2-state", 0.9), #(3, 3)
    
    O_("Pause", "A-plus-1-state", "A-plus-1-state", 0.8), #(2, 2)
    O_("Pause", "A-plus-1-state", "A-plus-2-state", 0.2), #(2, 3)
    O_("Pause", "A-first-state", "A-plus-1-state", 0.5), #(1, 2)
    O_("Pause", "A-first-state", "A-first-state", 0.5), #(1, 1)
    O_("Pause", "A-plus-2-state", "A-plus-1-state", 0.1), #(3, 2)
    O_("Pause", "A-plus-2-state", "A-plus-2-state", 0.9), #(3, 3)
    
    O_("End", "A-plus-1-state", "A-plus-1-state", 0.8), #(2, 2)
    O_("End", "A-plus-1-state", "A-plus-2-state", 0.2), #(2, 3)
    O_("End", "A-first-state", "A-plus-1-state", 0.5), #(1, 2)
    O_("End", "A-first-state", "A-first-state", 0.5), #(1, 1)
    O_("End", "A-plus-2-state", "A-plus-1-state", 0.1), #(3, 2)
    O_("End", "A-plus-2-state", "A-plus-2-state", 0.9) #(3, 3)
    
  )
)

#compare this
m_start <- transition_matrix(objPOMDP)$Start
#with this
df_start <- objPOMDP$transition_prob[objPOMDP$transition_prob$action == "Start",]

for(i in 1:nrow(df_start)) {
  expect_equal(m_start[df_start[i,]$start.state, df_start[i,]$end.state], df_start[i,]$probability)
}

# check sparse/dense representation

sparsePOMDP <- POMDP(
  name = "Sparse POMDP",
  discount = 0.8, 
  states = 5, 
  actions = 3,
  observations = 5,
  horizon = Inf, 
  start = 5, 
  
  transition_prob = list("identity",
    rbind(
      c(0, 1, 0, 0, 0),
      c(0, 1, 0, 0, 0),
      c(0, 0, .5, .5, 0),
      c(0, 1, 0, 0, 0),
      c(0, 1, 0, 0, 0)
    ),
    "uniform"), 
  
  reward = rbind(
    R_("*", "*", "*", v = -1),
    R_(1, 2, 1, 1, v = 100),
    R_(1, 3, v = 200),
    R_(2, 1, v = 0)
  ),
  
  observation_prob = list(
    "uniform",
    rbind(
      c(0, 1, 0, 0, 0), 
      c(0, 1, 0, 0, 0), 
      c(0, 0, .5, .5, 0), 
      c(0, 1, 0, 0, 0), 
      c(0, 1, 0, 0, 0)
      ),
    rbind(
      c(.2, .2, .2, .2, .2), 
      c(.2, .2, .2, .2, .2), 
      c(.2, .2, .2, .2, .2), 
      c(.2, .2, .2, .2, .2), 
      c(.2, .2, .2, .2, .2) 
    )
  )
)

densePOMDP <- normalize_POMDP(sparsePOMDP, sparse = FALSE)

expect_equal(sparsePOMDP$states, paste0("s", 1:5))
expect_equal(sparsePOMDP$observations, paste0("o", 1:5))
expect_equal(sparsePOMDP$actions, paste0("a", 1:3))

expect_equal(start_vector(sparsePOMDP), c(s1 = 0, s2 = 0, s3 = 0, s4 = 0, s5 = 1))

### keep it sparse (only identity was sparse)
m <- transition_matrix(sparsePOMDP, sparse = NULL)
expect_equal(names(m), sparsePOMDP$actions)
expect_true(inherits(m[[1]], "Matrix"))
expect_true(inherits(m[[2]], "matrix"))
expect_true(inherits(m[[3]], "matrix"))

### densify
m <- transition_matrix(sparsePOMDP, sparse = FALSE)
expect_equal(names(m), sparsePOMDP$actions)
expect_true(inherits(m[[1]], "matrix"))
expect_true(inherits(m[[2]], "matrix"))
expect_true(inherits(m[[3]], "matrix"))

### keep it dense
m <- transition_matrix(densePOMDP, sparse = NULL)
expect_equal(names(m), sparsePOMDP$actions)
expect_true(inherits(m[[1]], "matrix"))
expect_true(inherits(m[[2]], "matrix"))
expect_true(inherits(m[[3]], "matrix"))

### sparsify
m <- transition_matrix(densePOMDP, sparse = TRUE)
expect_equal(names(m), sparsePOMDP$actions)
expect_true(inherits(m[[1]], "Matrix"))
expect_true(inherits(m[[2]], "Matrix"))
expect_true(inherits(m[[3]], "matrix"))

m <- observation_matrix(sparsePOMDP, sparse = TRUE)
expect_equal(names(m), sparsePOMDP$actions)
expect_true(inherits(m[[1]], "matrix"))
expect_true(inherits(m[[2]], "Matrix"))
expect_true(inherits(m[[3]], "matrix"))

m <- transition_matrix(sparsePOMDP, sparse = FALSE)
expect_true(inherits(m[[1]], "matrix"))
expect_true(inherits(m[[2]], "matrix"))
expect_true(inherits(m[[3]], "matrix"))


rm <- reward_matrix(sparsePOMDP, sparse = TRUE)
expect_equal(names(rm), sparsePOMDP$actions)
m <- rm[[1]]
expect_equal(names(m), sparsePOMDP$states)
#print(rm)
expect_true(all(sapply(m, inherits, "matrix")))

m <- rm[[2]]
expect_true(inherits(m[[1]], "Matrix"))
