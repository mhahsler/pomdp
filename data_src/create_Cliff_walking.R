library(pomdp)

# Here is the complete problem definition:
nrows <- 4
ncols <- 12

gw <- gridworld_init(dim = c(nrows, ncols), unreachable_states =
                       c("s(4,2)", "s(4,3)", "s(4,4)", "s(4,5)", 
                         "s(4,6)", "s(4,7)", "s(4,8)", "s(4,9)", 
                         "s(4,10)", "s(4,11)"))
gridworld_matrix(gw)

S <- gw$states
A <- gw$actions

START = "s(4,1)"
GOAL = "s(4,12)"

T <- function(action, start.state, end.state) {
  action <- match.arg(action, choices = A)
  
  # GOAL is absorbing
  if (start.state == GOAL)
    return(as.integer(end.state == start.state))
  
  # the cliff leads to START instead
  start_rc <- gridworld_s2rc(start.state)
  if ((action == "down" &&
       start_rc[1] == 3 &&
       start_rc[2] >= 2 && start_rc[2] <= 11) ||
      (start.state == "s(4,1)" && action == "right") ||
      (start.state == "s(4,12)" && action == "left"))
    return(as.integer(end.state == START))
  
  # rest of the actions are normal
  return(gw$transition_prob(action, start.state, end.state))
}

T("up", "s(4,1)", "s(3,1)")
T("right", "s(4,1)", "s(4,1)")
T("right", "s(4,1)", "s(4,2)") # 0
T("right", "s(3,1)", "s(3,2)")
T("right", "s(3,2)", "s(3,1)") # 0
T("down", "s(3,5)", START)
T("left", "s(1,1)", "s(1,1)")
T("up", "s(1,1)", "s(1,1)")
T("down", GOAL, GOAL)

R <- rbind(
  R_(value = -1),
  R_(end.state = START, value = -100),
  R_(
    action = "down",
    start.state = "s(3,1)",
    end.state = START,
    value = -1
  ),
  R_(
    start.state = GOAL,
    end.state = GOAL,
    value = 0
  )
)

R

Cliff_walking <- MDP(
  name = "Cliff Walking Gridworld",
  discount = 1,
  horizon = Inf,
  states = S,
  actions = A,
  start = START,
  transition_prob = T,
  reward = R,
  info = list(
    gridworld_dim = c(4, 12),
    gridworld_labels = list("s(4,1)" = "Start",
                            "s(4,12)" = "Goal")
  )
)

Cliff_walking

#Cliff_walking <- remove_unreachable_states(Cliff_walking)
#Cliff_walking

save(Cliff_walking, file = "data/Cliff_walking.rda")
