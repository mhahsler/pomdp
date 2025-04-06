library(pomdp)

# Here is the complete problem definition:
nrows <- 7
ncols <- 10

gw <- gridworld_init(dim = c(nrows, ncols))
gridworld_matrix(gw)

S <- gw$states
A <- gw$actions

START = "s(4,1)"
GOAL = "s(4,8)"

T <- function(action, start.state, end.state) {
  action <- match.arg(action, choices = A)
  
  # GOAL is absorbing
  if (start.state == GOAL)
    return(as.integer(end.state == start.state))
  
  # Wind towards north
  wind <- c(0, 0, 0, 1, 1, 1, 2, 2, 1, 0)
  
  rc <- gridworld_s2rc(start.state)
  w <- wind[rc[2]]
  
  try_move <- function(a, rc) {
    rc_new <- switch(
      a,
      "up" =     c(rc[1] - 1, rc[2]),
      "down" =   c(rc[1] + 1, rc[2]),
      "left" =   c(rc[1],     rc[2] - 1),
      "right" =  c(rc[1],     rc[2] + 1)
    )
    if (rc_new[1] >= 1 &&
        rc_new[1] <= 7 && rc_new[2] >= 1 && rc_new[2] <= 10)
      return(rc_new)
    else
      return(rc)
  }
  
  rc <- try_move(action, rc) 
    
  for (i in seq_len(w)) 
    rc <- try_move("up", rc) 
      
  new.state <- gridworld_rc2s(rc)
  
  
  return(as.integer(new.state == end.state))
}

T("left", "s(4,9)", "s(3,8)")
T("left", "s(4,9)", "s(3,9)")
T("right", "s(1,1)", "s(1,2)")
T("right", GOAL, GOAL)
T("up", "s(6,8)", "s(3,8)")

R <- rbind(
  R_(value = -1),
  R_(
    start.state = GOAL,
    end.state = GOAL,
    value = 0
  )
)

R

Windy_gridworld <- MDP(
  name = "Windy Gridworld",
  discount = 1,
  horizon = Inf,
  states = S,
  actions = A,
  start = START,
  transition_prob = T,
  reward = R,
  info = list(
    gridworld_dim = c(7,10),
    gridworld_labels = list("s(4,1)" = "Start",
                            "s(4,8)" = "Goal")
  )
)

Windy_gridworld


gridworld_matrix(Windy_gridworld, what = "reachable")
w2 <- remove_unreachable_states(Windy_gridworld)
gridworld_matrix(w2, what = "reachable")
w2 <- remove_unreachable_states(w2)
gridworld_matrix(w2, what = "reachable")

gridworld_matrix(w2, what = "labels")
gridworld_matrix(w2)

gridworld_matrix(Windy_gridworld)
gridworld_matrix(Windy_gridworld, what = "labels")

sol <- solve_MDP(Windy_gridworld)
gridworld_plot_policy(sol)



save(Windy_gridworld, file = "data/Windy_gridworld.rda")
