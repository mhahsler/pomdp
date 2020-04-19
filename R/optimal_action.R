# find the optimal action for a belief point

optimal_action <- function(model, belief, epoch = 1)  reward(model, belief = belief, epoch = epoch)[["action"]]