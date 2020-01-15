transition_matrix <- function(x) 
  .translate_probabilities(x, field = "transition_prob", from = "states", to = "states")

observation_matrix <- function(x) 
  .translate_probabilities(x, field = "observation_prob", from = "states", to = "observations")

reward_matrix <- function(x)
  .translate_reward(x)