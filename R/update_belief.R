# Updating the belief state: update for a single belief vector, one action, and one observation.
# $$b'(s') = \eta O(o | s',a) \sum_{s \in S} T(s' | s,a) b(s)$$
# $$\eta = 1/ \sum_{s' \in S}[ O(o | s',a) \sum_{s \in S} T(s' | s,a) b(s)]$$
.update_belief <-
  function(belief,
    action,
    observation,
    Tr,
    Ob,
    digits = 7) {
    belief <-
      Ob[[action]][, observation, drop = FALSE] * crossprod(Tr[[action]], cbind(belief))
    belief <- belief / sum(belief)
   
    belief <- round_stochastic_int(belief, digits)
    
    drop(belief)
  }

.update_belief_vec <- Vectorize(
  .update_belief,
  vectorize.args = c("action", "observation"),
  SIMPLIFY = TRUE
)
  

#' Belief Update
#'
#' Update the belief given a taken action and observation.
#'
#' @details
#' Update the belief state \eqn{b} (`belief`) with an action \eqn{a} and observation \eqn{o}. The new
#' belief state \eqn{b'} is:
#'
#' \deqn{b'(s') = \eta O(o | s',a) \sum_{s \in S} T(s' | s,a) b(s)}
#'
#' where \eqn{\eta = 1/ \sum_{s' \in S}[ O(o | s',a) \sum_{s \in S} T(s' | s,a) b(s)]} normalizes the new belief state so the probabilities add up to one.
#'
#' @family POMDP
#'
#' @param model a [POMDP] object.
#' @param belief the current belief state.
#' Defaults to the start belief state specified in
#' the model or "uniform".
#' @param action the taken action. Can also be a vector of multiple actions or, if missing, then all actions are evaluated.
#' @param observation the received observation. Can also be a vector of multiple observations or, if missing, then all observations are evaluated.
#' @param episode Use transition and observation matrices for the given episode
#' for time-dependent POMDPs (see [POMDP]).
#' @param digits round decimals.
#' @param drop logical; drop the result to a vector if only a single belief
#' state is returned.
#' 
#' @returns returns the updated belief state as a named vector. 
#'   If `action` or `observations` is a vector with multiple elements ot missing, then a matrix with all
#'   resulting belief states is returned.
#' 
#' @author Michael Hahsler
#' @examples
#' data(Tiger)
#'
#' update_belief(c(.5,.5), model = Tiger)
#' update_belief(c(.5,.5), action = "listen", observation = "tiger-left", model = Tiger)
#' update_belief(c(.15,.85), action = "listen", observation = "tiger-right", model = Tiger)
#'
#' @export
update_belief <-
  function(model,
    belief = NULL,
    action = NULL,
    observation = NULL,
    episode = 1,
    digits = 7,
    drop = TRUE) {
    # belief has to be a single row vector
    belief <- .translate_belief(belief, model = model)
    if (!is.vector(belief))
      stop("belief has to be specified as a numeric vector.")
    
    Ob <- observation_matrix(model, episode = episode)
    Tr <- transition_matrix(model, episode = episode)
    
    if (is.null(action))
      action <- as.character(model$actions)
    if (is.null(observation))
      observation <- as.character(model$observations)
    
    g <- expand.grid(action, observation, stringsAsFactors = FALSE)
    
    b <- t(.update_belief_vec(belief, g[, 1], g[, 2], Tr, Ob, digits))
    rownames(b) <- apply(g, MARGIN = 1, paste, collapse = "+")
    colnames(b) <- as.character(model$states)
    
    if (drop)
      b <- drop(b)
    b
  }

