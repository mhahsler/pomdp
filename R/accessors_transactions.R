# Accessor Functions for transitions 
#
# Representations:
# Default:
# * Sparse (list):
#     Trans: A action list -> start.state x end.state sparse matrix
#
# Others
# * Dense (list): Same as sparse with dense matrices
# * df: A data.frame with value
# * A function can be converted to a list
#
# sparse = NULL translates functions/data frames/strings
#


#' @include accessors.R
#' @rdname accessors
#' @importFrom markovDP transition_matrix
#' @export
transition_matrix.POMDP <-
  function(x,
           action = NULL,
           start.state = NULL,
           end.state = NULL,
           episode = NULL,
           epoch = NULL,
           sparse = FALSE,
           trans_keyword = TRUE) {
    value_matrix(x,
                 "transition_prob",
                 action,
                 start.state,
                 end.state,
                 episode,
                 epoch,
                 sparse,
                 trans_keyword)
    
  }

#' @rdname accessors
#' @export
transition_val <-
  function(x,
           action,
           start.state,
           end.state,
           episode = NULL,
           epoch = NULL) {
    #warning("transition_val is deprecated. Use reward_matrix instead!")
    value_matrix(x,
                 "transition_prob",
                 action,
                 start.state,
                 end.state,
                 episode,
                 epoch)
  }

