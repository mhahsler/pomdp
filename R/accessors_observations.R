# Accessor Functions for observations
#
# Representations:
# Default:
# * Sparse (list):
#     Obs: A action list -> end.state x observation sparse matrix
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
#' @export
observation_matrix <-
  function(x,
           action = NULL,
           end.state = NULL,
           observation = NULL,
           episode = NULL,
           epoch = NULL,
           sparse = FALSE,
           trans_keyword = TRUE) {
    value_matrix(x,
                 "observation_prob",
                 action,
                 end.state,
                 observation,
                 episode,
                 epoch,
                 sparse,
                 trans_keyword)
    
  }

#' @rdname accessors
#' @export
observation_val <-
  function(x,
           action,
           end.state,
           observation,
           episode = NULL,
           epoch = NULL) {
    #warning("observation_val is deprecated. Use observation_matrix instead!")
    value_matrix(x,
                 "observation_prob",
                 action,
                 end.state,
                 observation,
                 episode,
                 epoch)
  }
