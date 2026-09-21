.validate_labels <- function(x, name) {
  if (!is.character(x) || length(x) == 0L || anyNA(x) || any(!nzchar(x)))
    stop(name, " must be a non-empty character vector without missing values.", call. = FALSE)
  if (anyDuplicated(x))
    stop(name, " must contain unique values.", call. = FALSE)
  x
}

.validate_scalar_logical <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x))
    stop(name, " must be TRUE or FALSE.", call. = FALSE)
  x
}

.validate_class <- function(x, name, class) {
  if (!inherits(x, class))
    stop(
      "`", name, "` must be an object of class \"", class, "\".",
      call. = FALSE
    )
  invisible(x)
}

.validate_discount <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x <= 0 || x > 1)
    stop("discount must be a single finite value in the range (0, 1].", call. = FALSE)
  as.numeric(x)
}

.validate_positive_integer <- function(x, name, allow_inf = FALSE,
                                       allow_vector = FALSE) {
  if (!is.numeric(x) || length(x) == 0L || anyNA(x) ||
      (!allow_vector && length(x) != 1L) ||
      any(x <= 0) || any(is.finite(x) & x != floor(x)) ||
      (!allow_inf && any(!is.finite(x))))
    stop(
      name, " must be ",
      if (allow_vector) "a vector of " else "a ",
      "positive integer", if (allow_vector) "s" else "",
      if (allow_inf) " or Inf." else ".",
      call. = FALSE
    )
  x
}

.match_model_value <- function(value, choices, name) {
  if (is.null(value))
    return(NULL)
  if (is.factor(value))
    value <- as.character(value)
  if (length(value) != 1L || anyNA(value))
    stop(name, " must specify exactly one value.", call. = FALSE)
  if (is.numeric(value)) {
    if (!is.finite(value) || value != floor(value) || value < 1L || value > length(choices))
      stop(name, " index must be between 1 and ", length(choices), ".", call. = FALSE)
    return(choices[as.integer(value)])
  }
  if (!is.character(value) || !value %in% choices)
    stop("Unknown ", name, ": ", sQuote(as.character(value)), ".", call. = FALSE)
  value
}

.match_model_values <- function(value, choices, name) {
  if (is.null(value))
    return(NULL)
  if (is.factor(value))
    value <- as.character(value)
  if (length(value) == 0L || anyNA(value))
    stop(name, " must not be empty or contain missing values.", call. = FALSE)
  unname(vapply(seq_along(value), function(i)
    .match_model_value(value[i], choices, name), character(1)))
}

.validate_episode_epoch <- function(x, episode = NULL, epoch = NULL) {
  if (!is.null(episode) && !is.null(epoch))
    stop("Specify only one of episode and epoch.", call. = FALSE)

  if (!is.null(epoch)) {
    .validate_positive_integer(epoch, "epoch")
    return(epoch_to_episode(x, epoch))
  }

  if (is.null(episode))
    return(1L)

  .validate_positive_integer(episode, "episode")
  n_episodes <- if (is_timedependent_POMDP(x)) length(x$horizon) else 1L
  if (episode > n_episodes)
    stop("episode must be between 1 and ", n_episodes, ".", call. = FALSE)
  as.integer(episode)
}

.validate_belief <- function(belief, model, allow_matrix = TRUE) {
  states <- as.character(model$states)

  if (is.matrix(belief)) {
    if (!allow_matrix)
      stop("belief must specify a single belief state.", call. = FALSE)
    if (!is.numeric(belief) || ncol(belief) != length(states) ||
        anyNA(belief) || any(!is.finite(belief)) || any(belief < 0) ||
        any(abs(rowSums(belief) - 1) > sqrt(.Machine$double.eps)))
      stop("belief matrix must contain probability rows with one column per state.", call. = FALSE)
    if (!is.null(colnames(belief))) {
      if (anyDuplicated(colnames(belief)) || !setequal(colnames(belief), states))
        stop("belief matrix column names must match the model states.", call. = FALSE)
      belief <- belief[, states, drop = FALSE]
    }
    colnames(belief) <- states
    return(belief)
  }

  if (length(belief) == 0L || anyNA(belief))
    stop("belief must not be empty or contain missing values.", call. = FALSE)

  if (is.numeric(belief) && length(belief) == length(states) &&
      all(is.finite(belief)) && all(belief >= 0) && all(belief <= 1)) {
    if (abs(sum(belief) - 1) > sqrt(.Machine$double.eps))
      stop("belief probabilities must add up to 1.", call. = FALSE)
    if (!is.null(names(belief))) {
      if (anyDuplicated(names(belief)) || !setequal(names(belief), states))
        stop("belief names must match the model states.", call. = FALSE)
      belief <- belief[states]
    }
    return(belief)
  }

  if (is.numeric(belief)) {
    if (any(!is.finite(belief)) || any(belief != floor(belief)) || any(belief == 0) ||
        any(abs(belief) > length(states)) || (any(belief < 0) && any(belief > 0)))
      stop("belief state indices are invalid.", call. = FALSE)
    return(belief)
  }

  if (is.character(belief)) {
    if (identical(belief, "uniform"))
      return(belief)
    excluded <- identical(belief[1L], "-")
    values <- if (excluded) belief[-1L] else belief
    if (length(values) == 0L || any(!values %in% states) || anyDuplicated(values))
      stop("belief contains unknown or duplicate states.", call. = FALSE)
    return(belief)
  }

  stop("belief must be a probability vector, state selection, or probability matrix.", call. = FALSE)
}
