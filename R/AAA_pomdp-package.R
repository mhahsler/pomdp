#' @title `r packageDescription("pomdp")$Package`: `r packageDescription("pomdp")$Title`
#'
#' @description Provides the infrastructure to define and analyze the solutions of Partially Observable Markov Decision Process (POMDP) models. Interfaces for various exact and approximate solution algorithms are available including value iteration, Point-Based Value Iteration (PBVI) and Successive Approximations of the Reachable Space under Optimal Policies (SARSOP).
#'
#' @section Key functions:
#' - Problem specification: [POMDP], [MDP]
#' - Solvers: [solve_POMDP()], [solve_MDP()], [solve_SARSOP()]
#'
#' @author Michael Hahsler
#' @docType package
#' @name pomdp-package
#' 
#' @import Rcpp
#' @useDynLib pomdp, .registration=TRUE
NULL
