#' POMDP Example Files
#'
#' Some POMDP example files are shipped with the package.
#' 
#' Currently, the following POMDP example files are available:
#' * `"light_maze.POMDP"`: a simple maze introduced in Littman (2009).
#' * `"shuttle_95.POMDP"`: Transport goods between two space 
#'  stations (Chrisman, 1992).
#' * `"tiger_aaai.POMDP"`: Tiger Problem introduced in Cassandra et al (1994).
#' 
#' More files can be found at https://www.pomdp.org/examples/
#'
#' @name POMDP_example_files
#' @family POMDP_examples
#' @docType data
#' @keywords datasets
#' @references 
#' Anthony R. Cassandra, Leslie P Kaelbling, and Michael L. Littman (1994). 
#' Acting Optimally in Partially Observable Stochastic Domains. 
#' _In Proceedings of the Twelfth National Conference on Artificial 
#' Intelligence,_ pp. 1023-1028.
#' 
#' Lonnie Chrisman (1992), Reinforcement Learning with Perceptual Aliasing: The 
#  Perceptual Distinctions Approach, 
#' _Proceedings of the AAAI Conference on Artificial Intelligence,_ 
#' 10, AAAI-92.
#' 
#' Michael L. Littman (2009), A tutorial on partially observable Markov decision processes, 
#' _Journal of Mathematical Psychology,_ Volume 53, Issue 3, June 2009, Pages 119-125.
#' \doi{10.1016/j.jmp.2009.01.005}
#' 
#' @examples
#' dir(system.file("examples/", package = "pomdp"))
#' 
#' model <- read_POMDP(system.file("examples/light_maze.POMDP", 
#'   package = "pomdp"))
#' model
NULL
