# R package pomdp: Partially Observable Markov Decision Processes

[![CRAN version](https://www.r-pkg.org/badges/version/pomdp)](https://cran.r-project.org/package=pomdp)
[![Rdoc](https://www.rdocumentation.org/badges/version/pomdp)](https://www.rdocumentation.org/packages/pomdp)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/pomdp)](https://cran.r-project.org/package=pomdp)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/pomdp)](https://cran.r-project.org/package=pomdp)

Provides the infrastructure to define and analyze the solutions of Partially Observable Markov Decision Processes (POMDP) models. The package includes [pomdp-solve](http://www.pomdp.org/code/index.html) (Cassandra, 2015) to solve POMDPs using
a variety of algorithms.

The package provides the following algorithms:

* Exact value iteration
  - Enumeration algorithm (Sondik 1971).
  - Two pass algorihtm (Sondik 1971).
  - Witness algorithm (Littman, Cassandra, Kaelbling, 1996).
  - Incremental pruning algorithm (Zhang and Liu, 1996, Cassandra et al 1997).

* Approximate value iteration
  - Finite grid algorithm, a variation of point-based value iteration to solve larger POMDPs (PBVI; see Pineau 2003) without dynamic belief set expansion.

## Installation

__Stable CRAN version:__ install from within R with
```R
install.packages("pomdp")
```
__Current development version:__ install from GitHub (needs devtools).
```R 
library("devtools")
install_github("mhahsler/pomdp")
```

## Usage

Solving the simple infinite-horizon Tiger problem.
```R
R> library("pomdp")
R> data("Tiger")
R> Tiger
```

```
Unsolved POMDP model: Tiger Problem 
 	horizon: Inf 
```

```R
> sol <- solve_POMDP(model = Tiger)
> sol
```

```
Solved POMDP model: Tiger Problem 
 	solution method: grid 
 	horizon: Inf 
  	converged: TRUE 
 	total expected reward (for start probabilities): 1.933439 
```

```R
> policy(sol)
```

```R
[[1]]
  tiger-left tiger-right     action tiger-left tiger-right
1 -98.549921   11.450079  open-left          3           3
2 -10.854299    6.516937     listen          3           1
3   1.933439    1.933439     listen          4           2
4   6.516937  -10.854299     listen          5           3
5  11.450079  -98.549921 open-right          3           3
```

## References

* Cassandra, A. (2015). pomdp-solve: POMDP Solver Software, http://www.pomdp.org.
* Sondik, E. (1971). The Optimal Control of Partially Observable Markov Processes. Ph.D. Dissertation, Stanford University.
* Cassandra, A., Littman M.L., Zhang L. (1997). Incremental Pruning: A Simple, Fast, Exact Algorithm for Partially Observable Markov Decision Processes. UAI'97: Proceedings of the Thirteenth conference on Uncertainty in artificial intelligence, August 1997, pp. 54-61.
* Monahan, G. E. (1982). A survey of partially observable Markov decision processes: Theory, models, and algorithms. Management Science 28(1):1-16.
* Littman, M. L.; Cassandra, A. R.; and Kaelbling, L. P. (1996). Efficient dynamic-programming updates in partially observable Markov decision processes. Technical Report CS-95-19, Brown University, Providence, RI.
* Zhang, N. L., and Liu, W. (1996). Planning in stochastic domains: Problem characteristics and approximation. Technical Report HKUST-CS96-31, Department of Computer Science, Hong Kong University of Science and Technology.
* Pineau J., Geoffrey J Gordon G.J., Thrun S.B. (2003). Point-based value iteration: an anytime algorithm for POMDPs. IJCAI'03: Proceedings of the 18th international joint conference on Artificial Intelligence. Pages 1025-1030.
