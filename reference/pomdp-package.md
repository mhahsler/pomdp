# pomdp: Infrastructure for Partially Observable Markov Decision Processes (POMDP)

Provides the infrastructure to define and analyze solutions to Partially
Observable Markov Decision Process (POMDP) models. Interfaces to various
exact and approximate solution algorithms are available, including value
iteration, point-based value iteration, and SARSOP. Hahsler and
Cassandra
[doi:10.32614/RJ-2024-021](https://doi.org/10.32614/RJ-2024-021) .

## Key functions

- Problem specification:
  [POMDP](http://michael.hahsler.net/pomdp/reference/POMDP.md),
  [MDP](http://michael.hahsler.net/pomdp/reference/MDP.md)

- Solvers:
  [`solve_POMDP()`](http://michael.hahsler.net/pomdp/reference/solve_POMDP.md),
  [`solve_MDP()`](http://michael.hahsler.net/pomdp/reference/solve_MDP.md),
  [`solve_SARSOP()`](http://michael.hahsler.net/pomdp/reference/solve_SARSOP.md)

## See also

Useful links:

- <https://github.com/mhahsler/pomdp>

- Report bugs at <https://github.com/mhahsler/pomdp/issues>

## Author

**Maintainer**: Michael Hahsler <mhahsler@lyle.smu.edu>
([ORCID](https://orcid.org/0000-0003-2716-1405)) \[copyright holder\]

Authors:

- Michael Hahsler <mhahsler@lyle.smu.edu>
  ([ORCID](https://orcid.org/0000-0003-2716-1405)) \[copyright holder\]

Other contributors:

- Hossein Kamalzadeh \[contributor\]
