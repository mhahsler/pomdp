# Read and write a POMDP Model to a File in POMDP Format

Reads and writes a POMDP file suitable for the `pomdp-solve` program.

## Usage

``` r
write_POMDP(x, file, digits = 7, labels = FALSE)

read_POMDP(file, parse = TRUE, normalize = FALSE, verbose = FALSE)
```

## Arguments

- x:

  an object of class
  [POMDP](http://michael.hahsler.net/pomdp/reference/POMDP.md).

- file:

  a file name. `read_POMDP()` also accepts
  [connections](https://rdrr.io/r/base/connections.html) including URLs.

- digits:

  precision for writing numbers (digits after the decimal point).

- labels:

  logical; write original labels or use index numbers? Labels are
  restricted to `[a-zA-Z0-9_-]` and the first character has to be a
  letter.

- parse:

  logical; try to parse the model matrices. Solvers still work with
  unparsed matrices, but helpers for simulation are not available.

- normalize:

  logical; should the description be normalized for faster access (see
  [`normalize_POMDP()`](http://michael.hahsler.net/pomdp/reference/accessors.md))?

- verbose:

  logical; report parsed lines. This is useful for debugging a POMDP
  file.

## Value

`read_POMDP()` returns a
[POMDP](http://michael.hahsler.net/pomdp/reference/POMDP.md) object.

## Details

[POMDP](http://michael.hahsler.net/pomdp/reference/POMDP.md) objects
read from a POMDP file have an extra element called `problem` which
contains the original POMDP specification. **The original specification
is directly used by external solvers.** In addition, the file is parsed
using an experimental POMDP file parser. The parsed information can be
used with auxiliary functions in this package that use fields like the
transition matrix, the observation matrix and the reward structure.

The range of useful rewards is restricted by the solver. Here the values
are restricted to the range `[-1e10, 1e10]`. Unavailable actions have a
reward of `-Inf` which is translated to -2 times the maximum absolute
reward value used in the model.

**Notes:** The parser for POMDP files is experimental. Please report
problems here: <https://github.com/mhahsler/pomdp/issues>.

## References

POMDP solver website: https://www.pomdp.org

## See also

Other POMDP:
[`MDP2POMDP`](http://michael.hahsler.net/pomdp/reference/MDP2POMDP.md),
[`POMDP()`](http://michael.hahsler.net/pomdp/reference/POMDP.md),
[`accessors`](http://michael.hahsler.net/pomdp/reference/accessors.md),
[`actions()`](http://michael.hahsler.net/pomdp/reference/actions.md),
[`add_policy()`](http://michael.hahsler.net/pomdp/reference/add_policy.md),
[`plot_belief_space()`](http://michael.hahsler.net/pomdp/reference/plot_belief_space.md),
[`projection()`](http://michael.hahsler.net/pomdp/reference/projection.md),
[`reachable_and_absorbing`](http://michael.hahsler.net/pomdp/reference/reachable_and_absorbing.md),
[`regret()`](http://michael.hahsler.net/pomdp/reference/regret.md),
[`sample_belief_space()`](http://michael.hahsler.net/pomdp/reference/sample_belief_space.md),
[`simulate_POMDP()`](http://michael.hahsler.net/pomdp/reference/simulate_POMDP.md),
[`solve_POMDP()`](http://michael.hahsler.net/pomdp/reference/solve_POMDP.md),
[`solve_SARSOP()`](http://michael.hahsler.net/pomdp/reference/solve_SARSOP.md),
[`transition_graph()`](http://michael.hahsler.net/pomdp/reference/transition_graph.md),
[`update_belief()`](http://michael.hahsler.net/pomdp/reference/update_belief.md),
[`value_function()`](http://michael.hahsler.net/pomdp/reference/value_function.md)

## Author

Hossein Kamalzadeh, Michael Hahsler

## Examples

``` r
data(Tiger)

## show the POMDP file that would be written.
write_POMDP(Tiger, file = stdout())
#> # POMDP File: Tiger Problem
#> # Produced with R package pomdp (created: Fri Sep 25 00:26:55 2026)
#> 
#> discount: 0.75
#> values: reward
#> states: 2
#> actions: 3
#> observations: 2
#>  
#> start: uniform
#>  
#> T: 0
#> identity
#> 
#> T: 1
#> uniform
#> 
#> T: 2
#> uniform
#> 
#> O: 0
#> 0.8500000 0.1500000
#> 0.1500000 0.8500000
#> 
#> O: 1
#> uniform
#> 
#> O: 2
#> uniform
#> 
#> R: 0 : * : * : * -1.0000000
#> R: 1 : 0 : * : * -100.0000000
#> R: 1 : 1 : * : * 10.0000000
#> R: 2 : 0 : * : * 10.0000000
#> R: 2 : 1 : * : * -100.0000000
```
