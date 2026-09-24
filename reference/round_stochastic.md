# Round a stochastic vector or a row-stochastic matrix

Rounds a vector such that the sum of 1 is preserved. Rounds a matrix
such that each row sum up to 1. One entry is adjusted after rounding
such that the rounding error is the smallest.

## Usage

``` r
round_stochastic(x, digits = 7)
```

## Arguments

- x:

  a stochastic vector or a row-stochastic matrix.

- digits:

  number of digits for rounding.

## Value

The rounded vector or matrix.

## See also

[round](https://rdrr.io/r/base/Round.html)

## Examples

``` r
# regular rounding would not sum up to 1 
x <- c(0.333, 0.334, 0.333)

round_stochastic(x)
#> [1] 0.333 0.334 0.333
round_stochastic(x, digits = 2)
#> [1] 0.34 0.33 0.33
round_stochastic(x, digits = 1)
#> [1] 0.3 0.3 0.4
round_stochastic(x, digits = 0)
#> [1] 0 1 0


# round a stochastic matrix
m <- matrix(runif(15), ncol = 3)
m <- sweep(m, 1, rowSums(m), "/")

m
#>           [,1]        [,2]      [,3]
#> [1,] 0.2831399 0.420851179 0.2960089
#> [2,] 0.4273351 0.416486882 0.1561780
#> [3,] 0.5765247 0.004821691 0.4186536
#> [4,] 0.3831227 0.030681111 0.5861962
#> [5,] 0.4217106 0.220229395 0.3580600
round_stochastic(m, digits = 2)
#>      [,1] [,2] [,3]
#> [1,] 0.28 0.42 0.30
#> [2,] 0.42 0.42 0.16
#> [3,] 0.58 0.00 0.42
#> [4,] 0.38 0.03 0.59
#> [5,] 0.42 0.22 0.36
round_stochastic(m, digits = 1)
#>      [,1] [,2] [,3]
#> [1,]  0.3  0.4  0.3
#> [2,]  0.4  0.4  0.2
#> [3,]  0.6  0.0  0.4
#> [4,]  0.4  0.0  0.6
#> [5,]  0.4  0.2  0.4
round_stochastic(m, digits = 0)
#>      [,1] [,2] [,3]
#> [1,]    1    0    0
#> [2,]    1    0    0
#> [3,]    1    0    0
#> [4,]    0    0    1
#> [5,]    0    0    1
```
