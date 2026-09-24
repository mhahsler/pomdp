# Default Colors for Visualization in Package pomdp

Default discrete and continuous colors used in pomdp for states (nodes),
beliefs and values.

## Usage

``` r
colors_discrete(n, col = NULL)

colors_continuous(val, col = NULL)
```

## Arguments

- n:

  number of states.

- col:

  custom color palette. `colors_discrete()` uses the first n colors.
  `colors_continuous()` uses these colors to calculate a palette (see
  [`grDevices::colorRamp()`](https://rdrr.io/r/grDevices/colorRamp.html))

- val:

  a vector with values to be translated to colors.

## Value

`colors_discrete()` returns a color palette and `colors_continuous()`
returns the colors associated with the supplied values.

## Examples

``` r
colors_discrete(5)
#> [1] "#E41A1C" "#377EB8" "#4DAF4A" "#984EA3" "#FF7F00"

colors_continuous(runif(10))
#>  [1] "#487CB3" "#E22020" "#427DB5" "#AC606E" "#C64D4E" "#9B687F" "#E41A1C"
#>  [8] "#B05E69" "#377EB8" "#C74C4C"
```
