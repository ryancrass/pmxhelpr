# Test whether an object is a pmx theme element

Predicate that returns `TRUE` if `x` carries the shared `"pmx_element"`
class – i.e., it was constructed by one of
[`pmx_point()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_point.md),
[`pmx_line()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_line.md),
[`pmx_ribbon()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_ribbon.md),
[`pmx_errorbar()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_errorbar.md),
[`pmx_trend()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_trend.md),
[`pmx_style()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_style.md),
or
[`pmx_color()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_color.md).
For a specific-type check, use `inherits(x, "pmx_point")` (or whichever
type) directly.

## Usage

``` r
is_pmx_element(x)
```

## Arguments

- x:

  Object to test.

## Value

Logical scalar.

## See also

Other pmx theme class:
[`is_pmx_theme()`](https://ryancrass.github.io/pmxhelpr/reference/is_pmx_theme.md),
[`pmx_theme()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_theme.md)

## Examples

``` r
is_pmx_element(pmx_point(shape = 1))     # TRUE
#> [1] TRUE
is_pmx_element(pmx_line(linewidth = 1))  # TRUE
#> [1] TRUE
is_pmx_element(list(shape = 1))          # FALSE
#> [1] FALSE
inherits(pmx_point(), "pmx_point")       # TRUE -- specific-type check
#> [1] TRUE
```
