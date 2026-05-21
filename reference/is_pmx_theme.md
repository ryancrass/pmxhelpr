# Test whether an object is a pmx plot theme

Predicate that returns `TRUE` if `x` carries the shared `"pmx_theme"`
class – i.e., it was constructed by
[`pmx_theme()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_theme.md)
or one of
[`plot_dvtime_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvtime_theme.md),
[`plot_gof_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_gof_theme.md),
[`plot_dvconc_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvconc_theme.md),
[`plot_doseprop_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_doseprop_theme.md),
or
[`plot_vpc_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_theme.md).
For a specific-type check, use `inherits(x, "plot_vpc_theme")` (or
whichever subclass) directly.

## Usage

``` r
is_pmx_theme(x, strict = FALSE)
```

## Arguments

- x:

  Object to test.

- strict:

  Logical. When `TRUE`, additionally checks that every named entry of
  the theme is a `pmx_element`. Default `FALSE` (class-tag check only,
  cheap).

## Value

Logical scalar.

## See also

Other pmx theme class:
[`is_pmx_element()`](https://ryancrass.github.io/pmxhelpr/reference/is_pmx_element.md),
[`pmx_theme()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_theme.md)

## Examples

``` r
is_pmx_theme(plot_dvtime_theme())                     # TRUE
#> [1] TRUE
is_pmx_theme(plot_vpc_theme())                        # TRUE
#> [1] TRUE
is_pmx_theme(pmx_point())                             # FALSE
#> [1] FALSE
is_pmx_theme(list(obs_point = 1))                     # FALSE
#> [1] FALSE
inherits(plot_vpc_theme(), "plot_vpc_theme")          # TRUE -- specific-type check
#> [1] TRUE
```
