# Construct a `pmx_theme`

Public factory for the shared `pmx_theme` base class. Builds a (possibly
partial) named list of `pmx_element` objects and tags it with a class
vector. Pair with
[`+.pmx_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plus-.pmx_theme.md)
to compose a partial override onto a complete plot theme:

    base  <- plot_vpc_theme()
    patch <- pmx_theme(list(obs_point = pmx_point(color = "red")))
    base + patch

The five per-plot theme factories
([`plot_dvtime_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvtime_theme.md),
[`plot_gof_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_gof_theme.md),
[`plot_dvconc_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvconc_theme.md),
[`plot_doseprop_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_doseprop_theme.md),
[`plot_vpc_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_theme.md))
call `pmx_theme()` internally with their own subclass tag, so user code
rarely needs the `subclass` argument.

## Usage

``` r
pmx_theme(elements = list(), subclass = NULL)
```

## Arguments

- elements:

  Named list of `pmx_element` objects (e.g. as returned by
  [`pmx_point()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_point.md),
  [`pmx_line()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_line.md),
  [`pmx_ribbon()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_ribbon.md)).
  May be empty. `NULL` entries are dropped.

- subclass:

  Character scalar tag prepended to the class vector (e.g.
  `"plot_vpc_theme"`). When `NULL` (default), the result carries only
  `"pmx_theme"` — the right shape for a generic partial.

## Value

An object of class `c(subclass, "pmx_theme")` (or just `"pmx_theme"`
when `subclass` is `NULL`).

## See also

Other pmx theme class:
[`is_pmx_element()`](https://ryancrass.github.io/pmxhelpr/reference/is_pmx_element.md),
[`is_pmx_theme()`](https://ryancrass.github.io/pmxhelpr/reference/is_pmx_theme.md)

## Examples

``` r
# Generic partial — no subclass; useful as a `+.pmx_theme` right-hand side
pmx_theme(list(obs_point = pmx_point(color = "red")))
#> <pmx_theme>
#>   obs_point <pmx_point>: color = red

# Compose onto a complete theme
plot_vpc_theme() + pmx_theme(list(obs_point = pmx_point(color = "red")))
#> <plot_vpc_theme>
#>   obs_point       <pmx_point>: shape = 1, size = 1, alpha = 0.7, color = red
#>   obs_median_line <pmx_line>: linewidth = 1, linetype = solid, color = #FF0000
#>   obs_pi_line     <pmx_line>: linewidth = 0.5, linetype = dashed, color = #0000FF
#>   sim_pi_line     <pmx_line>: linewidth = 1, linetype = dotted, color = #000000
#>   sim_pi_ci       <pmx_ribbon>: fill = #0000FF, alpha = 0.15
#>   sim_pi_area     <pmx_ribbon>: fill = #0000FF, alpha = 0.15
#>   sim_median_line <pmx_line>: linewidth = 1, linetype = dashed, color = #000000
#>   sim_median_ci   <pmx_ribbon>: fill = #FF0000, alpha = 0.3
#>   loq_line        <pmx_line>: linewidth = 0.5, linetype = dashed, color = #990000
```
