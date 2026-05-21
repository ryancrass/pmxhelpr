# Combine two pmx plot themes

Left-to-right merge of two `pmx_theme` objects. Each named entry in `b`
overrides the matching entry in `a`; entries unique to `a` are
unchanged. The class vector of `a` is preserved on the result, so a
partial override applied to a typed theme keeps its subclass (e.g.
`plot_vpc_theme() + pmx_theme(list(obs_point = ...))` is still a
`plot_vpc_theme`).

## Usage

``` r
# S3 method for class 'pmx_theme'
a + b
```

## Arguments

- a:

  A `pmx_theme` object (left side, the "base").

- b:

  A `pmx_theme` object (right side, the "override"), or `NULL`. When
  `NULL`, `a` is returned unchanged.

## Value

A `pmx_theme` with the same class vector as `a`.

## Examples

``` r
base  <- plot_vpc_theme()
patch <- pmx_theme(list(obs_point = pmx_point(color = "red")))
out   <- base + patch
inherits(out, "plot_vpc_theme")   # TRUE -- class preserved
#> [1] TRUE
out$obs_point$color               # "red"
#> [1] "red"

base + NULL                       # identical to `base`
#> <plot_vpc_theme>
#>   obs_point       <pmx_point>: shape = 1, size = 1, alpha = 0.7, color = #0000FF
#>   obs_median_line <pmx_line>: linewidth = 1, linetype = solid, color = #FF0000
#>   obs_pi_line     <pmx_line>: linewidth = 0.5, linetype = dashed, color = #0000FF
#>   sim_pi_line     <pmx_line>: linewidth = 1, linetype = dotted, color = #000000
#>   sim_pi_ci       <pmx_ribbon>: fill = #0000FF, alpha = 0.15
#>   sim_pi_area     <pmx_ribbon>: fill = #0000FF, alpha = 0.15
#>   sim_median_line <pmx_line>: linewidth = 1, linetype = dashed, color = #000000
#>   sim_median_ci   <pmx_ribbon>: fill = #FF0000, alpha = 0.3
#>   loq_line        <pmx_line>: linewidth = 0.5, linetype = dashed, color = #990000
```
