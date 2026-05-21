# Shared style for point and line layers

Convenience constructor for setting aesthetics that apply to both point
and line elements of a role. Pass to role-level theme arguments (e.g.,
`obs`, `cent`) to set shared properties without specifying each element
individually.

## Usage

``` r
pmx_style(color = NULL, alpha = NULL)
```

## Arguments

- color:

  Color applied to both point and line elements.

- alpha:

  Alpha applied to both point and line elements.

## Value

A `pmx_style` element object

## See also

Other element constructors:
[`pmx_color()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_color.md),
[`pmx_errorbar()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_errorbar.md),
[`pmx_line()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_line.md),
[`pmx_point()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_point.md),
[`pmx_ribbon()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_ribbon.md),
[`pmx_trend()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_trend.md)

## Examples

``` r
plot_dvtime_theme(obs = pmx_style(alpha = 0.3))
#> <plot_dvtime_theme>
#>   obs_point     <pmx_point>: shape = 1, size = 0.75, alpha = 0.3
#>   obs_line      <pmx_line>: linewidth = 0.5, linetype = 1, alpha = 0.3
#>   cent_point    <pmx_point>: shape = 16, size = 1.25, alpha = 0
#>   cent_line     <pmx_line>: linewidth = 0.75, linetype = 1, alpha = 1
#>   cent_errorbar <pmx_errorbar>: linewidth = 0.75, linetype = 1, alpha = 1, width = NULL
#>   ref_line      <pmx_line>: linewidth = 0.5, linetype = 2, alpha = 1
#>   loq_line      <pmx_line>: linewidth = 0.5, linetype = 2, alpha = 1
```
