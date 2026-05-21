# Population overlay GOF plot theme

Constructor and factory for `plot_gof` plot aesthetics. Call with no
arguments to view defaults. Pass element overrides to customize. Use
role-level shortcuts `obs` and `cent` with
[`pmx_style()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_style.md)
to set shared aesthetics for both point and line elements at once.
Override overlay colors with `cent_color = pmx_color()`.

## Usage

``` r
plot_gof_theme(
  obs_point = NULL,
  obs_line = NULL,
  cent_point = NULL,
  cent_line = NULL,
  cent_errorbar = NULL,
  ref_line = NULL,
  loq_line = NULL,
  obs = NULL,
  cent = NULL,
  cent_color = NULL
)
```

## Arguments

- obs_point:

  Observed data point aesthetics. See
  [`pmx_point()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_point.md).

- obs_line:

  Observed data line aesthetics. See
  [`pmx_line()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_line.md).

- cent_point:

  Shared central tendency point aesthetics for DV, PRED, and IPRED. See
  [`pmx_point()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_point.md).

- cent_line:

  Shared central tendency line aesthetics for DV, PRED, and IPRED. See
  [`pmx_line()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_line.md).

- cent_errorbar:

  Central tendency error bar aesthetics. See
  [`pmx_errorbar()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_errorbar.md).

- ref_line:

  Reference line aesthetics. See
  [`pmx_line()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_line.md).

- loq_line:

  LOQ reference line aesthetics. See
  [`pmx_line()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_line.md).

- obs:

  Shortcut: apply shared aesthetics to both `obs_point` and `obs_line`.
  See
  [`pmx_style()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_style.md).

- cent:

  Shortcut: apply shared aesthetics to both `cent_point` and
  `cent_line`. See
  [`pmx_style()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_style.md).

- cent_color:

  Overlay color mapping for DV, PRED, and IPRED. See
  [`pmx_color()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_color.md).

## Value

A named list of theme elements

## See also

Other goodness-of-fit:
[`plot_gof()`](https://ryancrass.github.io/pmxhelpr/reference/plot_gof.md),
[`plot_gof_shown()`](https://ryancrass.github.io/pmxhelpr/reference/plot_gof_shown.md)

## Examples

``` r
plot_gof_theme()
#> <plot_gof_theme>
#>   obs_point     <pmx_point>: shape = 1, size = 0.75, alpha = 0.5, color = darkgrey
#>   obs_line      <pmx_line>: linewidth = 0.5, linetype = 1, alpha = 0.75, color = darkgrey
#>   cent_point    <pmx_point>: shape = 16, size = 1.25, alpha = 0
#>   cent_line     <pmx_line>: linewidth = 0.75, linetype = 1, alpha = 1
#>   cent_errorbar <pmx_errorbar>: linewidth = 0.75, linetype = 1, alpha = 1, width = NULL
#>   ref_line      <pmx_line>: linewidth = 0.5, linetype = 2, alpha = 1
#>   loq_line      <pmx_line>: linewidth = 0.5, linetype = 2, alpha = 1
#>   cent_color    <pmx_color>: dv = blue, pred = red, ipred = green
plot_gof_theme(cent_color = pmx_color(pred = "purple"))
#> <plot_gof_theme>
#>   obs_point     <pmx_point>: shape = 1, size = 0.75, alpha = 0.5, color = darkgrey
#>   obs_line      <pmx_line>: linewidth = 0.5, linetype = 1, alpha = 0.75, color = darkgrey
#>   cent_point    <pmx_point>: shape = 16, size = 1.25, alpha = 0
#>   cent_line     <pmx_line>: linewidth = 0.75, linetype = 1, alpha = 1
#>   cent_errorbar <pmx_errorbar>: linewidth = 0.75, linetype = 1, alpha = 1, width = NULL
#>   ref_line      <pmx_line>: linewidth = 0.5, linetype = 2, alpha = 1
#>   loq_line      <pmx_line>: linewidth = 0.5, linetype = 2, alpha = 1
#>   cent_color    <pmx_color>: dv = blue, pred = purple, ipred = green
plot_gof_theme(cent = pmx_style(alpha = 0.8))
#> <plot_gof_theme>
#>   obs_point     <pmx_point>: shape = 1, size = 0.75, alpha = 0.5, color = darkgrey
#>   obs_line      <pmx_line>: linewidth = 0.5, linetype = 1, alpha = 0.75, color = darkgrey
#>   cent_point    <pmx_point>: shape = 16, size = 1.25, alpha = 0.8
#>   cent_line     <pmx_line>: linewidth = 0.75, linetype = 1, alpha = 0.8
#>   cent_errorbar <pmx_errorbar>: linewidth = 0.75, linetype = 1, alpha = 0.8, width = NULL
#>   ref_line      <pmx_line>: linewidth = 0.5, linetype = 2, alpha = 1
#>   loq_line      <pmx_line>: linewidth = 0.5, linetype = 2, alpha = 1
#>   cent_color    <pmx_color>: dv = blue, pred = red, ipred = green
```
