# Concentration-time plot theme

Constructor and factory for `plot_dvtime` plot aesthetics. Call with no
arguments to view defaults. Pass element overrides to customize. Use
role-level shortcuts `obs` and `cent` with
[`pmx_style()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_style.md)
to set shared aesthetics (e.g., color, alpha) for both point and line
elements at once.

## Usage

``` r
plot_dvtime_theme(
  obs_point = NULL,
  obs_line = NULL,
  cent_point = NULL,
  cent_line = NULL,
  cent_errorbar = NULL,
  ref_line = NULL,
  loq_line = NULL,
  obs = NULL,
  cent = NULL
)
```

## Arguments

- obs_point:

  Observed data point aesthetics. See
  [`pmx_point()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_point.md).

- obs_line:

  Observed data line aesthetics (spaghetti). See
  [`pmx_line()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_line.md).

- cent_point:

  Central tendency point aesthetics. See
  [`pmx_point()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_point.md).

- cent_line:

  Central tendency line aesthetics. See
  [`pmx_line()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_line.md).

- cent_errorbar:

  Central tendency error bar aesthetics. See
  [`pmx_errorbar()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_errorbar.md).

- ref_line:

  Reference line aesthetics (e.g., change-from-baseline). See
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

## Value

A named list of theme elements

## See also

Other exploratory analysis:
[`plot_dvconc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvconc.md),
[`plot_dvconc_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvconc_theme.md),
[`plot_dvtime()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvtime.md)

## Examples

``` r
plot_dvtime_theme()
#> <plot_dvtime_theme>
#>   obs_point     <pmx_point>: shape = 1, size = 0.75, alpha = 0.5
#>   obs_line      <pmx_line>: linewidth = 0.5, linetype = 1, alpha = 0.5
#>   cent_point    <pmx_point>: shape = 16, size = 1.25, alpha = 0
#>   cent_line     <pmx_line>: linewidth = 0.75, linetype = 1, alpha = 1
#>   cent_errorbar <pmx_errorbar>: linewidth = 0.75, linetype = 1, alpha = 1, width = NULL
#>   ref_line      <pmx_line>: linewidth = 0.5, linetype = 2, alpha = 1
#>   loq_line      <pmx_line>: linewidth = 0.5, linetype = 2, alpha = 1
plot_dvtime_theme(obs_point = pmx_point(size = 2), ref_line = pmx_line(linetype = 3))
#> <plot_dvtime_theme>
#>   obs_point     <pmx_point>: shape = 1, size = 2, alpha = 0.5
#>   obs_line      <pmx_line>: linewidth = 0.5, linetype = 1, alpha = 0.5
#>   cent_point    <pmx_point>: shape = 16, size = 1.25, alpha = 0
#>   cent_line     <pmx_line>: linewidth = 0.75, linetype = 1, alpha = 1
#>   cent_errorbar <pmx_errorbar>: linewidth = 0.75, linetype = 1, alpha = 1, width = NULL
#>   ref_line      <pmx_line>: linewidth = 0.5, linetype = 3, alpha = 1
#>   loq_line      <pmx_line>: linewidth = 0.5, linetype = 2, alpha = 1
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
