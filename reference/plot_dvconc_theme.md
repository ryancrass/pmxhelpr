# Response versus concentration plot theme

Constructor and factory for `plot_dvconc` plot aesthetics. Call with no
arguments to view defaults. Pass element overrides to customize.

## Usage

``` r
plot_dvconc_theme(
  obs_point = NULL,
  ref_line = NULL,
  loess = NULL,
  linear = NULL,
  obs = NULL
)
```

## Arguments

- obs_point:

  Observed data point aesthetics. See
  [`pmx_point()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_point.md).

- ref_line:

  Reference line aesthetics. See
  [`pmx_line()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_line.md).

- loess:

  LOESS trend line aesthetics. See
  [`pmx_trend()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_trend.md).

- linear:

  Linear trend line aesthetics. See
  [`pmx_trend()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_trend.md).

- obs:

  Shortcut: apply shared aesthetics to `obs_point`. See
  [`pmx_style()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_style.md).

## Value

A named list of theme elements

## See also

Other exploratory analysis:
[`plot_dvconc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvconc.md),
[`plot_dvtime()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvtime.md),
[`plot_dvtime_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvtime_theme.md)

## Examples

``` r
plot_dvconc_theme()
#> <plot_dvconc_theme>
#>   obs_point <pmx_point>: shape = 1, size = 1.25, alpha = 0.5
#>   ref_line  <pmx_line>: linewidth = 0.5, linetype = 2, alpha = 1
#>   loess     <pmx_trend>: linewidth = 1, linetype = 1, color = black, se_color = lightgrey, se_alpha = 0.4
#>   linear    <pmx_trend>: linewidth = 1, linetype = 2, color = black, se_color = lightgrey, se_alpha = 0.4
plot_dvconc_theme(loess = pmx_trend(color = "red"))
#> <plot_dvconc_theme>
#>   obs_point <pmx_point>: shape = 1, size = 1.25, alpha = 0.5
#>   ref_line  <pmx_line>: linewidth = 0.5, linetype = 2, alpha = 1
#>   loess     <pmx_trend>: linewidth = 1, linetype = 1, color = red, se_color = lightgrey, se_alpha = 0.4
#>   linear    <pmx_trend>: linewidth = 1, linetype = 2, color = black, se_color = lightgrey, se_alpha = 0.4
plot_dvconc_theme(obs = pmx_style(alpha = 0.3))
#> <plot_dvconc_theme>
#>   obs_point <pmx_point>: shape = 1, size = 1.25, alpha = 0.3
#>   ref_line  <pmx_line>: linewidth = 0.5, linetype = 2, alpha = 1
#>   loess     <pmx_trend>: linewidth = 1, linetype = 1, color = black, se_color = lightgrey, se_alpha = 0.4
#>   linear    <pmx_trend>: linewidth = 1, linetype = 2, color = black, se_color = lightgrey, se_alpha = 0.4
```
