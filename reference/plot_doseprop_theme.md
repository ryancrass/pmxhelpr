# Dose-proportionality plot theme

Constructor and factory for `plot_doseprop` plot aesthetics. Call with
no arguments to view defaults. Pass element overrides to customize.

## Usage

``` r
plot_doseprop_theme(obs_point = NULL, linear = NULL, obs = NULL)
```

## Arguments

- obs_point:

  Observed data point aesthetics. See
  [`pmx_point()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_point.md).

- linear:

  Linear regression line + SE ribbon aesthetics. See
  [`pmx_trend()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_trend.md).

- obs:

  Shortcut: apply shared aesthetics to `obs_point`. See
  [`pmx_style()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_style.md).

## Value

A named list of theme elements

## See also

Other dose proportionality:
[`df_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/df_doseprop.md),
[`is_doseprop_stats()`](https://ryancrass.github.io/pmxhelpr/reference/is_doseprop_stats.md),
[`plot_build_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_doseprop.md),
[`plot_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_doseprop.md)

## Examples

``` r
plot_doseprop_theme()
#> <plot_doseprop_theme>
#>   obs_point <pmx_point>: shape = 1, size = 2, alpha = 0.7
#>   linear    <pmx_trend>: linewidth = 1, linetype = 1, color = black, se_color = lightgrey, se_alpha = 0.4
plot_doseprop_theme(linear = pmx_trend(color = "red"))
#> <plot_doseprop_theme>
#>   obs_point <pmx_point>: shape = 1, size = 2, alpha = 0.7
#>   linear    <pmx_trend>: linewidth = 1, linetype = 1, color = red, se_color = lightgrey, se_alpha = 0.4
plot_doseprop_theme(obs = pmx_style(alpha = 0.3))
#> <plot_doseprop_theme>
#>   obs_point <pmx_point>: shape = 1, size = 2, alpha = 0.3
#>   linear    <pmx_trend>: linewidth = 1, linetype = 1, color = black, se_color = lightgrey, se_alpha = 0.4
```
