# VPC plot theme

Constructor and factory for `plot_vpc_cont` plot aesthetics. Call with
no arguments to view defaults. Pass element overrides to customize. Each
theme key maps 1:1 with a
[`plot_vpc_shown()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_shown.md)
visibility toggle.

## Usage

``` r
plot_vpc_theme(
  obs_point = NULL,
  obs_median_line = NULL,
  obs_pi_line = NULL,
  sim_pi_line = NULL,
  sim_pi_ci = NULL,
  sim_pi_area = NULL,
  sim_median_line = NULL,
  sim_median_ci = NULL,
  loq_line = NULL
)
```

## Arguments

- obs_point:

  Observed data point aesthetics. See
  [`pmx_point()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_point.md).

- obs_median_line:

  Observed median line aesthetics. See
  [`pmx_line()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_line.md).

- obs_pi_line:

  Observed quantile line aesthetics. See
  [`pmx_line()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_line.md).

- sim_pi_line:

  Simulated prediction interval line aesthetics. See
  [`pmx_line()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_line.md).

- sim_pi_ci:

  Simulated prediction interval CI ribbon aesthetics. See
  [`pmx_ribbon()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_ribbon.md).

- sim_pi_area:

  Simulated prediction interval area ribbon aesthetics. See
  [`pmx_ribbon()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_ribbon.md).

- sim_median_line:

  Simulated median line aesthetics. See
  [`pmx_line()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_line.md).

- sim_median_ci:

  Simulated median CI ribbon aesthetics. See
  [`pmx_ribbon()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_ribbon.md).

- loq_line:

  LOQ reference line aesthetics. See
  [`pmx_line()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_line.md).

## Value

A named list of theme elements

## See also

Other vpc:
[`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md),
[`is_pmx_vpc_plot()`](https://ryancrass.github.io/pmxhelpr/reference/is_pmx_vpc_plot.md),
[`is_vpc_stats()`](https://ryancrass.github.io/pmxhelpr/reference/is_vpc_stats.md),
[`plot_build_vpc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_vpc.md),
[`plot_vpc_cens()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cens.md),
[`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md),
[`plot_vpc_legend()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_legend.md),
[`plot_vpc_shown()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_shown.md)

## Examples

``` r
plot_vpc_theme()
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
plot_vpc_theme(obs_point = pmx_point(color = "#000000"))
#> <plot_vpc_theme>
#>   obs_point       <pmx_point>: shape = 1, size = 1, alpha = 0.7, color = #000000
#>   obs_median_line <pmx_line>: linewidth = 1, linetype = solid, color = #FF0000
#>   obs_pi_line     <pmx_line>: linewidth = 0.5, linetype = dashed, color = #0000FF
#>   sim_pi_line     <pmx_line>: linewidth = 1, linetype = dotted, color = #000000
#>   sim_pi_ci       <pmx_ribbon>: fill = #0000FF, alpha = 0.15
#>   sim_pi_area     <pmx_ribbon>: fill = #0000FF, alpha = 0.15
#>   sim_median_line <pmx_line>: linewidth = 1, linetype = dashed, color = #000000
#>   sim_median_ci   <pmx_ribbon>: fill = #FF0000, alpha = 0.3
#>   loq_line        <pmx_line>: linewidth = 0.5, linetype = dashed, color = #990000
```
