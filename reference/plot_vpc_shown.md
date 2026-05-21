# VPC layer visibility settings

Constructor and factory for controlling which VPC layers are displayed.
Call with no arguments to view defaults. Pass overrides to customize.
Each element maps 1:1 with a
[`plot_vpc_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_theme.md)
aesthetic key.

## Usage

``` r
plot_vpc_shown(
  obs_point = NULL,
  obs_pi_line = NULL,
  obs_median_line = NULL,
  sim_pi_line = NULL,
  sim_pi_ci = NULL,
  sim_pi_area = NULL,
  sim_median_line = NULL,
  sim_median_ci = NULL
)
```

## Arguments

- obs_point:

  Show observed data points. Default is `TRUE`.

- obs_pi_line:

  Show observed quantile lines. Default is `TRUE`.

- obs_median_line:

  Show observed median line. Default is `TRUE`.

- sim_pi_line:

  Show simulated prediction interval lines. Default is `FALSE`.

- sim_pi_ci:

  Show simulated prediction interval CI ribbons. Default is `TRUE`.

- sim_pi_area:

  Show simulated prediction interval as shaded area. Default is `FALSE`.

- sim_median_line:

  Show simulated median line. Default is `FALSE`.

- sim_median_ci:

  Show simulated median CI ribbon. Default is `TRUE`.

## Value

A named list of logicals

## See also

Other vpc:
[`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md),
[`is_pmx_vpc_plot()`](https://ryancrass.github.io/pmxhelpr/reference/is_pmx_vpc_plot.md),
[`is_vpc_stats()`](https://ryancrass.github.io/pmxhelpr/reference/is_vpc_stats.md),
[`plot_build_vpc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_vpc.md),
[`plot_vpc_cens()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cens.md),
[`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md),
[`plot_vpc_legend()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_legend.md),
[`plot_vpc_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_theme.md)

## Examples

``` r
plot_vpc_shown()
#> $obs_point
#> [1] TRUE
#> 
#> $obs_pi_line
#> [1] TRUE
#> 
#> $obs_median_line
#> [1] TRUE
#> 
#> $sim_pi_line
#> [1] FALSE
#> 
#> $sim_pi_ci
#> [1] TRUE
#> 
#> $sim_pi_area
#> [1] FALSE
#> 
#> $sim_median_line
#> [1] FALSE
#> 
#> $sim_median_ci
#> [1] TRUE
#> 
plot_vpc_shown(obs_point = FALSE, sim_pi_line = TRUE)
#> $obs_point
#> [1] FALSE
#> 
#> $obs_pi_line
#> [1] TRUE
#> 
#> $obs_median_line
#> [1] TRUE
#> 
#> $sim_pi_line
#> [1] TRUE
#> 
#> $sim_pi_ci
#> [1] TRUE
#> 
#> $sim_pi_area
#> [1] FALSE
#> 
#> $sim_median_line
#> [1] FALSE
#> 
#> $sim_median_ci
#> [1] TRUE
#> 
```
