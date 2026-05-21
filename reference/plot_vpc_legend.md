# Plot a legend for a visual predictive check (VPC)

Plot a legend for a visual predictive check (VPC)

## Usage

``` r
plot_vpc_legend(
  ci = 0.9,
  pi = c(0.05, 0.95),
  shown = NULL,
  lloq = NULL,
  theme = NULL,
  type = c("cont", "cens"),
  ...
)
```

## Arguments

- ci:

  Numeric confidence level for simulation intervals (e.g., `0.90` for
  90% CI). Should match argument passed to
  [`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md).
  Default is `0.90`.

- pi:

  prediction intervals plotted. Should match argument passed to
  [`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md).
  Default is c(0.05, 0.95). Ignored when `type = "cens"` (cens VPCs do
  not have prediction intervals).

- shown:

  Layer visibility settings created by
  [`plot_vpc_shown()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_shown.md).
  Defaults can be viewed by running
  [`plot_vpc_shown()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_shown.md)
  with no arguments.

- lloq:

  Numeric scalar or vector of LLOQ values to label in the legend, or
  `NULL` to omit. Each unique value becomes one legend entry rendered
  with the theme's `loq_line` linetype. Pass `compute_out$config$loq`
  from a
  [`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)
  result to mirror the reference lines drawn by
  [`plot_build_vpc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_vpc.md).

- theme:

  Named list of aesthetic parameters for the plot created by
  [`plot_vpc_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_theme.md).
  Defaults can be viewed by running
  [`plot_vpc_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_theme.md)
  with no arguments.

- type:

  One of `"cont"` (default) or `"cens"`. Selects the labels and layer
  set the legend describes. Under `"cont"`, the central-tendency entries
  are labeled `"Obs Med"`, `"Sim Med"`, and `"Sim <ci>% CI Med"` and the
  pi entries (`"Obs <p1>th and <p2>th"`, `"Sim <p1>th - <p2>th"`,
  `"Sim <ci>% CI <p1>th and <p2>th"`) are included when their `shown`
  keys are on. Under `"cens"`, the three central-tendency labels are
  relabeled to `"Obs Prop BLQ"`, `"Sim Prop BLQ"`,
  `"Sim <ci>% CI Prop BLQ"` and all pi-related entries are suppressed
  regardless of `shown` (cens VPCs have no prediction interval).

- ...:

  Other arguments passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

## Value

a ggplot2 object

## See also

Other vpc:
[`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md),
[`is_pmx_vpc_plot()`](https://ryancrass.github.io/pmxhelpr/reference/is_pmx_vpc_plot.md),
[`is_vpc_stats()`](https://ryancrass.github.io/pmxhelpr/reference/is_vpc_stats.md),
[`plot_build_vpc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_vpc.md),
[`plot_vpc_cens()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cens.md),
[`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md),
[`plot_vpc_shown()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_shown.md),
[`plot_vpc_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_theme.md)

## Examples

``` r
plot_vpc_legend()

plot_vpc_legend(
pi = c(0.025, 0.975),
ci = 0.95,
 shown = plot_vpc_shown(obs_point = FALSE, obs_pi_line = TRUE,
 sim_pi_line = FALSE, sim_pi_area = FALSE, sim_pi_ci = TRUE,
 obs_median_line = TRUE,
 sim_median_line = FALSE, sim_median_ci = TRUE))

## Cens VPC legend
plot_vpc_legend(type = "cens", lloq = 1)
```
