# Plot a legend for a visual predictive check (VPC)

Plot a legend for a visual predictive check (VPC)

## Usage

``` r
plot_vpclegend(
  ci = c(0.05, 0.95),
  pi = c(0.05, 0.95),
  shown = NULL,
  lloq = NULL,
  update = NULL,
  ...
)
```

## Arguments

- ci:

  simulated confidence interval plotted. Should match argument passed to
  [`vpc::vpc()`](https://rdrr.io/pkg/vpc/man/vpc.html). Default is
  c(0.05, 0.95).

- pi:

  prediction intervals plotted. Should match argument passed to
  [`vpc::vpc()`](https://rdrr.io/pkg/vpc/man/vpc.html). Default is
  c(0.05, 0.95).

- shown:

  Named list of logicals specifying which layers to include on the plot.
  Passed to `show` argument of
  [`vpc::vpc()`](https://rdrr.io/pkg/vpc/man/vpc.html).

  Defaults are:

  - Observed points: `obs_dv` = TRUE.

  - Observed quantiles: `obs_ci` = TRUE

  - Simulated inter-quantile range:`pi` = FALSE

  - Simulated inter-quantile area: `pi_as_area` = FALSE

  - Simulated Quantile CI: `pi_ci` = TRUE

  - Observed Median: `obs_median` = TRUE

  - Simulated Median: `sim_median` = FALSE

  - Simulated Median CI: `sim_median_ci` = TRUE

- lloq:

  label for lower limit of quantification in the plot legend.

- update:

  list containing the plot elements to be updated. Default is set by
  [`pmxhelpr_vpc_theme()`](https://ryancrass.github.io/pmxhelpr/reference/pmxhelpr_vpc_theme.md).

- ...:

  Other arguments passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

## Value

a ggplot2 object

## Examples

``` r
plot_vpclegend()
#> Warning: Arguments in `...` must be used.
#> ✖ Problematic argument:
#> • na.rm = TRUE
#> ℹ Did you misspell an argument name?
#> Warning: Ignoring empty aesthetic: `linewidth`.
#> Warning: Ignoring empty aesthetic: `linewidth`.

plot_vpclegend(
pi = c(0.025, 0.975),
ci = c(0.025, 0.925),
 shown = list(obs_dv = FALSE, obs_ci = TRUE,
 pi = FALSE, pi_as_area = FALSE, pi_ci = TRUE,
 obs_median = TRUE,
 sim_median =FALSE, sim_median_ci = TRUE))
#> Warning: Arguments in `...` must be used.
#> ✖ Problematic argument:
#> • na.rm = TRUE
#> ℹ Did you misspell an argument name?
#> Warning: Ignoring empty aesthetic: `linewidth`.
#> Warning: Ignoring empty aesthetic: `linewidth`.
```
