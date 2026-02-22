# Plot a visual predictive check (VPC) with exact time bins

`plot_vpc_exactbins()` is a wrapper function for
[`vpc::vpc()`](https://rdrr.io/pkg/vpc/man/vpc.html) that returns a
`ggplot2` object.

## Usage

``` r
plot_vpc_exactbins(
  sim,
  pcvpc = FALSE,
  time_vars = c(TIME = "TIME", NTIME = "NTIME"),
  output_vars = c(PRED = "PRED", IPRED = "IPRED", SIMDV = "SIMDV", OBSDV = "OBSDV"),
  loq = NULL,
  strat_var = NULL,
  irep_name = "SIM",
  min_bin_count = 1,
  show_rep = TRUE,
  lower_bound = 0,
  shown = NULL,
  theme = NULL,
  timeu = "hours",
  n_breaks = 8,
  ...
)
```

## Arguments

- sim:

  Input dataset. Must contain the following variables: `"ID"`, `"TIME"`

- pcvpc:

  logical for prediction correction. Default is `FALSE`.

- time_vars:

  Names of actual and nominal time variables. Must be named character
  vector. Defaults is: c(`TIME`=`"TIME"`, `NTIME`=`"NTIME"`).

- output_vars:

  Names of model outputs from `model`. Must be named character vector.
  Defaults is: c(`PRED`= `"PRED"`, `IPRED` = `"IPRED"`, `DV`= `"DV"`).

- loq:

  Numeric value of the lower limit of quantification (LLOQ) for the
  assay. Passed to `lloq` argument of
  [`vpc::vpc()`](https://rdrr.io/pkg/vpc/man/vpc.html). Specifying this
  argument implies that `OBSDV` is missing in `sim` where \< LLOQ.

- strat_var:

  Character string of stratification variable passed to `stratify`
  argument of [`vpc::vpc()`](https://rdrr.io/pkg/vpc/man/vpc.html).
  Currently, only a single stratifying variable is supported.

- irep_name:

  Name of replicate variable in `data`. Must be a string. Default is
  `"SIM"`.

- min_bin_count:

  Minimum number of quantifiable observations in exact bin for inclusion
  in binned plot layers. This argument drops small bins from summary
  statistic calculation but retains these observations in the observed
  data points.

- show_rep:

  Display number of replicates as a plot caption. Default is `TRUE`.

- lower_bound:

  Lower bound of the dependent variable for prediction correction.
  Default is `0`.

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

- theme:

  Named list of aesthetic parameters for the plot.Passed to `vpc_theme`
  arumgent of [`vpc::vpc()`](https://rdrr.io/pkg/vpc/man/vpc.html).
  Defaults can be obtained by running
  [`vpc::new_vpc_theme()`](https://rdrr.io/pkg/vpc/man/new_vpc_theme.html)
  with no arguments.

- timeu:

  Character string specifying units for the time variable. Passed to
  `breaks_time` and assigned to default x-axis label. Options include:

  - "hours" (default)

  - "days"

  - "weeks"

  - "months"

- n_breaks:

  Number of breaks requested for x-axis. Default is 8.

- ...:

  Other arguments passed to
  [`vpc::vpc()`](https://rdrr.io/pkg/vpc/man/vpc.html).

## Value

A list containing calculated VPC information (when `vpcdb=TRUE`), or a
ggplot2 object (default)

## Examples

``` r
model <- model_mread_load(model = "model")
#> Loading model from cache.
simout <- df_mrgsim_replicate(data = data_sad, model = model, replicates = 100,
dv_var = "ODV",
num_vars = c("CMT", "EVID", "MDV", "NTIME", "LLOQ", "WTBL", "FOOD"),
char_vars = c("USUBJID", "PART"),
irep_name = "SIM")

vpc_plot <- plot_vpc_exactbins(
sim = simout,
pcvpc = TRUE,
pi = c(0.05, 0.95),
ci = c(0.05, 0.95))
#> Joining with `by = join_by(NTIME, CMT)`
#> Joining with `by = join_by(NTIME, CMT)`
#> Scale for x is already present.
#> Adding another scale for x, which will replace the existing scale.
```
