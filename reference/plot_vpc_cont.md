# Plot a visual predictive check (VPC) for continuous data with exact time bins

`plot_vpc_cont()` generates a VPC plot using exact time bins and returns
a `ggplot2` object. Thin wrapper that delegates to
[`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)
for computation and an internal builder for plot construction.

## Usage

``` r
plot_vpc_cont(
  data,
  time_var = "TIME",
  ntime_var = "NTIME",
  pred_var = "PRED",
  sim_dv_var = "SIMDV",
  obs_dv_var = "OBSDV",
  strat_var = NULL,
  pcvpc = FALSE,
  loq = NULL,
  irep_name = "SIM",
  min_bin_count = 1,
  show_rep = TRUE,
  lower_bound = 0,
  mode = c("auto", "rank", "drop"),
  shown = NULL,
  theme = NULL,
  pi = c(0.05, 0.95),
  ci = 0.9
)
```

## Arguments

- data:

  Input dataset. \`

- time_var:

  Column containing the actual time variable in `data`. Accepts bare
  names or strings. Default is `TIME`.

- ntime_var:

  Column containing the nominal time variable in `data`. Accepts bare
  names or strings. Default is `NTIME`.

- pred_var:

  Column containing population predictions in `data`. Accepts bare names
  or strings. Default is `PRED`.

- sim_dv_var:

  Column containing simulated DV in `data`. Accepts bare names or
  strings. Default is `SIMDV`.

- obs_dv_var:

  Column containing observed DV in `data`. Accepts bare names or
  strings. Default is `OBSDV`.

- strat_var:

  Stratification variable. Accepts bare names or strings. Currently,
  only a single stratifying variable is supported.

- pcvpc:

  logical for prediction correction. Default is `FALSE`.

- loq:

  Numeric scalar, or `NULL`. Lower limit of quantification (LLOQ). When
  `NULL` and column `LLOQ` is present in `data`, per-row `LLOQ` values
  are used as the censoring threshold; a scalar `loq` broadcasts to a
  constant threshold across rows. For standard VPCs (`pcvpc = FALSE`)

       + If a LOQ source is available (scalar `loq` or `LLOQ` column), all
       observations (including BLQ) are processed and censoring is performed
       at the quantile level. Filter to `EVID==0` so that doses are dropped.
       + If `loq=NULL` and `LLOQ` is NOT present in `data`, the dataset is
       filtered to `MDV==0` since `loq` is unknown.

  For prediction-corrected VPCs (`pcvpc = TRUE`) + If a LOQ source is
  available, all `SIMDV` and `OBSDV` values `< LOQ` (per row) are set to
  missing (`NA_real_`) so that both observed and simulated data are
  censored in the same way before quantile calculation. + If `loq=NULL`
  and `LLOQ` is NOT present in `data`, filter to `MDV==0` since `loq` is
  unknown. Dashed horizontal line plotted at each unique LLOQ value by
  default for standard VPCs (controlled via `theme`); suppressed for
  `pcvpc = TRUE` since `loq` has no meaning on the prediction-corrected
  scale.

- irep_name:

  Name of replicate variable in `data`. Accepts bare names or strings.
  Default is `SIM`.

- min_bin_count:

  Minimum number of quantifiable observations (`obs_n - obs_n_blq` in
  the summary statistics frame) per exact bin required for inclusion in
  binned plot layers. BLQ-encoded records (`obs_n_blq`) do not count
  toward this threshold. This argument drops small bins from summary
  statistic plotting but retains the underlying observations as data
  points.

- show_rep:

  Display number of replicates as a plot caption. Default is `TRUE`.

- lower_bound:

  Lower bound for prediction correction formula.

- mode:

  One of `"auto"` (default), `"rank"`, or `"drop"`. Controls how
  BLQ-encoded values are carried through quantile aggregation. In
  `"rank"` mode, BLQ rows ranks low at
  [`stats::quantile`](https://rdrr.io/r/stats/quantile.html);
  fully-censored quantiles return `-Inf` and are masked to `NA` before
  plotting. In `"drop"` mode, BLQ rows are excluded from quantile
  computation. `"auto"` resolves to `"rank"` for std VPC and `"drop"`
  for pcVPC, matching the package's historical behavior.

- shown:

  Layer visibility settings created by
  [`plot_vpc_shown()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_shown.md).
  Defaults can be viewed by running
  [`plot_vpc_shown()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_shown.md)
  with no arguments.

- theme:

  Named list of aesthetic parameters for the plot created by
  [`plot_vpc_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_theme.md).
  Defaults can be viewed by running
  [`plot_vpc_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_theme.md)
  with no arguments.

- pi:

  Numeric vector of length 2 specifying prediction interval quantiles.
  Default is `c(0.05, 0.95)`.

- ci:

  Numeric scalar in `(0, 1)` for simulation interval (e.g., `0.90` for
  90% CI). Default is `0.90`.

## Value

A `pmx_vpc_plot` object (a `ggplot2` subclass). To access the underlying
VPC summary statistics data.frame directly, use
[`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md).
Adding
[`facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
or
[`facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html)
to the result directly will warn, as stratification must be specified
via `strat_var` at call time.

## See also

Other vpc:
[`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md),
[`is_pmx_vpc_plot()`](https://ryancrass.github.io/pmxhelpr/reference/is_pmx_vpc_plot.md),
[`is_vpc_stats()`](https://ryancrass.github.io/pmxhelpr/reference/is_vpc_stats.md),
[`plot_build_vpc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_vpc.md),
[`plot_vpc_cens()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cens.md),
[`plot_vpc_legend()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_legend.md),
[`plot_vpc_shown()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_shown.md),
[`plot_vpc_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_theme.md)

## Examples

``` r
model <- model_mread_load(model = "pkmodel")
#> Loading model from cache.
data_sad_pk <- dplyr::filter(data_sad, CMT %in% c(1,2))
simout <- df_mrgsim_replicate(data = data_sad_pk, model = model, replicates = 100,
dv_var = ODV,
carry_out = c("LLOQ", "WTBL", "FOOD"),
recover  = c("USUBJID", "PART"),
irep_name = SIM)

vpc_plot <- plot_vpc_cont(
data = simout,
pcvpc = TRUE,
loq = 1,
pi = c(0.05, 0.95),
ci = 0.90)
```
