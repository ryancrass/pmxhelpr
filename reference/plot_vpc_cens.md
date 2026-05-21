# Plot a censoring (BLQ-proportion) VPC with exact time bins

`plot_vpc_cens()` generates a VPC plot of the proportion of
below-limit-of-quantification (BLQ) observations over time and returns a
`ggplot2` object. Mirrors
[`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md)
structurally but plots the BLQ proportion rather than concentration
quantiles: the simulated non-parametric confidence band is the empirical
distribution of per-replicate BLQ proportions across `ci` bounds, and
the observed proportion is a single point/line per bin.

Thin wrapper that delegates to
[`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)
for computation and to
[`plot_build_vpc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_vpc.md)
(`type = "cens"`) for plot construction. The `sim_prop_blq_*` columns
produced by
[`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)
when a LOQ source is available are the sole source of plot data.

## Usage

``` r
plot_vpc_cens(
  data,
  time_var = "TIME",
  ntime_var = "NTIME",
  pred_var = "PRED",
  sim_dv_var = "SIMDV",
  obs_dv_var = "OBSDV",
  strat_var = NULL,
  loq = NULL,
  irep_name = "SIM",
  min_bin_count = 1,
  show_rep = TRUE,
  shown = NULL,
  theme = NULL,
  ci = 0.9
)
```

## Arguments

- data:

  Input dataset. Simulated replicate data (typically from
  [`df_mrgsim_replicate()`](https://ryancrass.github.io/pmxhelpr/reference/df_mrgsim_replicate.md))
  **or** a precomputed `vpc_stats` container (output of
  [`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)).
  When a precomputed container is passed, the pipeline does not re-run
  and pipeline arguments are rejected; pass raw data when you need
  pipeline control.

- time_var:

  Column containing the actual time variable in `data`. Accepts bare
  names or strings. Default is `TIME`.

- ntime_var:

  Column containing the nominal time variable in `data`. Accepts bare
  names or strings. Default is `NTIME`.

- pred_var:

  Column containing population predictions in `data`. Accepts bare names
  or strings. Default is `PRED`. Required by
  [`df_vpcpreprocess()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcpreprocess.md)
  even though `PRED` does not appear in cens output.

- sim_dv_var:

  Column containing simulated DV in `data`. Accepts bare names or
  strings. Default is `SIMDV`.

- obs_dv_var:

  Column containing observed DV in `data`. Accepts bare names or
  strings. Default is `OBSDV`.

- strat_var:

  Stratification variable. Accepts bare names or strings. Currently,
  only a single stratifying variable is supported.

- loq:

  Numeric scalar, or `NULL`. Lower limit of quantification (LLOQ).
  Either `loq` or an `LLOQ` column in `data` is **required** — without a
  LOQ source,
  [`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)
  does not emit the `sim_prop_blq_*` columns a cens VPC needs. When
  `NULL` and column `LLOQ` is present in `data`, per-row `LLOQ` values
  are used as the censoring threshold; a scalar `loq` broadcasts to a
  constant threshold across rows.

- irep_name:

  Name of replicate variable in `data`. Accepts bare names or strings.
  Default is `SIM`.

- min_bin_count:

  Minimum number of observations per exact bin required for inclusion in
  binned plot layers. Applied to total obs (`obs_n` in the summary
  statistics frame); BLQ-encoded records *do* count toward this
  threshold, in contrast to
  [`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md)
  which requires quantifiable obs. Rationale: BLQ-heavy bins are the
  most informative on a cens VPC and should be retained.

- show_rep:

  Display number of replicates as a plot caption. Default is `TRUE`.

- shown:

  Layer visibility settings created by
  [`plot_vpc_shown()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_shown.md).
  The cens builder reads four of the keys: `obs_point`,
  `obs_median_line`, `sim_median_line`, `sim_median_ci`. Defaults follow
  [`plot_vpc_shown()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_shown.md):
  the observed proportion line/points and simulated CI ribbon are shown;
  the simulated median line is off by default. Pass
  `plot_vpc_shown(sim_median_line = TRUE)` to enable it.

- theme:

  Named list of aesthetic parameters created by
  [`plot_vpc_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_theme.md).
  The cens builder reads the same four keys listed above (`obs_point`,
  `obs_median_line`, `sim_median_line`, `sim_median_ci`); other keys are
  ignored.

- ci:

  Numeric scalar in `(0, 1)` for the simulated CI bound on the BLQ
  proportion across replicates (e.g., `0.90` for 90% CI). Default is
  `0.90`. Honored only on the raw-data path; passing `ci` explicitly
  when `data` is a precomputed `vpc_stats` container raises an error.
  The container's stored `ci` (set when
  [`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)
  was called) is the source of truth on that path.

## Value

A `pmx_vpc_plot` object (a `ggplot2` subclass). To access the underlying
VPC summary statistics directly, use
[`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)
and inspect the `obs_prop_blq` and `sim_prop_blq_*` columns of the
`stats` data.frame.

## See also

Other vpc:
[`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md),
[`is_pmx_vpc_plot()`](https://ryancrass.github.io/pmxhelpr/reference/is_pmx_vpc_plot.md),
[`is_vpc_stats()`](https://ryancrass.github.io/pmxhelpr/reference/is_vpc_stats.md),
[`plot_build_vpc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_vpc.md),
[`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md),
[`plot_vpc_legend()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_legend.md),
[`plot_vpc_shown()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_shown.md),
[`plot_vpc_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_theme.md)

## Examples

``` r
model <- model_mread_load(model = "pkmodel")
#> Loading model from cache.
data_sad_pk <- dplyr::filter(data_sad, CMT %in% c(1, 2))
simout <- df_mrgsim_replicate(data = data_sad_pk, model = model,
                              replicates = 100, dv_var = ODV,
                              carry_out = c("LLOQ", "WTBL", "FOOD"),
                              recover = c("USUBJID", "PART"),
                              irep_name = SIM)

cens_plot <- plot_vpc_cens(data = simout, loq = 1, ci = 0.90)
```
