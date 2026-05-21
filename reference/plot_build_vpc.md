# Build a VPC ggplot from a `vpc_stats` object

Constructs a ggplot2 VPC plot from a
[`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)
result (or any object satisfying the `vpc_stats` contract). Applies the
`min_bin_count` filter, draws the simulated and observed quantile
layers, overlays the observation scatter, draws the LOQ reference line,
applies stratification facets, and adds the replicates caption and panel
theme. The `type` argument selects the continuous (concentration
quantile) or censored (BLQ proportion) layer set; the `pcvpc` argument
selects the standard or prediction-corrected column set within the
continuous type.

Most users will reach this function indirectly via
[`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md)
or
[`plot_vpc_cens()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cens.md).
Call `plot_build_vpc()` directly when working from a
manually-constructed or cached `vpc_stats` object — for example,
plotting a precomputed result from disk or a custom pipeline that
produces compatible columns.

## Usage

``` r
plot_build_vpc(
  compute_out,
  type = c("cont", "cens"),
  min_bin_count = 1,
  show_rep = TRUE,
  shown = NULL,
  theme = NULL,
  pcvpc = FALSE,
  loq = NULL,
  strat_var = NULL,
  bin_var = BIN_MID_VAR
)
```

## Arguments

- compute_out:

  A `vpc_stats` object (typically the output of
  [`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)).
  Must contain `stats` and `obs` data.frames with the columns documented
  in
  [`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md).
  Validated by
  [`validate_vpc_stats()`](https://ryancrass.github.io/pmxhelpr/reference/validate_vpc_stats.md)
  at entry.

- type:

  One of `"cont"` (default) or `"cens"`. `"cont"` plots the continuous
  concentration VPC (quantile ribbons and obs scatter). `"cens"` plots
  the BLQ-proportion VPC (proportion CI ribbon plus observed per-bin
  proportion line/points); requires `sim_prop_blq_*` columns in
  `compute_out$stats` (i.e.,
  [`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)
  was called with a LOQ source). `type = "cens"` is incompatible with
  `pcvpc = TRUE`.

- min_bin_count:

  Minimum number of observations per bin required for inclusion in
  binned plot layers. For `type = "cont"`, the threshold is applied to
  *quantifiable* obs only (`obs_n - obs_n_blq`); BLQ-encoded records do
  not count toward it. For `type = "cens"`, the threshold is applied to
  total obs (`obs_n`) — BLQ-heavy bins are the most informative on a
  cens VPC and are retained.

- show_rep:

  Logical. Display replicate count as a plot caption.

- shown:

  Layer visibility settings created by
  [`plot_vpc_shown()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_shown.md).
  For `type = "cens"`, only the `obs_point`, `obs_median_line`,
  `sim_median_line`, and `sim_median_ci` keys are read. Defaults follow
  [`plot_vpc_shown()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_shown.md):
  observed proportion line/points and simulated CI ribbon are shown; the
  simulated median line is off by default (pass
  `plot_vpc_shown(sim_median_line = TRUE)` to enable it).

- theme:

  Aesthetic parameters created by
  [`plot_vpc_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_theme.md).
  For `type = "cens"`, only the keys corresponding to the four cens
  layers above are read; other keys are ignored. The cens `obs_point`
  color is inherited from `obs_median_line` (so points match the obs
  line); the `obs_point` theme key still controls shape/alpha/size.

- pcvpc:

  Logical. When `TRUE`, plot the prediction-corrected columns (`pc_*`
  for stats, `PC_OBSDV` for the obs scatter) and suppress the LOQ
  reference line. Default is `FALSE` (standard VPC). Only meaningful
  when `type = "cont"`.

- loq:

  Numeric scalar or vector of LOQ values for the reference line, or
  `NULL` to suppress. When omitted (inherited from
  `compute_out$config$loq`) and `strat_var` is set and `compute_out$obs`
  carries a row-aligned `LOQ` column, ref lines are drawn per facet —
  each facet shows only the LLOQ values applicable to its strat-level
  rows. When `loq` is supplied explicitly, the value always wins and one
  global reference line is drawn per unique value of `loq` (the
  per-facet column-based dispatch is reserved for the inherited case).
  In the no-stratification or no-`obs$LOQ` cases, a global reference
  line is drawn per unique value of `loq` regardless of inheritance. The
  legend entry for LLOQ is *not* attached here — pass the same value to
  [`plot_vpc_legend()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_legend.md)
  when composing the legend panel. `compute_out$config$loq` is already a
  vector of unique non-NA LOQ values when
  [`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)
  populated the container. Forced to `NULL` when `pcvpc = TRUE` (LOQ has
  no meaning on the prediction-corrected scale).

- strat_var:

  Stratification variable. Accepts bare names or strings. Default is
  `NULL`. When `NULL`, the value is read from
  `compute_out$config$strat_var` so output of
  [`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)
  is handled automatically.

- bin_var:

  String. Binning variable name. Default is `"BIN_MID"`.

## Value

A `ggplot2` object.

## See also

Other vpc:
[`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md),
[`is_pmx_vpc_plot()`](https://ryancrass.github.io/pmxhelpr/reference/is_pmx_vpc_plot.md),
[`is_vpc_stats()`](https://ryancrass.github.io/pmxhelpr/reference/is_vpc_stats.md),
[`plot_vpc_cens()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cens.md),
[`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md),
[`plot_vpc_legend()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_legend.md),
[`plot_vpc_shown()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_shown.md),
[`plot_vpc_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_theme.md)

## Examples

``` r
model <- model_mread_load(model = "pkmodel")
#> Loading model from cache.
simout <- df_mrgsim_replicate(data = data_sad, model = model, replicates = 5,
                              dv_var = ODV, carry_out = "FOOD")
out <- df_vpcstats(simout, strat_var = FOOD)
#> Warning: Multiple unique values of `CMT` detected in `data` after filtering to `EVID == 0`: 2, 3.
#> ℹ Functions assume a single observation type per call.
#> ℹ Pre-filter to a single observation compartment (e.g., `dplyr::filter(data, CMT == <n>)`) before passing to this function.
p <- plot_build_vpc(out, pcvpc = FALSE)
```
