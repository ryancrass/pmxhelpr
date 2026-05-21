# Compute VPC quantile statistics from preprocessed data

Internal function. Always emits both standard and prediction-corrected
summary statistics: the std flavor uses raw `OBSDV`/`SIMDV`; the pc
flavor applies `var_loqcens` to `SIMDV` (when `loq` is supplied) and
[`var_predcorr()`](https://ryancrass.github.io/pmxhelpr/reference/var_predcorr.md)
per (`bin_var` × `strat_var` × `CMT`) to both `OBSDV` and `SIMDV`. The
pc columns are returned with a `pc_` prefix.

## Usage

``` r
df_vpccompute(
  data,
  pi = c(0.05, 0.95),
  ci = 0.9,
  bin_var = BIN_MID_VAR,
  strat_var = NULL,
  irep_name = "SIM",
  lower_bound = 0,
  mode = c("auto", "rank", "drop"),
  loq = attr(data, "loq")
)
```

## Arguments

- data:

  Preprocessed simulation data, typically from
  [`df_vpcpreprocess()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcpreprocess.md).

- pi:

  Numeric vector of length 2 specifying prediction interval quantiles.
  Default is `c(0.05, 0.95)`.

- ci:

  Numeric scalar in `(0, 1)` specifying the simulation interval (e.g.
  `0.90` for a 90% CI). Default is `0.90`.

- bin_var:

  String. Binning variable name. Default is `"BIN_MID"`.

- strat_var:

  String or `NULL`. Stratification variable name. Default is `NULL`.

- irep_name:

  String. Replicate identifier column name. Default is `"SIM"`.

- lower_bound:

  Numeric. Forwarded to
  [`var_predcorr()`](https://ryancrass.github.io/pmxhelpr/reference/var_predcorr.md)
  in the pc flavor.

- mode:

  One of `"auto"` (default), `"rank"`, or `"drop"`. Controls how
  BLQ-encoded values flow into quantile aggregation per-flavor: `"auto"`
  uses `"rank"` for the std flavor and `"drop"` for the pc flavor (the
  historical defaults); `"rank"` and `"drop"` apply to both flavors.

- loq:

  Numeric vector of unique LOQ values present in `data` (length 1 when
  LLOQ is constant), or `NULL`. Defaults to `attr(data, "loq")`. When
  non-NULL, the std stats include `sim_prop_blq_low/med/hi` (BLQ
  detection reads per-row thresholds from `data$LOQ`). The pc stats do
  *not* emit a `pc_sim_prop_blq_*` set — LOQ has no meaning on the
  prediction-corrected scale.

## Value

A list of two data.frames with class `c("vpc_stats", "list")`:

- `stats`:

  Wide summary with both std and pc columns. Std columns are unprefixed
  (`obs_low/med/hi`, `sim_low_*` etc.); pc-flavor observed and simulated
  quantile columns carry a `pc_` prefix (`pc_obs_low`, `pc_sim_low_med`,
  etc.). `obs_n`, `obs_n_blq`, `obs_prop_blq`, `sim_prop_blq_*`, `ci`,
  `pi_low`, `pi_hi` are single (not duplicated). Carries attributes
  `n_replicates`, `loq`, and `strat_var`.

- `obs`:

  First-replicate observation rows with `MDV == 0`, carrying both
  `OBSDV` (std) and `PC_OBSDV` (prediction-corrected).
