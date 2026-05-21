# Preprocess simulation data for VPC statistics

Internal function. Validates columns, resolves `loq` (inheritance from
an `LLOQ` column when not explicitly supplied), filters dose rows
(`EVID == 0`), renames simulation-output columns to standard internal
names, and applies BLQ encoding to `OBSDV`. Prediction-correction and
the (mode-aware) BLQ-encoding of `SIMDV` are handled per-flavor in
[`df_vpccompute()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpccompute.md).

BLQ encoding (in this stage):

- `OBSDV` positions where `MDV == 1`, `is.na(OBSDV)`, or (when a LOQ
  source is available) `OBSDV < LOQ` are encoded as `-Inf` via
  [`var_loqcens()`](https://ryancrass.github.io/pmxhelpr/reference/var_loqcens.md).

## Usage

``` r
df_vpcpreprocess(
  data,
  time_var_str,
  ntime_var_str,
  pred_var_str,
  sim_dv_var_str,
  obs_dv_var_str,
  strat_var_str = NULL,
  irep_name_str = "SIM",
  loq = NULL
)
```

## Arguments

- data:

  Simulated data from
  [`df_mrgsim_replicate()`](https://ryancrass.github.io/pmxhelpr/reference/df_mrgsim_replicate.md)
  or equivalent.

- time_var_str:

  String name of the actual time column in `data`.

- ntime_var_str:

  String name of the nominal time column in `data`.

- pred_var_str:

  String name of the population prediction column in `data`.

- sim_dv_var_str:

  String name of the simulated DV column in `data`.

- obs_dv_var_str:

  String name of the observed DV column in `data`.

- strat_var_str:

  String or `NULL`. Stratification variable name.

- irep_name_str:

  String. Replicate identifier column name. Default is `"SIM"`.

- loq:

  Numeric scalar, or `NULL`. When `NULL` and column `LLOQ` is present in
  `data`, per-row `LLOQ` values are used as the censoring threshold. A
  scalar `loq` broadcasts to a constant threshold across rows.

## Value

A preprocessed data.frame with standardized columns and (when a LOQ
source exists) a row-aligned `LOQ` column. The unique non-NA LOQ values
are attached as `attr(., "loq")`.
