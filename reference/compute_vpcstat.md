# Internal helper: per-flavor VPC quantile aggregation

Computes the per-bin two-stage simulated quantile summary plus observed
quantiles for a single VPC flavor (standard or prediction-corrected).
Called twice by
[`df_vpccompute()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpccompute.md)
— once with std-mode data, once with a prediction-corrected copy.

## Usage

``` r
compute_vpcstat(data, group_vars, irep_name, pi, ci_bounds, has_loq, pcvpc)
```

## Arguments

- data:

  Preprocessed data with standardized columns. For the pc flavor,
  `OBSDV` and `SIMDV` are expected to already be prediction-corrected.

- group_vars:

  Character vector of grouping columns (`bin_var` and optional
  `strat_var`).

- irep_name:

  String. Replicate identifier column name.

- pi:

  Numeric vector of length 2 specifying prediction interval quantiles.

- ci_bounds:

  Numeric vector of length 2 specifying CI bounds derived from `ci`.

- has_loq:

  Logical. When `TRUE`, the `data` frame is expected to carry a
  row-aligned `LOQ` column and `sim_prop_blq_*` columns are produced.
  When `FALSE`, `sim_prop_blq_*` columns are dropped.

- pcvpc:

  Logical. Selects the BLQ-detection expression for `sim_prop_blq` in
  stage 1: in pcVPC, BLQ rows were masked to NA before
  prediction-correction, so detection uses `is.na(SIMDV)`; in std VPC,
  detection uses `(SIMDV < LOQ) | is.na(SIMDV)` per row.

## Value

A `data.frame` with `group_vars`, `obs_n`, `obs_n_blq`, `obs_prop_blq`,
the simulated quantile CIs (`sim_low_low/med/hi`, `sim_med_low/med/hi`,
`sim_hi_low/med/hi`), the observed quantiles (`obs_low`, `obs_med`,
`obs_hi`), and (when `has_loq = TRUE`) `sim_prop_blq_low/med/hi`.
Quantile columns are masked through
[`var_infna()`](https://ryancrass.github.io/pmxhelpr/reference/var_infna.md).
