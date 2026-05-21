# Compute VPC summary statistics from raw simulation data

Returns the VPC summary statistics and observation overlay underlying a
continuous-data VPC plot. Takes raw simulation output (e.g., from
[`df_mrgsim_replicate()`](https://ryancrass.github.io/pmxhelpr/reference/df_mrgsim_replicate.md))
and always emits **both** standard and prediction-corrected statistics
in a single call. Preprocessing — column renames, dose-row filtering,
BLQ encoding, prediction-correction — is applied internally.

For a plotted VPC, use
[`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md),
which is a thin wrapper that calls `df_vpcstats()` and forwards the
result to the internal plot builder. The standard vs.
prediction-corrected view is selected at plot time via
`plot_vpc_cont(pcvpc = TRUE/FALSE)`.

## Usage

``` r
df_vpcstats(
  data,
  time_var = "TIME",
  ntime_var = "NTIME",
  pred_var = "PRED",
  sim_dv_var = "SIMDV",
  obs_dv_var = "OBSDV",
  strat_var = NULL,
  loq = NULL,
  irep_name = "SIM",
  lower_bound = 0,
  mode = c("auto", "rank", "drop"),
  pi = c(0.05, 0.95),
  ci = 0.9
)
```

## Arguments

- data:

  Raw simulation output. Must contain `EVID`, `MDV`, `PRED`, and columns
  named by `time_var`, `ntime_var`, `sim_dv_var`, `obs_dv_var`, and
  `irep_name`.

- time_var:

  Column containing the actual time variable in `data`. Accepts bare
  names or strings. Default is `TIME`.

- ntime_var:

  Column containing the nominal time variable in `data`. Accepts bare
  names or strings. Default is `NTIME`.

- pred_var:

  Column containing population predictions in `data`. Accepts bare names
  or strings. Default is `PRED`. Required for the prediction-corrected
  statistics.

- sim_dv_var:

  Column containing simulated DV in `data`. Accepts bare names or
  strings. Default is `SIMDV`.

- obs_dv_var:

  Column containing observed DV in `data`. Accepts bare names or
  strings. Default is `OBSDV`.

- strat_var:

  Stratification variable. Accepts bare names or strings. Default is
  `NULL`. Only a single stratifying variable is supported.

- loq:

  Numeric scalar, or `NULL`. When `NULL` and column `LLOQ` is present in
  `data`, per-row `LLOQ` values are used as the censoring threshold; the
  unique non-NA values are exposed via `config$loq` for plotting. A
  scalar `loq` broadcasts to a constant threshold across rows.

- irep_name:

  Replicate identifier column. Accepts bare names or strings. Default is
  `SIM`.

- lower_bound:

  Lower bound for prediction correction formula.

- mode:

  One of `"auto"` (default), `"rank"`, or `"drop"`. See
  [`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md)
  for semantics. `"auto"` resolves to `"rank"` for the standard flavor
  and `"drop"` for the prediction-corrected flavor.

- pi:

  Numeric vector of length 2 specifying prediction interval quantiles.
  Default is `c(0.05, 0.95)`.

- ci:

  Numeric scalar in `(0, 1)` specifying the simulation interval (e.g.,
  `0.90` for a 90% CI). Default is `0.90`.

## Value

A list with two data.frames (class `c("vpc_stats", "list")`):

- `stats`:

  Wide summary statistics. Standard-VPC columns are unprefixed
  (`obs_low/med/hi`, `sim_low_low/med/hi`, etc.); prediction-corrected
  counterparts carry a `pc_` prefix (`pc_obs_low`, `pc_sim_low_med`,
  etc.). `obs_n`, `obs_n_blq`, `obs_prop_blq`, `sim_prop_blq_*`, `ci`,
  `pi_low`, `pi_hi` are single (not duplicated). `sim_prop_blq_*` is
  std-only — LOQ has no meaning on the prediction-corrected scale.
  `-Inf` quantile values from rank-mode fully-censored bins are masked
  to `NA`. Carries attributes `n_replicates`, `loq`, and `strat_var`.

- `obs`:

  First-replicate observation rows with `MDV == 0`, used as the scatter
  overlay in
  [`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md).
  `OBSDV` carries the std-scale value; `PC_OBSDV` carries the
  prediction-corrected counterpart.

## See also

Other vpc:
[`is_pmx_vpc_plot()`](https://ryancrass.github.io/pmxhelpr/reference/is_pmx_vpc_plot.md),
[`is_vpc_stats()`](https://ryancrass.github.io/pmxhelpr/reference/is_vpc_stats.md),
[`plot_build_vpc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_vpc.md),
[`plot_vpc_cens()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cens.md),
[`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md),
[`plot_vpc_legend()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_legend.md),
[`plot_vpc_shown()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_shown.md),
[`plot_vpc_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_theme.md)

## Examples

``` r
model <- model_mread_load(model = "pkmodel")
#> Loading model from cache.
simout <- df_mrgsim_replicate(data = data_sad, model = model, replicates = 10,
                              dv_var = ODV)
result <- df_vpcstats(simout, loq = 1)
#> Warning: Multiple unique values of `CMT` detected in `data` after filtering to `EVID == 0`: 2, 3.
#> ℹ Functions assume a single observation type per call.
#> ℹ Pre-filter to a single observation compartment (e.g., `dplyr::filter(data, CMT == <n>)`) before passing to this function.
head(result$stats)
#> # A tibble: 6 × 34
#>   BIN_MID obs_n obs_n_blq obs_prop_blq sim_prop_blq_low sim_prop_blq_med
#>     <dbl> <int>     <int>        <dbl>            <dbl>            <dbl>
#> 1     0      72        72       1                1                1     
#> 2     0.5    72         4       0.0556           0.0618           0.0903
#> 3     1      72         0       0                0                0.0278
#> 4     1.5    72         0       0                0                0     
#> 5     2      72         0       0                0                0     
#> 6     3      72         0       0                0                0     
#> # ℹ 28 more variables: sim_prop_blq_hi <dbl>, obs_low <dbl>, obs_med <dbl>,
#> #   obs_hi <dbl>, sim_low_low <dbl>, sim_low_med <dbl>, sim_low_hi <dbl>,
#> #   sim_med_low <dbl>, sim_med_med <dbl>, sim_med_hi <dbl>, sim_hi_low <dbl>,
#> #   sim_hi_med <dbl>, sim_hi_hi <dbl>, pc_obs_low <dbl>, pc_obs_med <dbl>,
#> #   pc_obs_hi <dbl>, pc_sim_low_low <dbl>, pc_sim_low_med <dbl>,
#> #   pc_sim_low_hi <dbl>, pc_sim_med_low <dbl>, pc_sim_med_med <dbl>,
#> #   pc_sim_med_hi <dbl>, pc_sim_hi_low <dbl>, pc_sim_hi_med <dbl>, …
head(result$obs)
#>   ID TIME BIN_MID     PRED     IPRED     SIMDV    OBSDV EVID CMT MDV      GUT
#> 1  1 0.81     1.0 2.469903 0.5809776 0.6000915  2.02000    0   2   0 4.132762
#> 2  1 0.81     1.0 2.469903 0.5809776 0.6106797 99.44932    0   3   0 4.132762
#> 3  1 1.49     1.5 5.869272 1.4434493 1.7538614  4.02000    0   2   0 3.724851
#> 4  1 1.49     1.5 5.869272 1.4434493 1.5973721 97.60720    0   3   0 3.724851
#> 5  1 2.11     2.0 8.672230 2.2412012 2.3345173  3.50000    0   2   0 3.388126
#> 6  1 2.11     2.0 8.672230 2.2412012 2.5344445 95.25727    0   3   0 3.388126
#>         CENT      PERIPH    TRANS1     TRANS2         Y SIM LOQ   PC_OBSDV
#> 1 0.02369888 0.003378545 0.5115784 0.03166314 0.6000915   1   1   42.00469
#> 2 0.02369888 0.003378545 0.5115784 0.03166314 0.6106797   1   1 2067.98928
#> 3 0.05888029 0.016219628 0.8481680 0.09656615 1.7538614   1   1   70.01649
#> 4 0.05888029 0.016219628 0.8481680 0.09656615 1.5973721   1   1 1700.02826
#> 5 0.09142169 0.036971977 1.0925184 0.17614404 2.3345173   1   1   62.31583
#> 6 0.09142169 0.036971977 1.0925184 0.17614404 2.5344445   1   1 1696.01013
```
