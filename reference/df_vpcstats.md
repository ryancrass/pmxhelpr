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
  to `NA`. `pc_sim_*` quantiles are `NA` in bins with no quantifiable
  observations (`obs_n == obs_n_blq`). Carries attributes
  `n_replicates`, `loq`, and `strat_var`.

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
[`style_vpc()`](https://ryancrass.github.io/pmxhelpr/reference/style_vpc.md)

## Examples

``` r
model <- model_mread_load(model = "pkmodel")
#> Loading model from cache.
simout <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT %in% c(1, 2)),
                              model = model, replicates = 10,
                              dv_var = ODV)
result <- df_vpcstats(simout, loq = 1)
head(result$stats)
#> # A tibble: 6 × 34
#>   BIN_MID obs_n obs_n_blq obs_prop_blq sim_prop_blq_low sim_prop_blq_med
#>     <dbl> <int>     <int>        <dbl>            <dbl>            <dbl>
#> 1     0      36        36       1                1                1     
#> 2     0.5    36         2       0.0556           0.0681           0.111 
#> 3     1      36         0       0                0                0.0278
#> 4     1.5    36         0       0                0                0     
#> 5     2      36         0       0                0                0     
#> 6     3      36         0       0                0                0     
#> # ℹ 28 more variables: sim_prop_blq_hi <dbl>, obs_low <dbl>, obs_med <dbl>,
#> #   obs_hi <dbl>, sim_low_low <dbl>, sim_low_med <dbl>, sim_low_hi <dbl>,
#> #   sim_med_low <dbl>, sim_med_med <dbl>, sim_med_hi <dbl>, sim_hi_low <dbl>,
#> #   sim_hi_med <dbl>, sim_hi_hi <dbl>, pc_obs_low <dbl>, pc_obs_med <dbl>,
#> #   pc_obs_hi <dbl>, pc_sim_low_low <dbl>, pc_sim_low_med <dbl>,
#> #   pc_sim_low_hi <dbl>, pc_sim_med_low <dbl>, pc_sim_med_med <dbl>,
#> #   pc_sim_med_hi <dbl>, pc_sim_hi_low <dbl>, pc_sim_hi_med <dbl>, …
head(result$obs)
#>   ID TIME BIN_MID      PRED     IPRED     SIMDV OBSDV EVID CMT MDV      GUT
#> 1  1 0.81     1.0  2.393706 0.5659779 0.7298292  1.96    0   2   0 4.132762
#> 2  1 1.49     1.5  5.590033 1.3880030 1.0785968  3.86    0   2   0 3.724851
#> 3  1 2.11     2.0  8.176810 2.1407808 2.2112115  3.34    0   2   0 3.388126
#> 4  1 3.05     3.0 10.974942 3.1429334 3.3036137  6.83    0   2   0 2.934755
#> 5  1 4.14     4.0 12.666037 4.0503191 4.9213355  8.87    0   2   0 2.484441
#> 6  1 5.14     5.0 13.168275 4.6617823 5.1588934 11.92    0   2   0 2.132351
#>         CENT      PERIPH    TRANS1     TRANS2         Y SIM LOQ  PC_OBSDV
#> 1 0.02308702 0.004109114 0.5115784 0.03166314 0.7298292   1   1  40.64147
#> 2 0.05661856 0.019382671 0.8481680 0.09656615 1.0785968   1   1  67.33181
#> 3 0.08732541 0.043617826 1.0925184 0.17614404 2.2112115   1   1  59.59779
#> 4 0.12820460 0.095402436 1.3679129 0.31879759 3.3036137   1   1 132.56808
#> 5 0.16521811 0.170075613 1.5718669 0.49724779 4.9213355   1   1 183.28737
#> 6 0.19016054 0.244844700 1.6749763 0.65785251 5.1588934   1   1 259.15132
```
