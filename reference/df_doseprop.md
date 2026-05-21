# Compute and tabulate estimates for log-log regression

Computes per-metric log-log regression statistics and returns them in a
cacheable, replottable container. The returned object is a
[`pmx_stats()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_stats.md)
container with class `c("doseprop_stats", "pmx_stats")` and three slots
— `stats` (per-metric regression body), `obs` (filtered observation rows
used for the scatter overlay), and `config` (regression configuration:
`metric_name_var`, `metric_value_var`, `dose_var`, `ci`, `method`) — so
that
[`plot_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_doseprop.md)
/
[`plot_build_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_doseprop.md)
can render directly from this object without re-fitting any regressions.

## Usage

``` r
df_doseprop(
  data,
  metrics,
  metric_name_var = "PPTESTCD",
  metric_value_var = "PPORRES",
  dose_var = "DOSE",
  method = "normal",
  ci = 0.9,
  sigdigits = 3
)
```

## Arguments

- data:

  Input dataset for log-log regression. Default expected format is
  output from `PKNCA::pk.nca()` (i.e., SDTM PP formatting).

- metrics:

  character vector of exposure metrics in `data` to plot

- metric_name_var:

  Column in `data` containing the metric names listed in `metrics`.
  Accepts bare names or strings. Default is `PPTESTCD`.

- metric_value_var:

  Column in `data` containing the exposure metric values (dependent
  variable). Accepts bare names or strings. Default is `PPORRES`.

- dose_var:

  Column in `data` containing the dose (independent variable). Accepts
  bare names or strings. Default is `DOSE`.

- method:

  character string specifying the distribution to be used to derive the
  confidence interval. Options are `"normal"` (default) and `"tdist"`.

- ci:

  confidence interval to be calculated. Options `0.90` (default) and
  `0.95`.

- sigdigits:

  number of significant digits for rounding.

## Value

A `doseprop_stats` container (subclass of `pmx_stats`) with three slots:

- `stats`:

  One row per metric and columns `Intercept`, `StandardError`, `CI`,
  `Power`, `LCL`, `UCL`, `Proportional`, `PowerCI`, `Interpretation`,
  plus the column named in `metric_name_var`.

- `obs`:

  The filtered observation rows used for the plot scatter overlay.

- `config`:

  Named list with `metric_name_var`, `metric_value_var`, `dose_var`,
  `ci`, `method`.

Pass directly to
[`plot_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_doseprop.md)
or
[`plot_build_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_doseprop.md)
to replot without refitting.

## See also

Other dose proportionality:
[`is_doseprop_stats()`](https://ryancrass.github.io/pmxhelpr/reference/is_doseprop_stats.md),
[`plot_build_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_doseprop.md),
[`plot_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_doseprop.md),
[`plot_doseprop_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_doseprop_theme.md)

## Examples

``` r
df_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"))
#> <doseprop_stats>
#>   stats: 2 rows x 10 columns
#>   obs:   72 rows
#>   config: metric_name_var = PPTESTCD, metric_value_var = PPORRES, dose_var = DOSE, ci = 0.9, method = normal
#> 
#>   stats body:
#>   Intercept StandardError  CI Power   LCL  UCL Proportional
#> 1      4.04        0.0663 90% 0.997 0.888 1.11         TRUE
#> 2      1.09        0.0616 90% 1.070 0.967 1.17         TRUE
#>                            PowerCI    Interpretation   PPTESTCD
#> 1 Power: 0.997 (90% CI 0.888-1.11) Dose-proportional aucinf.obs
#> 2  Power: 1.07 (90% CI 0.967-1.17) Dose-proportional       cmax
#> 
#>   Use `x$obs` for the observation overlay.
```
