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

  confidence interval to be calculated: a single numeric value in
  `(0, 1)`, e.g. `0.90` (default) or `0.95`.

- sigdigits:

  number of significant digits for the reported `Intercept`,
  `StandardError`, `Power`, `LCL`, `UCL`, and `PowerCI` columns.
  `PowerCI` pads every value to this many digits so the facet labels
  line up. The `Proportional` / `Interpretation` verdict is taken from
  the unrounded confidence bounds, so it never moves with `sigdigits`.

## Value

A `doseprop_stats` container (subclass of `pmx_stats`) with three slots:

- `stats`:

  One row per metric and columns `Intercept`, `StandardError`, `CI`,
  `Power`, `LCL`, `UCL`, `Proportional`, `PowerCI`, `Interpretation`,
  plus the column named in `metric_name_var`. `Interpretation` is
  `"Dose-proportional"` when the `ci` interval for `Power` contains 1,
  and less or greater than dose-proportional when it lies wholly below
  or above 1. It is a screening label: a CI that contains 1 does not
  establish proportionality, and no predefined equivalence bounds are
  applied.

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
[`style_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/style_doseprop.md)

## Examples

``` r
df_doseprop(dplyr::filter(data_sad_nca, PART == "Part 1-SAD"),
            metrics = c("aucinf.obs", "cmax"))
#> <doseprop_stats>
#>   stats: 2 rows x 10 columns
#>   obs:   60 rows
#>   config: metric_name_var = PPTESTCD, metric_value_var = PPORRES, dose_var = DOSE, ci = 0.9, method = normal
#> 
#>   stats body:
#>   Intercept StandardError  CI Power   LCL  UCL Proportional
#> 1      3.97        0.0437 90% 0.979 0.907 1.05         TRUE
#> 2      1.02        0.0597 90% 1.060 0.959 1.16         TRUE
#>                            PowerCI    Interpretation   PPTESTCD
#> 1 Power: 0.979 (90% CI 0.907-1.05) Dose-proportional aucinf.obs
#> 2  Power: 1.06 (90% CI 0.959-1.16) Dose-proportional       cmax
#> 
#>   Use `x$obs` for the observation overlay.
```
