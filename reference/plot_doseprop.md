# Plot a dose-proportionality assessment via power law (log-log) regression

Dual-mode wrapper that delegates to
[`df_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/df_doseprop.md)
for computation and
[`plot_build_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_doseprop.md)
for rendering. Accepts either:

- raw observation data (e.g. PKNCA output) plus a `metrics` vector — the
  common one-shot mode; or

- a precomputed `doseprop_stats` object returned by
  [`df_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/df_doseprop.md)
  — skip the regression refit and replot with different `style` / `se`
  settings.

On the precomputed path, pipeline arguments (`metrics`,
`metric_name_var`, `metric_value_var`, `dose_var`, `method`, `ci`,
`sigdigits`) cannot be honored because the regression does not run again
— passing any of them aborts with a message pointing the caller at
[`df_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/df_doseprop.md).
Only `style` and `se` are accepted on both paths.

## Usage

``` r
plot_doseprop(
  data,
  metrics = NULL,
  metric_name_var = "PPTESTCD",
  metric_value_var = "PPORRES",
  dose_var = "DOSE",
  method = "normal",
  ci = 0.9,
  sigdigits = 3,
  se = TRUE,
  style = NULL
)
```

## Arguments

- data:

  Either raw observation data (data.frame, default expected format is
  output from `PKNCA::pk.nca()`) or a `doseprop_stats` object returned
  by
  [`df_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/df_doseprop.md).

- metrics:

  character vector of exposure metrics in `data` to plot. Required on
  the raw-data path; must not be passed with a `doseprop_stats` object.

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

- se:

  logical to display confidence interval around regression. Default is
  `TRUE`.

- style:

  A
  [`ggstylekit::style_spec()`](https://rdrr.io/pkg/ggstylekit/man/style_spec.html)
  controlling plot aesthetics. Defaults to
  [`style_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/style_doseprop.md);
  view the defaults by running
  [`style_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/style_doseprop.md)
  with no arguments. Customize by passing `style = style_doseprop(...)`.
  The per-metric facet layout is taken from the style's `facet_scales`
  (default `"free"`), `facet_nrow`, and `facet_ncol`; the `facet`,
  `logx`, and `logy` fields are ignored because the builder sets the
  facet and log-log axes itself.

## Value

a `ggplot` plot object

## See also

Other dose proportionality:
[`df_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/df_doseprop.md),
[`is_doseprop_stats()`](https://ryancrass.github.io/pmxhelpr/reference/is_doseprop_stats.md),
[`plot_build_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_doseprop.md),
[`style_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/style_doseprop.md)

## Examples

``` r
# Raw-data path
plot_doseprop(dplyr::filter(data_sad_nca, PART == "Part 1-SAD"),
               metrics = c("aucinf.obs", "cmax"))


# Precomputed path: compute once, replot many times
stats <- df_doseprop(dplyr::filter(data_sad_nca, PART == "Part 1-SAD"),
                      metrics = c("aucinf.obs", "cmax"))
plot_doseprop(stats)

plot_doseprop(stats, se = FALSE)
```
