# Compute Summary Statistics for Covariate Forest Plots

Summarizes replicate draws for each metric and covariate level into a
median and a `ci` percentile interval, with formatted labels, and
returns a `forest_stats` object for
[`plot_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_forest.md)
/
[`plot_build_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_forest.md).
Rows are in plot order, top to bottom; reorder `stats$stats` to change
it. One object draws either reference layout (see `ref_layout` in
[`plot_build_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_forest.md))
without re-aggregating.

## Usage

``` r
df_forest(
  data,
  metric_name_var = "metric",
  metric_value_var = "value",
  cov_name_var = "cov_name",
  cov_level_var = "cov_level",
  ci = 0.9,
  sigdigits = 3,
  cov_name_ref = "Reference",
  cov_level_ref = NULL
)
```

## Arguments

- data:

  Input data.frame. Replicate level long format dataset: one row per
  metric, covariate, covariate level, and replicate.

- metric_name_var:

  Column in `data` containing metric names. Accepts bare names or
  strings. Default is `metric`.

- metric_value_var:

  Column in `data` containing replicate draws of the test/reference
  comparison statistic. Accepts bare names or strings. Default `value`.

- cov_name_var:

  Column in `data` containing covariate variable names. (e.g.,
  `WTBL`,`FOOD`, etc). Accepts bare names or strings. Default is
  `cov_name`.

- cov_level_var:

  Column in `data` containing covariate levels (e.g., `70 kg`). Accepts
  bare names or strings. Default is `cov_level`.

- ci:

  Numeric scalar in `(0, 1)` specifying the central interval width
  (e.g., `0.9` for 90% CI).

- sigdigits:

  Number of significant digits used to format the label columns. Digits
  are counted on the median estimate of each metric, which fixes one
  count of decimal places for every value of that metric (`"1.00"`,
  `"0.95"`, `"2.32"` for a ratio; `"12500"`, `"9500"` for an exposure
  near 12500). Trailing zeros are kept. Default `3`.

- cov_name_ref:

  The value of `cov_name_var` that marks the reference rows, drawn in
  the reference panel at the top. Default `"Reference"`, as in
  [`df_forest_preprocess()`](https://ryancrass.github.io/pmxhelpr/reference/df_forest_preprocess.md)
  output; `NULL` for no reference row.

- cov_level_ref:

  Optional named vector of per-covariate reference labels, e.g.
  `c(FOOD = "Fasted", WTBL = "70 kg")`. When given, the reference row is
  also drawn inside each named covariate panel under that label, and
  this becomes the default `ref_layout`. Unnamed covariates show no
  reference row. Requires `cov_name_ref`. Default `NULL` (reference
  panel only).

## Value

A `forest_stats` container (subclass of `pmx_stats`) with three slots:

- `stats`:

  One row per plotted point, in display order, with character columns
  named in `metric_name_var` / `cov_name_var` / `cov_level_var`,
  canonical numeric columns `est` (median of the draws), `lo`, `hi` (the
  `(1 - ci) / 2` and `1 - (1 - ci) / 2` percentiles), the integer `n`
  (the number of non-`NA` draws behind the row), the character columns
  `est_label`, `lo_label`, and `hi_label` (those values formatted per
  `sigdigits`), `ci_label` (the three combined for tables, e.g.
  `"1.12 (0.95, 1.32)"`), the logical `is_ref`, and `layout`, the
  reference layout(s) a row draws in: `"both"` (covariate rows),
  `"consolidated"` (the reference panel), or `"distributed"` (the
  reference copies placed in each `cov_level_ref` covariate). The
  `cov_name_var` value drives the per-row facet strip; the
  `cov_level_var` value drives the y-axis tick label;
  [`plot_build_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_forest.md)
  assembles `est_label`, `lo_label`, and `hi_label` into the right-hand
  secondary y-axis text, so edit those columns (not `ci_label`) to
  change the plotted text.

- `obs`:

  `NULL`. No scatter overlay for the forest family.

- `config`:

  Named list with column names, `ci`, `sigdigits`, `cov_name_ref`, and
  `cov_level_ref`.

Pass directly to
[`plot_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_forest.md)
or
[`plot_build_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_forest.md)
to render.

## Reference rows

Reference rows are flagged `is_ref` and drawn through the `ref_point`
style role. With `cov_level_ref`, `stats$stats` holds both the
reference-panel rows and a copy in each named covariate panel, told
apart by the `layout` column. `cov_level_ref` names not found in
`cov_name_var`, or naming the reference itself, are ignored with a
warning; a label equal to an existing level of that covariate also
warns, since both rows would share it.

## Row order

Rows of `stats$stats` are sorted into display order, top to bottom:
metrics and covariates follow their factor levels when the input columns
are factors, otherwise their order of first appearance in `data`, with
the reference panel moved first. Within a covariate, factor levels of
`cov_level_var` win when they cover every label (including the
covariate's reference label); otherwise, if every label carries a
distinct number, labels sort ascending by it, sign included (`50 kg`
above `90 kg`); any other set puts the reference label on top followed
by the data order. The reference copies are placed by the same rules, so
the covariate rows keep one order in both layouts.

## See also

Other forest plot:
[`df_forest_preprocess()`](https://ryancrass.github.io/pmxhelpr/reference/df_forest_preprocess.md),
[`is_forest_stats()`](https://ryancrass.github.io/pmxhelpr/reference/is_forest_stats.md),
[`plot_build_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_forest.md),
[`plot_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_forest.md),
[`style_forest()`](https://ryancrass.github.io/pmxhelpr/reference/style_forest.md),
[`style_forest_coveffectsplot()`](https://ryancrass.github.io/pmxhelpr/reference/style_forest_coveffectsplot.md),
[`style_forest_pmforest()`](https://ryancrass.github.io/pmxhelpr/reference/style_forest_pmforest.md),
[`style_forest_pmxforest()`](https://ryancrass.github.io/pmxhelpr/reference/style_forest_pmxforest.md),
[`style_forest_pumas()`](https://ryancrass.github.io/pmxhelpr/reference/style_forest_pumas.md)

## Examples

``` r
# Aggregate replicate simulations to point estimate + 90% CI
df_forest(dplyr::filter(data_sad_pkforest, grepl("RATIO$", metric)))
#> <forest_stats>
#>   stats: 24 rows x 13 columns
#>   config: metric_name_var = metric, metric_value_var = value, cov_name_var = cov_name, cov_level_var = cov_level, ci = 0.9, sigdigits = 3, cov_name_ref = Reference, cov_level_ref = NULL
#> 
#>   head(stats, 3):
#>      metric  cov_name cov_level      est       lo       hi   n est_label
#> 1 CMAXRATIO Reference Reference 1.000000 1.000000 1.000000 500      1.00
#> 2 CMAXRATIO      FOOD       Fed 1.702998 1.444680 2.021474 500      1.70
#> 3 CMAXRATIO      WTBL     50 kg 1.297623 1.216627 1.388727 500      1.30
#>   lo_label hi_label          ci_label is_ref       layout
#> 1     1.00     1.00 1.00 (1.00, 1.00)   TRUE consolidated
#> 2     1.44     2.02 1.70 (1.44, 2.02)  FALSE         both
#> 3     1.22     1.39 1.30 (1.22, 1.39)  FALSE         both
#> 
#>   Use `x$stats` for the full data.frame.

# A reference named something other than "Reference"
renamed <- dplyr::mutate(
  data_sad_pkforest,
  cov_level = ifelse(cov_name == "Reference", "70 kg, fasted", cov_level),
  cov_name = ifelse(cov_name == "Reference", "Typical subject", cov_name)
)
df_forest(renamed, cov_name_ref = "Typical subject")
#> <forest_stats>
#>   stats: 48 rows x 13 columns
#>   config: metric_name_var = metric, metric_value_var = value, cov_name_var = cov_name, cov_level_var = cov_level, ci = 0.9, sigdigits = 3, cov_name_ref = Typical subject, cov_level_ref = NULL
#> 
#>   head(stats, 3):
#>   metric        cov_name     cov_level       est        lo       hi   n
#> 1   CMAX Typical subject 70 kg, fasted  861.3751  741.2410 1005.205 500
#> 2   CMAX            FOOD           Fed 1465.1054 1180.1763 1830.221 500
#> 3   CMAX            WTBL         50 kg 1118.5361  936.4438 1306.034 500
#>   est_label lo_label hi_label          ci_label is_ref       layout
#> 1       861      741     1005   861 (741, 1005)   TRUE consolidated
#> 2      1465     1180     1830 1465 (1180, 1830)  FALSE         both
#> 3      1119      936     1306  1119 (936, 1306)  FALSE         both
#> 
#>   Use `x$stats` for the full data.frame.

# Per-covariate reference labels (the dataset's attribute,
# c(FOOD = "Fasted", WTBL = "70 kg")): the reference row is drawn in each
# covariate panel
df_forest(
  dplyr::filter(data_sad_pkforest, metric == "AUCRATIO"),
  cov_level_ref = attr(data_sad_pkforest, "cov_level_ref")
)
#> <forest_stats>
#>   stats: 8 rows x 13 columns
#>   config: metric_name_var = metric, metric_value_var = value, cov_name_var = cov_name, cov_level_var = cov_level, ci = 0.9, sigdigits = 3, cov_name_ref = Reference, cov_level_ref = c(FOOD = "Fasted", WTBL = "70 kg")
#> 
#>   head(stats, 3):
#>     metric  cov_name cov_level      est       lo       hi   n est_label
#> 1 AUCRATIO Reference Reference 1.000000 1.000000 1.000000 500      1.00
#> 2 AUCRATIO      FOOD    Fasted 1.000000 1.000000 1.000000 500      1.00
#> 3 AUCRATIO      FOOD       Fed 2.316439 2.004453 2.648564 500      2.32
#>   lo_label hi_label          ci_label is_ref       layout
#> 1     1.00     1.00 1.00 (1.00, 1.00)   TRUE consolidated
#> 2     1.00     1.00 1.00 (1.00, 1.00)   TRUE  distributed
#> 3     2.00     2.65 2.32 (2.00, 2.65)  FALSE         both
#> 
#>   Use `x$stats` for the full data.frame.

# Control the row order with factor levels on the input
d <- dplyr::filter(data_sad_pkforest, metric == "AUCRATIO")
d$cov_level <- factor(d$cov_level, levels = c("Reference", "Fed", "Fasted",
                                          "50 kg", "70 kg", "90 kg"))
df_forest(d, cov_level_ref = attr(data_sad_pkforest, "cov_level_ref"))
#> <forest_stats>
#>   stats: 7 rows x 13 columns
#>   config: metric_name_var = metric, metric_value_var = value, cov_name_var = cov_name, cov_level_var = cov_level, ci = 0.9, sigdigits = 3, cov_name_ref = Reference, cov_level_ref = c(FOOD = "Fasted", WTBL = "70 kg")
#> 
#>   head(stats, 3):
#>     metric  cov_name cov_level      est       lo       hi   n est_label
#> 1 AUCRATIO Reference Reference 1.000000 1.000000 1.000000 500      1.00
#> 2 AUCRATIO      FOOD       Fed 2.316439 2.004453 2.648564 500      2.32
#> 3 AUCRATIO      FOOD    Fasted 1.000000 1.000000 1.000000 500      1.00
#>   lo_label hi_label          ci_label is_ref       layout
#> 1     1.00     1.00 1.00 (1.00, 1.00)   TRUE consolidated
#> 2     2.00     2.65 2.32 (2.00, 2.65)  FALSE         both
#> 3     1.00     1.00 1.00 (1.00, 1.00)   TRUE  distributed
#> 
#>   Use `x$stats` for the full data.frame.
```
