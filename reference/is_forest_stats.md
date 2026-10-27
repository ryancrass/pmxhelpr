# Test whether an object is a `forest_stats` container

Predicate that returns `TRUE` if `x` carries the `"forest_stats"` class
– i.e., it is the container returned by
[`df_forest()`](https://ryancrass.github.io/pmxhelpr/reference/df_forest.md)
(also accepted by
[`plot_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_forest.md)
and
[`plot_build_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_forest.md)
on the precomputed-stats fast path).

## Usage

``` r
is_forest_stats(x, strict = FALSE)
```

## Arguments

- x:

  Object to test.

- strict:

  Logical. When `TRUE`, additionally runs
  [`validate_forest_stats()`](https://ryancrass.github.io/pmxhelpr/reference/validate_forest_stats.md)
  and returns `FALSE` on validation failure. Default `FALSE` (class-tag
  check only, cheap).

## Value

Logical scalar.

## See also

Other forest plot:
[`df_forest()`](https://ryancrass.github.io/pmxhelpr/reference/df_forest.md),
[`df_forest_preprocess()`](https://ryancrass.github.io/pmxhelpr/reference/df_forest_preprocess.md),
[`plot_build_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_forest.md),
[`plot_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_forest.md),
[`style_forest()`](https://ryancrass.github.io/pmxhelpr/reference/style_forest.md),
[`style_forest_coveffectsplot()`](https://ryancrass.github.io/pmxhelpr/reference/style_forest_coveffectsplot.md),
[`style_forest_pmforest()`](https://ryancrass.github.io/pmxhelpr/reference/style_forest_pmforest.md),
[`style_forest_pmxforest()`](https://ryancrass.github.io/pmxhelpr/reference/style_forest_pmxforest.md),
[`style_forest_pumas()`](https://ryancrass.github.io/pmxhelpr/reference/style_forest_pumas.md)

## Examples

``` r
set.seed(1)
draws <- expand.grid(
  METRIC = c("AUC", "CMAX"),
  COV = c("WTBL", "AGE"),
  COVLVL = c("Low", "High"),
  REP = 1:50,
  stringsAsFactors = FALSE
)
draws$VALUE <- rlnorm(nrow(draws), 0, 0.2)
stats <- df_forest(
  draws,
  metric_name_var = "METRIC",
  cov_name_var = "COV",
  cov_level_var = "COVLVL",
  metric_value_var = "VALUE",
  cov_name_ref = NULL
)
is_forest_stats(stats)                  # TRUE
#> [1] TRUE
is_forest_stats(as.data.frame(stats))   # FALSE -- coerced to plain frame
#> [1] FALSE
```
