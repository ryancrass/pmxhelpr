# Test whether an object is a `doseprop_stats` container

Predicate that returns `TRUE` if `x` carries the `"doseprop_stats"`
class – i.e., it is the container returned by
[`df_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/df_doseprop.md)
(also accepted by
[`plot_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_doseprop.md)
and
[`plot_build_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_doseprop.md)
on the precomputed-stats fast path).

## Usage

``` r
is_doseprop_stats(x, strict = FALSE)
```

## Arguments

- x:

  Object to test.

- strict:

  Logical. When `TRUE`, additionally runs
  [`validate_doseprop_stats()`](https://ryancrass.github.io/pmxhelpr/reference/validate_doseprop_stats.md)
  and returns `FALSE` on validation failure. Default `FALSE` (class-tag
  check only, cheap).

## Value

Logical scalar.

## See also

Other dose proportionality:
[`df_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/df_doseprop.md),
[`plot_build_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_doseprop.md),
[`plot_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_doseprop.md),
[`plot_doseprop_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_doseprop_theme.md)

## Examples

``` r
stats <- df_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"))
is_doseprop_stats(stats)             # TRUE
#> [1] TRUE
is_doseprop_stats(as.data.frame(stats))  # FALSE -- coerced to plain frame
#> [1] FALSE
```
