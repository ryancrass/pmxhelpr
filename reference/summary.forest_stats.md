# Summary method for `forest_stats`

Compact summary of a
[`df_forest()`](https://ryancrass.github.io/pmxhelpr/reference/df_forest.md)
result. Printing it shows the same header as
[`print.forest_stats()`](https://ryancrass.github.io/pmxhelpr/reference/print.forest_stats.md),
then one line per metric with its draw count `n` and each covariate's
levels in plot order. A covariate whose level repeats its name (the
reference panel) shows the name alone; reference copies drawn in a
covariate panel are marked `*`.

## Usage

``` r
# S3 method for class 'forest_stats'
summary(object, ...)

# S3 method for class 'summary.forest_stats'
print(x, ...)
```

## Arguments

- object:

  A `forest_stats` object.

- ...:

  Currently unused.

- x:

  A `summary.forest_stats` object.

## Value

A `summary.forest_stats` object (also `summary.pmx_stats`): a list with
`header` (the class tag, `dim` of `stats`, `obs_rows`, and the `config`
values shown by
[`print.forest_stats()`](https://ryancrass.github.io/pmxhelpr/reference/print.forest_stats.md)),
`metrics` (a data.frame with one row per metric and columns `metric`,
`n_min` and `n_max` (the range of `n`, `NA` when `stats` has no `n`
column), and `panels`, the covariate panels and levels in plot order as
printed), and `has_distributed` (whether any reference copy sits in a
covariate panel).
