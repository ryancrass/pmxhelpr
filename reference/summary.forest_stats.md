# Summary method for `forest_stats`

Compact summary of a
[`df_forest()`](https://ryancrass.github.io/pmxhelpr/reference/df_forest.md)
result: the same header shown by
[`print.forest_stats()`](https://ryancrass.github.io/pmxhelpr/reference/print.forest_stats.md),
then one line per metric with its draw count `n` and each covariate's
levels in plot order. A covariate whose level repeats its name (the
reference panel) shows the name alone; reference copies drawn in a
covariate panel are marked `*`.

## Usage

``` r
# S3 method for class 'forest_stats'
summary(object, ...)
```

## Arguments

- object:

  A `forest_stats` object.

- ...:

  Currently unused.

## Value

`invisible(object)`.
