# Summary method for `doseprop_stats`

Compact summary of a
[`df_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/df_doseprop.md)
result: the same header and configuration values shown by
[`print.doseprop_stats()`](https://ryancrass.github.io/pmxhelpr/reference/print.doseprop_stats.md),
but the body is condensed to one line per metric using the `PowerCI` and
`Interpretation` columns. Suitable for vignette output and test
snapshots.

## Usage

``` r
# S3 method for class 'doseprop_stats'
summary(object, ...)
```

## Arguments

- object:

  A `doseprop_stats` object.

- ...:

  Currently unused.

## Value

`invisible(object)`.
