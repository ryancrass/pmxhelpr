# Summary method for `doseprop_stats`

Compact summary of a
[`df_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/df_doseprop.md)
result. Printing it shows the same header and configuration values as
[`print.doseprop_stats()`](https://ryancrass.github.io/pmxhelpr/reference/print.doseprop_stats.md),
but the body is condensed to one line per metric using the `PowerCI` and
`Interpretation` columns. Suitable for vignette output and test
snapshots.

## Usage

``` r
# S3 method for class 'doseprop_stats'
summary(object, ...)

# S3 method for class 'summary.doseprop_stats'
print(x, ...)
```

## Arguments

- object:

  A `doseprop_stats` object.

- ...:

  Currently unused.

- x:

  A `summary.doseprop_stats` object.

## Value

A `summary.doseprop_stats` object (also `summary.pmx_stats`): a list
with `header` (the class tag, `dim` of `stats`, `obs_rows`, and the
`config` values `metric_name_var`, `metric_value_var`, `dose_var`, `ci`,
`method`) and `metrics`, a data.frame with one row per metric and
columns `metric`, `Power`, `LCL`, `UCL`, `PowerCI`, `Interpretation`.
