# Print method for `doseprop_stats`

Focused summary of a
[`df_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/df_doseprop.md)
result: object dimensions, the regression configuration values
(`metric_name_var`, `metric_value_var`, `dose_var`, `ci`, `method`), the
number of observation rows attached for the plot scatter overlay, and
the per-metric stats body. Inspect the underlying frames directly via
`x$stats` and `x$obs`; inspect run config via `x$config`.

## Usage

``` r
# S3 method for class 'doseprop_stats'
print(x, ...)
```

## Arguments

- x:

  A `doseprop_stats` object.

- ...:

  Passed to the underlying
  [`print.data.frame()`](https://rdrr.io/r/base/print.dataframe.html)
  call for the body.

## Value

`invisible(x)`.
