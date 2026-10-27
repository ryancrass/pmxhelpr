# Print method for `forest_stats`

Focused summary of a
[`df_forest()`](https://ryancrass.github.io/pmxhelpr/reference/df_forest.md)
result: object dimensions, the configuration values (`metric_name_var`,
`cov_name_var`, `cov_level_var`, `ci`, `sigdigits`, `cov_name_ref`,
`cov_level_ref`), and the first rows of the stats frame. Inspect the
full frame via `x$stats`; inspect run config via `x$config`.

## Usage

``` r
# S3 method for class 'forest_stats'
print(x, n_head = 3, ...)
```

## Arguments

- x:

  A `forest_stats` object.

- n_head:

  Number of rows of `x$stats` to preview. Default `3`; `0` omits the
  preview.

- ...:

  Passed to the underlying
  [`print.data.frame()`](https://rdrr.io/r/base/print.dataframe.html)
  call for the preview.

## Value

`invisible(x)`.
