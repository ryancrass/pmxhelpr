# Print method for `vpc_stats`

Focused summary of a
[`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)
result: object dimensions, run-config values (`n_replicates`, `loq`,
`strat_var`), the column groups present in `stats`, and a short head
preview. Inspect the underlying data.frames directly via `x$stats` and
`x$obs`; inspect run config via `x$config`.

## Usage

``` r
# S3 method for class 'vpc_stats'
print(x, n_head = 3, ...)
```

## Arguments

- x:

  A `vpc_stats` object.

- n_head:

  Integer. Number of rows of `stats` to preview.

- ...:

  Currently unused.

## Value

`invisible(x)`.
