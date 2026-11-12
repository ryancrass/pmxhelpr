# Summary method for `vpc_stats`

Summarises a
[`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)
result: the run configuration and the column groups present in `stats`.
Printing it shows the same content as
[`print.vpc_stats()`](https://ryancrass.github.io/pmxhelpr/reference/print.vpc_stats.md)
without the head preview, which suits vignette output and test
snapshots.

## Usage

``` r
# S3 method for class 'vpc_stats'
summary(object, ...)

# S3 method for class 'summary.vpc_stats'
print(x, ...)
```

## Arguments

- object:

  A `vpc_stats` object.

- ...:

  Currently unused.

- x:

  A `summary.vpc_stats` object.

## Value

A `summary.vpc_stats` object (also `summary.pmx_stats`): a list with
`header` (the class tag, `dim` of `stats`, `obs_rows`, and the `config`
values `n_replicates`, `ci`, `pi`, `mode`, `loq`, `strat_var`) and
`column_groups` (a named list of the `stats` columns in each group:
`identifiers`, `counts`, `sim_blq`, `std_observed`, `std_simulated`,
`pc_observed`, `pc_simulated`, `metadata`; empty groups are
`character(0)`).
