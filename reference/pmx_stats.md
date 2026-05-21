# Construct a `pmx_stats` container

Constructor for the cacheable stats containers returned by
[`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)
and
[`df_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/df_doseprop.md).
Each container is a named list with three slots — `stats` (the per-row
summary frame), `obs` (the observation overlay used by plot builders,
may be `NULL`), and `config` (named list of run configuration consumed
by the plot builders). The class vector is `c(<subclass>, "pmx_stats")`.
The structural shape is enforced by
[`validate_pmx_stats()`](https://ryancrass.github.io/pmxhelpr/reference/validate_pmx_stats.md)
at construction time.

## Usage

``` r
pmx_stats(stats, obs = NULL, config = list(), subclass)
```

## Arguments

- stats:

  A `data.frame` of per-row summary statistics.

- obs:

  A `data.frame` of observation rows for the scatter overlay, or `NULL`
  when no overlay is used.

- config:

  A named `list` of run configuration (e.g. column names, CI width,
  replicate count).

- subclass:

  Character scalar naming the concrete subclass (e.g. `"vpc_stats"`,
  `"doseprop_stats"`).

## Value

An object of class `c(subclass, "pmx_stats")`.

## See also

Other pmx stats class:
[`is_pmx_stats()`](https://ryancrass.github.io/pmxhelpr/reference/is_pmx_stats.md)
