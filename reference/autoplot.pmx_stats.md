# Draw a `pmx_stats` container with `autoplot()`

[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
methods for the cacheable stats containers. Each one calls the
container's builder with every other argument passed through:

- `vpc_stats`
  ([`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)):
  [`plot_build_vpc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_vpc.md),
  e.g. `autoplot(stats, type = "cens")` or
  `autoplot(stats, pcvpc = TRUE)`. `loq` still defaults from
  `stats$config$loq` unless passed.

- `doseprop_stats`
  ([`df_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/df_doseprop.md)):
  [`plot_build_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_doseprop.md).

- `forest_stats`
  ([`df_forest()`](https://ryancrass.github.io/pmxhelpr/reference/df_forest.md)):
  [`plot_build_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_forest.md);
  pass `metric` when the container holds several metrics.

Errors are reported by the builder (e.g.
`` Error in `plot_build_vpc()`  ``).
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html) is
re-exported from ggplot2, so it is available under
[`library(pmxhelpr)`](https://github.com/ryancrass/pmxhelpr) alone.

## Usage

``` r
# S3 method for class 'vpc_stats'
autoplot(object, ...)

# S3 method for class 'doseprop_stats'
autoplot(object, ...)

# S3 method for class 'forest_stats'
autoplot(object, ...)
```

## Arguments

- object:

  A `vpc_stats`, `doseprop_stats`, or `forest_stats` container.

- ...:

  Arguments passed to the builder.

## Value

The builder's plot: a `pmx_vpc_plot` for `vpc_stats`, a `ggplot`
otherwise.

## See also

[`plot_build_vpc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_vpc.md),
[`plot_build_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_doseprop.md),
[`plot_build_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_forest.md)

## Examples

``` r
model <- model_mread_load(model = "pkmodel")
#> Building pkmodel_cpp ... 
#> done.
simout <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT %in% c(1, 2)),
                              model = model, replicates = 5,
                              dv_var = ODV)
vs <- df_vpcstats(simout, loq = 1)
autoplot(vs)
#> Warning: Removed 5 rows containing missing values or values outside the scale range
#> (`geom_line()`).
#> Warning: Removed 8 rows containing missing values or values outside the scale range
#> (`geom_line()`).
#> Warning: Removed 3 rows containing missing values or values outside the scale range
#> (`geom_line()`).

autoplot(vs, type = "cens")

autoplot(df_forest(data_sad_pkforest), metric = "AUCRATIO")
```
