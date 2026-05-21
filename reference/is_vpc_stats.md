# Test whether an object is a `vpc_stats` container

Predicate that returns `TRUE` if `x` carries the `"vpc_stats"` class —
i.e., it is the container returned by
[`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)
(also accepted by
[`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md)
on the precomputed-stats fast path).

## Usage

``` r
is_vpc_stats(x, strict = FALSE)
```

## Arguments

- x:

  Object to test.

- strict:

  Logical. When `TRUE`, additionally runs
  [`validate_vpc_stats()`](https://ryancrass.github.io/pmxhelpr/reference/validate_vpc_stats.md)
  and returns `FALSE` on validation failure. Default `FALSE` (class-tag
  check only, cheap).

## Value

Logical scalar.

## See also

Other vpc:
[`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md),
[`is_pmx_vpc_plot()`](https://ryancrass.github.io/pmxhelpr/reference/is_pmx_vpc_plot.md),
[`plot_build_vpc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_vpc.md),
[`plot_vpc_cens()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cens.md),
[`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md),
[`plot_vpc_legend()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_legend.md),
[`plot_vpc_shown()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_shown.md),
[`plot_vpc_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_theme.md)

## Examples

``` r
model <- model_mread_load(model = "pkmodel")
#> Loading model from cache.
simout <- df_mrgsim_replicate(data = data_sad, model = model, replicates = 5,
                              dv_var = ODV)
out <- df_vpcstats(simout)
#> Warning: Multiple unique values of `CMT` detected in `data` after filtering to `EVID == 0`: 2, 3.
#> ℹ Functions assume a single observation type per call.
#> ℹ Pre-filter to a single observation compartment (e.g., `dplyr::filter(data, CMT == <n>)`) before passing to this function.
is_vpc_stats(out)        # TRUE
#> [1] TRUE
is_vpc_stats(out$stats)  # FALSE — that's the inner data.frame
#> [1] FALSE
```
