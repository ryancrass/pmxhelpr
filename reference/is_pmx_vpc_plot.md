# Test whether an object is a `pmx_vpc_plot`

Predicate that returns `TRUE` if `x` carries the `"pmx_vpc_plot"` class
— i.e., it is the ggplot returned by
[`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md)
or
[`plot_build_vpc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_vpc.md).

## Usage

``` r
is_pmx_vpc_plot(x)
```

## Arguments

- x:

  Object to test.

## Value

Logical scalar.

## See also

Other vpc:
[`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md),
[`is_vpc_stats()`](https://ryancrass.github.io/pmxhelpr/reference/is_vpc_stats.md),
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
p <- plot_vpc_cont(data = simout)
#> Warning: Multiple unique values of `CMT` detected in `data` after filtering to `EVID == 0`: 2, 3.
#> ℹ Functions assume a single observation type per call.
#> ℹ Pre-filter to a single observation compartment (e.g., `dplyr::filter(data, CMT == <n>)`) before passing to this function.
is_pmx_vpc_plot(p)         # TRUE
#> [1] TRUE
is_pmx_vpc_plot(simout)    # FALSE
#> [1] FALSE
```
