# GOF layer visibility settings

Constructor and factory for controlling which GOF overlay layers are
displayed. Call with no arguments to view defaults. Pass overrides to
customize.

## Usage

``` r
plot_gof_shown(obs = NULL, dv = NULL, pred = NULL, ipred = NULL)
```

## Arguments

- obs:

  Show observed data points/lines. Default is `TRUE`.

- dv:

  Show DV central tendency. Default is `TRUE`.

- pred:

  Show PRED central tendency. Default is `TRUE`.

- ipred:

  Show IPRED central tendency. Default is `TRUE`.

## Value

A named list of logicals

## See also

Other goodness-of-fit:
[`plot_gof()`](https://ryancrass.github.io/pmxhelpr/reference/plot_gof.md),
[`plot_gof_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_gof_theme.md)

## Examples

``` r
plot_gof_shown()
#> $obs
#> [1] TRUE
#> 
#> $dv
#> [1] TRUE
#> 
#> $pred
#> [1] TRUE
#> 
#> $ipred
#> [1] TRUE
#> 
plot_gof_shown(pred = FALSE)
#> $obs
#> [1] TRUE
#> 
#> $dv
#> [1] TRUE
#> 
#> $pred
#> [1] FALSE
#> 
#> $ipred
#> [1] TRUE
#> 
```
