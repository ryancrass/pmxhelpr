# GOF layer visibility settings

Constructor for the `shown` argument of
[`plot_gof()`](https://ryancrass.github.io/pmxhelpr/reference/plot_gof.md),
controlling which GOF overlay layers are built. Call with no arguments
to view the defaults; pass `TRUE`/`FALSE` overrides to customize. Each
key names a series of the plot: its color is the `style_gof(colors = )`
entry of the same name in upper case, which is also its legend label.

## Usage

``` r
plot_gof_shown(obs = NULL, dv = NULL, pred = NULL, ipred = NULL)
```

## Arguments

- obs:

  Show observed data points and lines (the `OBS` series). Default is
  `TRUE`.

- dv:

  Show DV central tendency (the `DV` series). Default is `TRUE`.

- pred:

  Show PRED central tendency (the `PRED` series). Default is `TRUE`.

- ipred:

  Show IPRED central tendency (the `IPRED` series). Default is `TRUE`.

## Value

A `gof_shown` object: a named list of `TRUE`/`FALSE`.

## See also

Other goodness-of-fit:
[`plot_gof()`](https://ryancrass.github.io/pmxhelpr/reference/plot_gof.md),
[`style_gof()`](https://ryancrass.github.io/pmxhelpr/reference/style_gof.md)

## Examples

``` r
plot_gof_shown()
#> <gof_shown>
#>   obs    TRUE
#>   dv     TRUE
#>   pred   TRUE
#>   ipred  TRUE
plot_gof_shown(pred = FALSE)
#> <gof_shown>
#>   obs    TRUE
#>   dv     TRUE
#>   pred   FALSE
#>   ipred  TRUE
```
