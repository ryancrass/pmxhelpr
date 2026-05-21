# GOF overlay color aesthetics

Creates a color element for
[`plot_gof_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_gof_theme.md)
controlling the manual color scale for DV, PRED, and IPRED overlay
lines.

## Usage

``` r
pmx_color(dv = NULL, pred = NULL, ipred = NULL)
```

## Arguments

- dv:

  Color for DV central tendency. Default `"blue"`.

- pred:

  Color for PRED central tendency. Default `"red"`.

- ipred:

  Color for IPRED central tendency. Default `"green"`.

## Value

A `pmx_color` element object

## See also

Other element constructors:
[`pmx_errorbar()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_errorbar.md),
[`pmx_line()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_line.md),
[`pmx_point()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_point.md),
[`pmx_ribbon()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_ribbon.md),
[`pmx_style()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_style.md),
[`pmx_trend()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_trend.md)

## Examples

``` r
pmx_color(dv = "black", pred = "purple", ipred = "darkgreen")
#> <pmx_color>
#>   dv = black, pred = purple, ipred = darkgreen
```
