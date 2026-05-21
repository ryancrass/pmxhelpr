# Ribbon aesthetics

Constructor for ribbon layer aesthetics. Used for simulated prediction
interval and median CI ribbons in VPC plots.

## Usage

``` r
pmx_ribbon(
  fill = NULL,
  alpha = NULL,
  color = NULL,
  linetype = NULL,
  linewidth = NULL
)
```

## Arguments

- fill:

  Ribbon fill color. Default varies by ribbon type.

- alpha:

  Ribbon alpha. Default varies by ribbon type.

- color:

  Ribbon border color. Default varies by ribbon type.

- linetype:

  Ribbon border line type. Default varies by ribbon type.

- linewidth:

  Ribbon border line width. Default varies by ribbon type.

## Value

A `pmx_ribbon` element object

## See also

Other element constructors:
[`pmx_color()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_color.md),
[`pmx_errorbar()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_errorbar.md),
[`pmx_line()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_line.md),
[`pmx_point()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_point.md),
[`pmx_style()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_style.md),
[`pmx_trend()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_trend.md)

## Examples

``` r
pmx_ribbon(fill = "skyblue", alpha = 0.3)
#> <pmx_ribbon>
#>   fill = skyblue, alpha = 0.3
```
