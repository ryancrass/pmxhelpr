# Point aesthetics

Constructor for point layer aesthetics. Used for observed data points
and central tendency points across all plot types.

## Usage

``` r
pmx_point(shape = NULL, size = NULL, alpha = NULL, color = NULL)
```

## Arguments

- shape:

  Point shape. Default varies by plot type.

- size:

  Point size. Default varies by plot type.

- alpha:

  Point alpha. Default varies by plot type.

- color:

  Point color. Default varies by plot type.

## Value

A `pmx_point` element object

## See also

Other element constructors:
[`pmx_color()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_color.md),
[`pmx_errorbar()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_errorbar.md),
[`pmx_line()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_line.md),
[`pmx_ribbon()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_ribbon.md),
[`pmx_style()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_style.md),
[`pmx_trend()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_trend.md)

## Examples

``` r
pmx_point(shape = 1, size = 2, color = "blue")
#> <pmx_point>
#>   shape = 1, size = 2, color = blue
```
