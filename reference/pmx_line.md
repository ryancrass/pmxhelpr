# Line aesthetics

Constructor for line layer aesthetics. Used for trend lines, reference
lines, spaghetti lines, and central tendency lines across all plot
types.

## Usage

``` r
pmx_line(linewidth = NULL, linetype = NULL, alpha = NULL, color = NULL)
```

## Arguments

- linewidth:

  Line width. Default varies by plot type.

- linetype:

  Line type. Default varies by plot type.

- alpha:

  Line alpha. Default varies by plot type.

- color:

  Line color. Default varies by plot type.

## Value

A `pmx_line` element object

## See also

Other element constructors:
[`pmx_color()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_color.md),
[`pmx_errorbar()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_errorbar.md),
[`pmx_point()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_point.md),
[`pmx_ribbon()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_ribbon.md),
[`pmx_style()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_style.md),
[`pmx_trend()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_trend.md)

## Examples

``` r
pmx_line(linewidth = 1, linetype = "dashed", color = "red")
#> <pmx_line>
#>   linewidth = 1, linetype = dashed, color = red
```
