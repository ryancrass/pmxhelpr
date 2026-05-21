# Error bar aesthetics

Error bar aesthetics

## Usage

``` r
pmx_errorbar(
  linewidth = NULL,
  linetype = NULL,
  alpha = NULL,
  color = NULL,
  width = NULL
)
```

## Arguments

- linewidth:

  Error bar line width. Default varies by plot type.

- linetype:

  Error bar line type. Default varies by plot type.

- alpha:

  Error bar alpha. Default varies by plot type.

- color:

  Error bar color. Default is `NULL` (inherits from ggplot2 default).

- width:

  Error bar cap width. Default is 2.5 percent of maximum `NTIME`.

## Value

A `pmx_errorbar` element object

## See also

Other element constructors:
[`pmx_color()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_color.md),
[`pmx_line()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_line.md),
[`pmx_point()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_point.md),
[`pmx_ribbon()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_ribbon.md),
[`pmx_style()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_style.md),
[`pmx_trend()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_trend.md)

## Examples

``` r
pmx_errorbar(linewidth = 0.5, width = 0.5)
#> <pmx_errorbar>
#>   linewidth = 0.5, width = 0.5
```
