# Trend line aesthetics (dvconc loess/linear)

Trend line aesthetics (dvconc loess/linear)

## Usage

``` r
pmx_trend(
  linewidth = NULL,
  linetype = NULL,
  color = NULL,
  se_color = NULL,
  se_alpha = NULL
)
```

## Arguments

- linewidth:

  Line width. Default varies by trend type.

- linetype:

  Line type. Default varies by trend type.

- color:

  Line color. Default is `"black"`.

- se_color:

  Standard error ribbon color. Default is `"lightgrey"`.

- se_alpha:

  Standard error ribbon alpha. Default is 0.4.

## Value

A `pmx_trend` element object

## See also

Other element constructors:
[`pmx_color()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_color.md),
[`pmx_errorbar()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_errorbar.md),
[`pmx_line()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_line.md),
[`pmx_point()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_point.md),
[`pmx_ribbon()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_ribbon.md),
[`pmx_style()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_style.md)

## Examples

``` r
pmx_trend(linewidth = 1.2, color = "darkblue", se_color = "lightgrey")
#> <pmx_trend>
#>   linewidth = 1.2, color = darkblue, se_color = lightgrey
```
