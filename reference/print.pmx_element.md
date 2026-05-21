# Print method for pmx theme elements

Compact REPL display for any object inheriting `"pmx_element"` – the
values returned by
[`pmx_point()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_point.md),
[`pmx_line()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_line.md),
[`pmx_ribbon()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_ribbon.md),
[`pmx_errorbar()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_errorbar.md),
[`pmx_trend()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_trend.md),
[`pmx_style()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_style.md),
and
[`pmx_color()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_color.md).
Shows the element type as a banner and the set fields inline as
`name = value, name = value`.

## Usage

``` r
# S3 method for class 'pmx_element'
print(x, ...)
```

## Arguments

- x:

  A pmx element object.

- ...:

  Currently unused.

## Value

`invisible(x)`.
