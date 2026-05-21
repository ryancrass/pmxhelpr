# Internal helper: Compute error bar width

Returns the user-specified error bar width from the theme, or defaults
to 2.5 percent of the maximum `NTIME` value.

## Usage

``` r
errorbar_width(plottheme, data)
```

## Arguments

- plottheme:

  Named list of theme elements containing `$cent_errorbar$width`.

- data:

  data.frame containing `NTIME` column.

## Value

Numeric error bar width value

## Examples

``` r
theme <- plot_dvtime_theme()
data <- data.frame(NTIME = c(0, 1, 2, 4, 8, 24))
pmxhelpr:::errorbar_width(theme, data)
#> [1] 0.6
```
