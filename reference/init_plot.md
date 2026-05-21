# Internal helper: Initialize a ggplot with standard theme

Creates a ggplot object with the base theme (`theme_bw`) and the unified
panel theme via
[`apply_panel_theme()`](https://ryancrass.github.io/pmxhelpr/reference/apply_panel_theme.md)
(continuous-family defaults: minor and major.x gridlines blanked).

## Usage

``` r
init_plot(data, x_var, y_var, col_var_str = NULL)
```

## Arguments

- data:

  data.frame to plot

- x_var:

  String name of the x-axis variable

- y_var:

  String name of the y-axis variable

- col_var_str:

  String name of the color variable, or `NULL`

## Value

A ggplot object with base theme applied
