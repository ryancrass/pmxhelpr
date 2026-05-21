# Add observed data point and spaghetti line layers to a plot

Adds a `geom_point` layer for observed data (visibility controlled by
theme alpha) and optionally a `geom_line` layer connecting observations
within groups when `id_var_str` is specified. Color is mapped to a data
column when `col_var_str` is provided.

## Usage

``` r
add_obs_layers(plot, id_var_str, point_el, line_el, col_var_str = NULL)
```

## Arguments

- plot:

  ggplot object

- id_var_str:

  character column name for spaghetti line grouping, or `NULL` for no
  lines

- point_el:

  A `pmx_point` element with point aesthetics.

- line_el:

  A `pmx_line` element with line aesthetics.

- col_var_str:

  optional string column name for color aesthetic, or `NULL`

## Value

modified ggplot object

## Details

Use this variant when color is column-mapped, inherited from a global
aesthetic, or fixed from the theme. For GOF-style overlay plots that
route color through
[`scale_color_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)
via a literal label, use
[`add_obs_layers_manual()`](https://ryancrass.github.io/pmxhelpr/reference/add_obs_layers_manual.md)
instead. The two helpers implement different
[`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) contracts
and intentionally are not unified.
