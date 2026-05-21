# Add observed data layers for GOF plots with manual legend

Use this variant for GOF-style overlay plots where the obs layer
participates in a manual color legend alongside cent layers for DV /
PRED / IPRED. The layer's `color` is set to the literal string
`color_aes` inside
[`aes()`](https://ggplot2.tidyverse.org/reference/aes.html), which
ggplot treats as a discrete level that
[`scale_color_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)
then maps to a color.

## Usage

``` r
add_obs_layers_manual(plot, id_var_str, point_el, line_el, color_aes)
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

- color_aes:

  string label for color aesthetic (e.g., "OBS")

## Value

modified ggplot object

## Details

For column-mapped, inherited, or fixed-from-theme color, use
[`add_obs_layers()`](https://ryancrass.github.io/pmxhelpr/reference/add_obs_layers.md).
The two helpers implement different
[`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) contracts
and intentionally are not unified.
