# Internal helper: Add central tendency point, line, and error bar layers

Use this variant when color is either fixed (from theme) or inherited
from a global `col_var` aesthetic set in
[`init_plot()`](https://ryancrass.github.io/pmxhelpr/reference/init_plot.md).
The layers do not contribute to a `scale_color_manual` legend.

## Usage

``` r
add_cent_layers(
  plot,
  cent,
  y_var,
  point_el,
  line_el,
  eb_el,
  width,
  color_mapped = FALSE,
  show_errorbars = TRUE
)
```

## Arguments

- plot:

  ggplot object to modify.

- cent:

  Character string specifying the central tendency measure. One of
  `"mean"`, `"mean_sdl"`, `"mean_sdl_upper"`, `"median"`,
  `"median_iqr"`, or `"none"`.

- y_var:

  Character string specifying the y variable name (e.g., `"DV"`).

- point_el:

  A `pmx_point` element with point aesthetics.

- line_el:

  A `pmx_line` element with line aesthetics.

- eb_el:

  A `pmx_errorbar` element with error bar aesthetics.

- width:

  Numeric error bar cap width.

- color_mapped:

  Logical indicating whether color is mapped via a global aesthetic
  (e.g., `col_var`). When `TRUE`, fixed theme colors are suppressed to
  allow inheritance.

- show_errorbars:

  Logical indicating whether to add error bar layers.

## Value

A modified ggplot object with central tendency layers added

## Details

For GOF-style overlay plots — multiple cent layers with different y-vars
(DV / PRED / IPRED) sharing a manual color legend — use
[`add_cent_layers_manual()`](https://ryancrass.github.io/pmxhelpr/reference/add_cent_layers_manual.md)
instead. The two helpers implement different
[`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) contracts
(inherited/fixed color vs. literal-label color routed through
[`scale_color_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html))
and intentionally are not unified.
