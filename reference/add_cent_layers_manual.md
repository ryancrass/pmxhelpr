# Add central tendency layers for GOF overlay plots

Use this variant for GOF-style overlay plots where multiple cent layers
with different y-vars (DV / PRED / IPRED) share a single manual color
legend. The layer's `color` is set to the literal string `color_aes`
inside [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) so
ggplot treats it as a discrete level, then
[`scale_color_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)
(added by the calling plot function) maps each level to a color. Fixed
theme colors are never applied.

## Usage

``` r
add_cent_layers_manual(
  plot,
  cent,
  y_var,
  point_el,
  line_el,
  eb_el,
  width,
  color_aes,
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

- color_aes:

  String label for color aesthetic (e.g., `"DV"`, `"PRED"`).

- show_errorbars:

  Logical indicating whether to add error bar layers.

## Value

A modified ggplot object with central tendency layers added

## Details

For single-y plots where color is inherited from a global `col_var`
aesthetic or fixed from the theme, use
[`add_cent_layers()`](https://ryancrass.github.io/pmxhelpr/reference/add_cent_layers.md).
The two helpers implement different
[`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) contracts
and intentionally are not unified.
