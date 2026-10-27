# Internal helper: add LOQ reference line and BLQ caption (style pattern)

Draws the LLOQ line (suppressed under `dosenorm`) and appends the BLQ
imputation caption. The line's fixed aesthetics come from the
`"loq_line"` series of `style`. When `show_legend` is `TRUE`, the
linetype is mapped to a manual scale titled `"LLOQ"` and keyed by the
value so the line joins the legend; with `show_value = FALSE` the title
is dropped and the key is labeled `"LLOQ"`. `style_plot()` preserves
this manual scale.

## Usage

``` r
add_loq_layer_style(
  plot,
  caption,
  loq_method,
  loq,
  dosenorm,
  style,
  show_legend = FALSE,
  show_value = TRUE
)
```

## Arguments

- plot:

  ggplot object.

- caption:

  Current caption string.

- loq_method:

  Numeric BLQ method (0/1/2).

- loq:

  Numeric LLOQ value.

- dosenorm:

  Logical; whether dose normalization is active.

- style:

  A `ggstylekit_style_spec` supplying the `loq_line` aesthetics.

- show_legend:

  Logical; whether to add LLOQ to the linetype legend.

- show_value:

  Logical; whether the legend key shows the LLOQ value.

## Value

A named list with `plot` (modified ggplot) and `caption` (modified
string).
