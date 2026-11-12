# Internal helper: add central tendency layers (style pattern)

Central-tendency points, lines, and error bars are `stat_summary` layers
tagged as the `"cent_point"`/`"cent_line"`/`"cent_errorbar"` series for
[`ggstylekit::style_plot()`](https://rdrr.io/pkg/ggstylekit/man/style_plot.html),
which fills their role-keyed fixed aesthetics. Error bars are
ggstylekit's errorbar entity (`GeomErrorbar`/`GeomLinerange`), so the
cap width comes from the style's `errorbar_width` field (see
[`style_errorbar_width()`](https://ryancrass.github.io/pmxhelpr/reference/style_errorbar_width.md)).
Color is inherited from the plot's global
[`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) when mapped,
or mapped to a literal label via `color_aes` (as
[`plot_gof()`](https://ryancrass.github.io/pmxhelpr/reference/plot_gof.md)
does for `"DV"`/`"IPRED"`/`"PRED"`, keying its manual overlay legend).

## Usage

``` r
add_cent_layers_style(
  plot,
  cent,
  y_var,
  color_aes = NULL,
  show_errorbars = TRUE
)
```

## Arguments

- plot:

  ggplot object.

- cent:

  Central tendency measure (see
  [`plot_dvtime()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvtime.md)).

- y_var:

  Y variable name (e.g. `"DV"`).

- color_aes:

  Literal color label, or `NULL` to inherit color.

- show_errorbars:

  Logical; add the error bar layers for `cent`.

## Value

Modified ggplot object.
