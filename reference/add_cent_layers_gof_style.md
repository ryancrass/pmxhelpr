# Internal helper: central tendency layers for a GOF overlay (style pattern)

Like
[`add_cent_layers_style()`](https://ryancrass.github.io/pmxhelpr/reference/add_cent_layers_style.md)
but for the GOF overlay: color is mapped to a literal label
(`"DV"`/`"PRED"`/`"IPRED"`) for the manual color legend. The point,
line, and error bar layers are tagged with
[`ggstylekit::series_layer()`](https://rdrr.io/pkg/ggstylekit/man/series_layer.html)
so
[`ggstylekit::style_plot()`](https://rdrr.io/pkg/ggstylekit/man/style_plot.html)
fills their role-keyed fixed aesthetics while leaving the mapped color
channel to the caller's scale.

## Usage

``` r
add_cent_layers_gof_style(plot, cent, y_var, color_aes, show_errorbars = TRUE)
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

  Literal color label.

- show_errorbars:

  Logical; add error bars (DV only in GOF).

## Value

Modified ggplot object.
