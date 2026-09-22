# Internal helper: add central tendency layers (style pattern)

Central-tendency points, lines, and error bars are `stat_summary` layers
tagged as the `"cent_point"`/`"cent_line"`/`"cent_errorbar"` series for
[`ggstylekit::style_plot()`](https://rdrr.io/pkg/ggstylekit/man/style_plot.html),
which fills their role-keyed fixed aesthetics. Error bars are
ggstylekit's errorbar entity (`GeomErrorbar`/`GeomLinerange`), so the
cap width comes from the style's `errorbar_width` field (see
[`style_errorbar_width()`](https://ryancrass.github.io/pmxhelpr/reference/style_errorbar_width.md)).
Color is inherited from the plot's global
[`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) when mapped.

## Usage

``` r
add_cent_layers_style(plot, cent, y_var)
```

## Arguments

- plot:

  ggplot object.

- cent:

  Central tendency measure (see
  [`plot_dvtime()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvtime.md)).

- y_var:

  Y variable name (e.g. `"DV"`).

## Value

Modified ggplot object.
