# Internal helper: error bar layers for a central-tendency summary

Adds the `stat_summary` error bar layer(s) for `cent`, each tagged as
the `"cent_errorbar"` series so
[`ggstylekit::style_plot()`](https://rdrr.io/pkg/ggstylekit/man/style_plot.html)
fills their role-keyed fixed aesthetics and the style's `errorbar_width`
(see
[`style_errorbar_width()`](https://ryancrass.github.io/pmxhelpr/reference/style_errorbar_width.md)).
`"mean_sdl"` draws mean +/- SD, `"median_iqr"` the 25th-75th
percentiles, and `"mean_sdl_upper"` an upper-only bar (cap at mean + SD
plus a `geom_linerange` from the mean). Other `cent` values add nothing.

## Usage

``` r
add_errorbar_layers_style(plot, cent, mapping)
```

## Arguments

- plot:

  ggplot object.

- cent:

  Central tendency measure (see
  [`plot_dvtime()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvtime.md)).

- mapping:

  The [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) shared
  with the point/line summary layers.

## Value

Modified ggplot object.
