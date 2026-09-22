# Internal helper: legend-title theme elements from a style_spec

[`plot_vpc_legend()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_legend.md)
draws a standalone legend panel on
[`theme_void()`](https://ggplot2.tidyverse.org/reference/ggtheme.html),
so the style's `theme` field does not apply. The two legend-title fields
of
[`ggstylekit::style_spec()`](https://rdrr.io/pkg/ggstylekit/man/style_spec.html)
are mirrored here the way `ggstylekit` applies them to a plot, so the
legend and its plot agree.

## Usage

``` r
legend_style_theme(style)
```

## Arguments

- style:

  A `ggstylekit_style_spec`.

## Value

A ggplot2 theme carrying `legend.title.position` and/or the
`legend.title` `hjust`; empty when neither field is set.
