# Internal helper: add a trend line layer (style pattern)

Adds a `geom_smooth` for `method`, tagged as the `series` name
(`"loess"` or `"linear"`) so
[`ggstylekit::style_plot()`](https://rdrr.io/pkg/ggstylekit/man/style_plot.html)
fills its color, linewidth, linetype, SE-ribbon fill (base `line_fill`),
and SE-ribbon alpha (the series `alphas` entry). When `col_trend` is
`TRUE` and a color variable is supplied, color and fill are mapped to it
instead.

## Usage

``` r
add_trend_layers_style(
  plot,
  method,
  show,
  se,
  series,
  col_var_str,
  col_trend,
  ...
)
```

## Arguments

- plot:

  ggplot object.

- method:

  Smoothing method (`"loess"` or `"lm"`).

- show:

  Logical; whether to add the layer.

- se:

  Logical; whether to draw the SE ribbon.

- series:

  Series/role name for the style (e.g. `"loess"`, `"linear"`).

- col_var_str:

  Color variable name, or `NULL`.

- col_trend:

  Logical; stratify the trend by `col_var_str`.

- ...:

  Passed to
  [`ggplot2::geom_smooth()`](https://ggplot2.tidyverse.org/reference/geom_smooth.html)
  (e.g. `formula`, `level`, `span`).

## Value

Modified ggplot object.
