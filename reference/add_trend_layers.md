# Internal helper: Add a trend line layer to a DV-vs-concentration plot

Adds a `geom_smooth` layer for the specified `method`. When
`col_var_str` is non-NULL and `col_trend` is TRUE, the color and fill
aesthetics are mapped to the grouping variable; otherwise, fixed colors
from `plottheme` are used.

## Usage

``` r
add_trend_layers(
  plot,
  method,
  show,
  se,
  plottheme,
  col_var_str,
  col_trend,
  ...,
  theme_key = method
)
```

## Arguments

- plot:

  ggplot object to modify

- method:

  Character smoothing method (`"loess"` or `"lm"`)

- show:

  Logical indicating whether to add this trend layer

- se:

  Logical indicating whether to show the standard error ribbon

- plottheme:

  Named list of theme aesthetics (must contain element named by
  `theme_key`)

- col_var_str:

  String name of the color variable, or `NULL`

- col_trend:

  Logical indicating if trends should be colored by group

- ...:

  Additional arguments passed to `geom_smooth` (e.g., `span`)

- theme_key:

  Character key to look up in `plottheme`. Defaults to `method`.

## Value

The (possibly modified) ggplot object
