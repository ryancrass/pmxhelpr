# Internal helper: add observed points and spaghetti lines (style pattern)

Color is inherited from the plot's global
[`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) (as
[`plot_dvtime()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvtime.md)
does), mapped at the layer level to a column via `col_var_str` (as
[`plot_dvconc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvconc.md)
does, so only the points are colored while trend lines stay
unstratified), or mapped to a literal label via `color_aes` (as
[`plot_gof()`](https://ryancrass.github.io/pmxhelpr/reference/plot_gof.md)
does, keying its manual overlay legend, e.g. `"OBS"`). The
`obs_point`/`obs_line` series aesthetics are filled by
[`ggstylekit::style_plot()`](https://rdrr.io/pkg/ggstylekit/man/style_plot.html),
which leaves a mapped color channel to the plot's scale.

## Usage

``` r
add_obs_layers_style(
  plot,
  id_var_str,
  col_var_str = NULL,
  show_point = TRUE,
  color_aes = NULL
)
```

## Arguments

- plot:

  ggplot object.

- id_var_str:

  Column name for spaghetti grouping, or `NULL` for no lines.

- col_var_str:

  Column name to map to color at the layer level, or `NULL`.

- show_point:

  Logical; whether to add the observed point layer. When `FALSE` the
  layer is omitted entirely, so it does not train the axes.

- color_aes:

  Literal color label, or `NULL`. Use instead of `col_var_str`, not with
  it.

## Value

Modified ggplot object.
