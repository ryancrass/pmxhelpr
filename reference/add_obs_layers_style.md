# Internal helper: add observed points and spaghetti lines (style pattern)

Color may be inherited from the plot's global
[`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) (as
[`plot_dvtime()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvtime.md)
does, leaving `col_var_str = NULL`) or mapped at the layer level via
`col_var_str` (as
[`plot_dvconc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvconc.md)
does, so only the points are colored while trend lines stay
unstratified). The `obs_point`/`obs_line` series aesthetics are filled
by
[`ggstylekit::style_plot()`](https://rdrr.io/pkg/ggstylekit/man/style_plot.html).

## Usage

``` r
add_obs_layers_style(plot, id_var_str, col_var_str = NULL)
```

## Arguments

- plot:

  ggplot object.

- id_var_str:

  Column name for spaghetti grouping, or `NULL` for no lines.

- col_var_str:

  Column name to map to color at the layer level, or `NULL` to inherit
  color from the plot's global mapping.

## Value

Modified ggplot object.
