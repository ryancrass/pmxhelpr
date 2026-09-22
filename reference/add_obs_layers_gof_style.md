# Internal helper: observed layers for a GOF overlay (style pattern)

GOF overlays route color through a manual scale keyed by a literal label
(`"OBS"`), so color is mapped via
[`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) while the
layer is tagged with
[`ggstylekit::series_layer()`](https://rdrr.io/pkg/ggstylekit/man/series_layer.html)
under its role name.
[`ggstylekit::style_plot()`](https://rdrr.io/pkg/ggstylekit/man/style_plot.html)
then fills the role-keyed fixed aesthetics
(shape/size/linewidth/linetype/ alpha) and skips the mapped color
channel, leaving it to the caller's
[`scale_color_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html).
Mirrors the single-series
[`add_obs_layers_style()`](https://ryancrass.github.io/pmxhelpr/reference/add_obs_layers_style.md)
but with a literal-label color contract.

## Usage

``` r
add_obs_layers_gof_style(plot, id_var_str, color_aes)
```

## Arguments

- plot:

  ggplot object.

- id_var_str:

  Column name for spaghetti grouping, or `NULL`.

- color_aes:

  Literal color label (e.g. `"OBS"`).

## Value

Modified ggplot object.
