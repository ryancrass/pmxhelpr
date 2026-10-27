# coveffectsplot-style forest plot style (`plot_forest`)

An alternative
[`ggstylekit::style_spec()`](https://rdrr.io/pkg/ggstylekit/man/style_spec.html)
for
[`plot_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_forest.md)
/
[`plot_build_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_forest.md)
that reproduces the default look of the `forest_plot()` function of the
[coveffectsplot](https://github.com/smouksassi/coveffectsplot/) package:
blue points (shape 16, size 4) on thick blue interval lines, blue
reference points, a thick dashed black reference line, a grey
equivalence band at alpha 80/255, grey major gridlines on both axes,
grey-filled covariate strips placed between the level labels and the
panels, separated panels, and a centered estimate column.

## Usage

``` r
style_forest_coveffectsplot(...)
```

## Arguments

- ...:

  Fields passed to
  [`ggstylekit::style_spec()`](https://rdrr.io/pkg/ggstylekit/man/style_spec.html),
  overriding the preset defaults; call the preset with no arguments to
  view them. Per-series maps merge entry-wise onto the defaults (e.g.
  `colors = c(point = "darkred")` leaves the other roles blue); `colors`
  entries keyed by covariate name opt into per-covariate coloring as in
  [`style_forest()`](https://ryancrass.github.io/pmxhelpr/reference/style_forest.md).
  All other fields replace their default wholesale.

## Value

A `ggstylekit_style_spec` object.

## Details

Series roles are those of
[`style_forest()`](https://ryancrass.github.io/pmxhelpr/reference/style_forest.md).
The plot keeps the
[`plot_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_forest.md)
layout, so it differs from coveffectsplot in that: text is scaled to an
11 pt base (coveffectsplot uses `base_size = 22`), with the covariate
strips raised to the axis text size for legibility; there are no minor
gridlines and no legend; the band is drawn only when `ref_band` is
supplied (coveffectsplot shows 0.8-1.25 by default); the estimate column
is `est (lo, hi)` axis text under an `Estimate (CI)` header rather than
a separate bordered table; reference rows show a point without an
interval; and the x-axis is labeled with the metric.

## See also

Other forest plot:
[`df_forest()`](https://ryancrass.github.io/pmxhelpr/reference/df_forest.md),
[`df_forest_preprocess()`](https://ryancrass.github.io/pmxhelpr/reference/df_forest_preprocess.md),
[`is_forest_stats()`](https://ryancrass.github.io/pmxhelpr/reference/is_forest_stats.md),
[`plot_build_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_forest.md),
[`plot_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_forest.md),
[`style_forest()`](https://ryancrass.github.io/pmxhelpr/reference/style_forest.md),
[`style_forest_pmforest()`](https://ryancrass.github.io/pmxhelpr/reference/style_forest_pmforest.md),
[`style_forest_pmxforest()`](https://ryancrass.github.io/pmxhelpr/reference/style_forest_pmxforest.md),
[`style_forest_pumas()`](https://ryancrass.github.io/pmxhelpr/reference/style_forest_pumas.md)

## Examples

``` r
style_forest_coveffectsplot()
#> <ggstylekit_style_spec>
#>   colors                 blue , blue , blue , black
#>   fill                   grey
#>   linetypes              solid , dashed
#>   alphas                 1.0000000, 1.0000000, 1.0000000, 1.0000000, 0.3137255
#>   shapes                 16, 16
#>   sizes                  4, 4
#>   linewidths             1, 1
#>   point_color            NULL
#>   point_alpha            NULL
#>   point_size             NULL
#>   point_shape            NULL
#>   line_color             NULL
#>   line_alpha             NULL
#>   line_linetype          NULL
#>   line_linewidth         NULL
#>   line_fill              NULL
#>   errorbar_color         NULL
#>   errorbar_alpha         NULL
#>   errorbar_linetype      NULL
#>   errorbar_linewidth     NULL
#>   errorbar_width         NULL
#>   errorbar_fill          NULL
#>   bar_fill               NULL
#>   bar_color              NULL
#>   bar_alpha              NULL
#>   bar_linewidth          NULL
#>   area_fill              NULL
#>   area_color             NULL
#>   area_alpha             NULL
#>   area_linewidth         NULL
#>   box_fill               NULL
#>   box_color              NULL
#>   box_alpha              NULL
#>   box_linewidth          NULL
#>   title                  NULL
#>   xlabel                 NULL
#>   ylabel                 NULL
#>   caption                NULL
#>   xlims                  NULL
#>   ylims                  NULL
#>   logx                   NULL
#>   logy                   NULL
#>   xbreaks                NULL
#>   ybreaks                NULL
#>   xminor_breaks          NULL
#>   yminor_breaks          NULL
#>   xtick_labels           NULL
#>   ytick_labels           NULL
#>   xorder                 NULL
#>   yorder                 NULL
#>   equal_axis             NULL
#>   legends                NULL
#>   legend.position        none
#>   legend.title.position  NULL
#>   legend_nrow            NULL
#>   legend_ncol            NULL
#>   legend.title.hjust     NULL
#>   caption_hjust          NULL
#>   fill_alpha             NULL
#>   facet                  NULL
#>   facet_scales           NULL
#>   facet_nrow             NULL
#>   facet_ncol             NULL
#>   theme                  <ggplot2 theme>
style_forest_coveffectsplot(colors = c(point = "darkred", errorbar = "darkred"))
#> <ggstylekit_style_spec>
#>   colors                 darkred, darkred, blue   , black  
#>   fill                   grey
#>   linetypes              solid , dashed
#>   alphas                 1.0000000, 1.0000000, 1.0000000, 1.0000000, 0.3137255
#>   shapes                 16, 16
#>   sizes                  4, 4
#>   linewidths             1, 1
#>   point_color            NULL
#>   point_alpha            NULL
#>   point_size             NULL
#>   point_shape            NULL
#>   line_color             NULL
#>   line_alpha             NULL
#>   line_linetype          NULL
#>   line_linewidth         NULL
#>   line_fill              NULL
#>   errorbar_color         NULL
#>   errorbar_alpha         NULL
#>   errorbar_linetype      NULL
#>   errorbar_linewidth     NULL
#>   errorbar_width         NULL
#>   errorbar_fill          NULL
#>   bar_fill               NULL
#>   bar_color              NULL
#>   bar_alpha              NULL
#>   bar_linewidth          NULL
#>   area_fill              NULL
#>   area_color             NULL
#>   area_alpha             NULL
#>   area_linewidth         NULL
#>   box_fill               NULL
#>   box_color              NULL
#>   box_alpha              NULL
#>   box_linewidth          NULL
#>   title                  NULL
#>   xlabel                 NULL
#>   ylabel                 NULL
#>   caption                NULL
#>   xlims                  NULL
#>   ylims                  NULL
#>   logx                   NULL
#>   logy                   NULL
#>   xbreaks                NULL
#>   ybreaks                NULL
#>   xminor_breaks          NULL
#>   yminor_breaks          NULL
#>   xtick_labels           NULL
#>   ytick_labels           NULL
#>   xorder                 NULL
#>   yorder                 NULL
#>   equal_axis             NULL
#>   legends                NULL
#>   legend.position        none
#>   legend.title.position  NULL
#>   legend_nrow            NULL
#>   legend_ncol            NULL
#>   legend.title.hjust     NULL
#>   caption_hjust          NULL
#>   fill_alpha             NULL
#>   facet                  NULL
#>   facet_scales           NULL
#>   facet_nrow             NULL
#>   facet_ncol             NULL
#>   theme                  <ggplot2 theme>
style_forest_coveffectsplot(logx = TRUE)
#> <ggstylekit_style_spec>
#>   colors                 blue , blue , blue , black
#>   fill                   grey
#>   linetypes              solid , dashed
#>   alphas                 1.0000000, 1.0000000, 1.0000000, 1.0000000, 0.3137255
#>   shapes                 16, 16
#>   sizes                  4, 4
#>   linewidths             1, 1
#>   point_color            NULL
#>   point_alpha            NULL
#>   point_size             NULL
#>   point_shape            NULL
#>   line_color             NULL
#>   line_alpha             NULL
#>   line_linetype          NULL
#>   line_linewidth         NULL
#>   line_fill              NULL
#>   errorbar_color         NULL
#>   errorbar_alpha         NULL
#>   errorbar_linetype      NULL
#>   errorbar_linewidth     NULL
#>   errorbar_width         NULL
#>   errorbar_fill          NULL
#>   bar_fill               NULL
#>   bar_color              NULL
#>   bar_alpha              NULL
#>   bar_linewidth          NULL
#>   area_fill              NULL
#>   area_color             NULL
#>   area_alpha             NULL
#>   area_linewidth         NULL
#>   box_fill               NULL
#>   box_color              NULL
#>   box_alpha              NULL
#>   box_linewidth          NULL
#>   title                  NULL
#>   xlabel                 NULL
#>   ylabel                 NULL
#>   caption                NULL
#>   xlims                  NULL
#>   ylims                  NULL
#>   logx                   TRUE
#>   logy                   NULL
#>   xbreaks                NULL
#>   ybreaks                NULL
#>   xminor_breaks          NULL
#>   yminor_breaks          NULL
#>   xtick_labels           NULL
#>   ytick_labels           NULL
#>   xorder                 NULL
#>   yorder                 NULL
#>   equal_axis             NULL
#>   legends                NULL
#>   legend.position        none
#>   legend.title.position  NULL
#>   legend_nrow            NULL
#>   legend_ncol            NULL
#>   legend.title.hjust     NULL
#>   caption_hjust          NULL
#>   fill_alpha             NULL
#>   facet                  NULL
#>   facet_scales           NULL
#>   facet_nrow             NULL
#>   facet_ncol             NULL
#>   theme                  <ggplot2 theme>
```
