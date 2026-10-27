# Pumas-style forest plot style (`plot_forest`)

An alternative
[`ggstylekit::style_spec()`](https://rdrr.io/pkg/ggstylekit/man/style_spec.html)
for
[`plot_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_forest.md)
/
[`plot_build_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_forest.md)
that reproduces the look of the `table_forest_plot()` function of Pumas;
see the [Pumas forest plot
tutorial](https://tutorials.pumas.ai/html/forest_plots/forest.html):
filled diamonds on thin capped error bars, coloured per covariate from
Makie's default Wong palette, a dashed black reference line, a light
grey equivalence band, and an open panel with no gridlines and only the
x-axis line.

## Usage

``` r
style_forest_pumas(...)
```

## Arguments

- ...:

  Fields passed to
  [`ggstylekit::style_spec()`](https://rdrr.io/pkg/ggstylekit/man/style_spec.html),
  overriding the preset defaults; call the preset with no arguments to
  view them. Per-series maps merge entry-wise onto the defaults (e.g.
  `sizes = c(point = 3)`). The default `colors` is a palette
  `function(n)`, so passing `colors` replaces it wholesale: a different
  palette function, or entries keyed by covariate name (covariates
  without an entry take the house palette). A `colors` naming only roles
  (e.g. `c(point = "red")`) turns covariate coloring off, with a
  warning. `errorbar_width` sets the cap height in row heights.

## Value

A `ggstylekit_style_spec` object.

## Details

Series roles are those of
[`style_forest()`](https://ryancrass.github.io/pmxhelpr/reference/style_forest.md).
The plot keeps the
[`plot_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_forest.md)
layout, so it differs from Pumas in that: the estimate column is on the
right as `est (lo, hi)` (Pumas puts a `Scenario` / `Median [90% CI]`
table on the left); covariate names head their rows as strips rather
than `COV (ref: X)` headers; the band is drawn only when `ref_band` is
supplied; and Pumas' optional density ridges are not drawn.

## See also

Other forest plot:
[`df_forest()`](https://ryancrass.github.io/pmxhelpr/reference/df_forest.md),
[`df_forest_preprocess()`](https://ryancrass.github.io/pmxhelpr/reference/df_forest_preprocess.md),
[`is_forest_stats()`](https://ryancrass.github.io/pmxhelpr/reference/is_forest_stats.md),
[`plot_build_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_forest.md),
[`plot_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_forest.md),
[`style_forest()`](https://ryancrass.github.io/pmxhelpr/reference/style_forest.md),
[`style_forest_coveffectsplot()`](https://ryancrass.github.io/pmxhelpr/reference/style_forest_coveffectsplot.md),
[`style_forest_pmforest()`](https://ryancrass.github.io/pmxhelpr/reference/style_forest_pmforest.md),
[`style_forest_pmxforest()`](https://ryancrass.github.io/pmxhelpr/reference/style_forest_pmxforest.md)

## Examples

``` r
style_forest_pumas()
#> <ggstylekit_style_spec>
#>   colors                 <palette function>
#>   fill                   grey50
#>   linetypes              solid , dashed
#>   alphas                 1.0, 1.0, 1.0, 1.0, 0.2
#>   shapes                 18, 18
#>   sizes                  3.5, 3.5
#>   linewidths             0.5, 0.6
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
#>   errorbar_width         0.3
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
style_forest_pumas(errorbar_width = 0.5)
#> <ggstylekit_style_spec>
#>   colors                 <palette function>
#>   fill                   grey50
#>   linetypes              solid , dashed
#>   alphas                 1.0, 1.0, 1.0, 1.0, 0.2
#>   shapes                 18, 18
#>   sizes                  3.5, 3.5
#>   linewidths             0.5, 0.6
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
#>   errorbar_width         0.5
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
style_forest_pumas(logx = TRUE)
#> <ggstylekit_style_spec>
#>   colors                 <palette function>
#>   fill                   grey50
#>   linetypes              solid , dashed
#>   alphas                 1.0, 1.0, 1.0, 1.0, 0.2
#>   shapes                 18, 18
#>   sizes                  3.5, 3.5
#>   linewidths             0.5, 0.6
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
#>   errorbar_width         0.3
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
