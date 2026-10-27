# PMXForest-style forest plot style (`plot_forest`)

An alternative
[`ggstylekit::style_spec()`](https://rdrr.io/pkg/ggstylekit/man/style_spec.html)
for
[`plot_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_forest.md)
/
[`plot_build_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_forest.md)
that reproduces the default look of the `forestPlot()` function of
Pharmetheus' [PMXForest](https://github.com/pharmetheus/PMXForest/)
package, the forest plot of the FREM workflow: black points (shape 16,
size 2.5) on uncapped black interval lines, a thick dotted black
reference line, a `lightgrey` equivalence band at alpha 0.7 (PMXForest's
0.4, raised so the band stands out from the grey panel), and the ggplot2
default
[`theme_grey()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
panel (grey background, white gridlines on both axes, grey strips) that
PMXForest inherits.

## Usage

``` r
style_forest_pmxforest(...)
```

## Arguments

- ...:

  Fields passed to
  [`ggstylekit::style_spec()`](https://rdrr.io/pkg/ggstylekit/man/style_spec.html),
  overriding the preset defaults; call the preset with no arguments to
  view them. Per-series maps merge entry-wise onto the defaults (e.g.
  `colors = c(point = "navy")` leaves the other roles black); `colors`
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
layout, so it differs from PMXForest in that: covariate strips sit on
the left rather than the right; there are no minor gridlines and no
legend (PMXForest keys every layer in a legend above the plot); the band
is drawn only when `ref_band` is supplied (PMXForest shows 0.8-1.25 by
default); the estimate column is `est (lo, hi)` axis text under an
`Estimate (CI)` header rather than a separate `est [lo-hi]` "Statistics"
panel; no reference-subject footnote is added; reference rows are drawn
as points like the test rows but without an interval; and the x-axis is
labeled with the metric.

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
[`style_forest_pumas()`](https://ryancrass.github.io/pmxhelpr/reference/style_forest_pumas.md)

## Examples

``` r
style_forest_pmxforest()
#> <ggstylekit_style_spec>
#>   colors                 black, black, black, black
#>   fill                   lightgrey
#>   linetypes              solid , dotted
#>   alphas                 1.0, 1.0, 1.0, 1.0, 0.7
#>   shapes                 16, 16
#>   sizes                  2.5, 2.5
#>   linewidths             0.7, 1.0
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
style_forest_pmxforest(alphas = c(ref_band = 1))
#> <ggstylekit_style_spec>
#>   colors                 black, black, black, black
#>   fill                   lightgrey
#>   linetypes              solid , dotted
#>   alphas                 1, 1, 1, 1, 1
#>   shapes                 16, 16
#>   sizes                  2.5, 2.5
#>   linewidths             0.7, 1.0
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
style_forest_pmxforest(logx = TRUE)
#> <ggstylekit_style_spec>
#>   colors                 black, black, black, black
#>   fill                   lightgrey
#>   linetypes              solid , dotted
#>   alphas                 1.0, 1.0, 1.0, 1.0, 0.7
#>   shapes                 16, 16
#>   sizes                  2.5, 2.5
#>   linewidths             0.7, 1.0
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
