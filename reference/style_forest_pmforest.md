# pmforest-style forest plot style (`plot_forest`)

An alternative
[`ggstylekit::style_spec()`](https://rdrr.io/pkg/ggstylekit/man/style_spec.html)
for
[`plot_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_forest.md)
/
[`plot_build_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_forest.md)
that reproduces the default look of Metrum Research Group's
[pmforest](https://github.com/metrumresearchgroup/pmforest/) package:
black-outlined filled diamonds (shape 23, size 3.5) and uncapped
interval lines coloured per covariate with the ggplot2 default hue
palette, a dot-dash reference line, a `grey35` equivalence band at alpha
0.2, and a
[`theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
panel with grey major vertical gridlines, ~9.9 pt text, a ~7.9 pt
estimate column, and no legend.

## Usage

``` r
style_forest_pmforest(...)
```

## Arguments

- ...:

  Fields passed to
  [`ggstylekit::style_spec()`](https://rdrr.io/pkg/ggstylekit/man/style_spec.html),
  overriding the preset defaults; call the preset with no arguments to
  view them. Per-series maps merge entry-wise onto the defaults (e.g.
  `sizes = c(point = 3)`). The default `colors` is a hue palette
  `function(n)`, so passing `colors` replaces it wholesale: a different
  palette function, or entries keyed by covariate name (covariates
  without an entry take the house palette). A `colors` naming only roles
  (e.g. `c(point = "red")`) turns covariate coloring off, with a
  warning. The point fill follows the covariate colour while the point
  shape is a fill shape (21-25); the `point` colour is the outline.

## Value

A `ggstylekit_style_spec` object.

## Details

Series roles are those of
[`style_forest()`](https://ryancrass.github.io/pmxhelpr/reference/style_forest.md).
The plot keeps the
[`plot_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_forest.md)
layout: covariate facet strips (pmforest labels groups on the y-axis),
the `est (lo, hi)` estimate column (pmforest prints `est [lo, hi]`),
reference rows drawn as open diamonds (pmforest draws no point for
them), and no minor gridlines (add them through `theme = ` with
`panel.grid.minor.x`).

## See also

Other forest plot:
[`df_forest()`](https://ryancrass.github.io/pmxhelpr/reference/df_forest.md),
[`df_forest_preprocess()`](https://ryancrass.github.io/pmxhelpr/reference/df_forest_preprocess.md),
[`is_forest_stats()`](https://ryancrass.github.io/pmxhelpr/reference/is_forest_stats.md),
[`plot_build_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_forest.md),
[`plot_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_forest.md),
[`style_forest()`](https://ryancrass.github.io/pmxhelpr/reference/style_forest.md),
[`style_forest_coveffectsplot()`](https://ryancrass.github.io/pmxhelpr/reference/style_forest_coveffectsplot.md),
[`style_forest_pmxforest()`](https://ryancrass.github.io/pmxhelpr/reference/style_forest_pmxforest.md),
[`style_forest_pumas()`](https://ryancrass.github.io/pmxhelpr/reference/style_forest_pumas.md)

## Examples

``` r
style_forest_pmforest()
#> <ggstylekit_style_spec>
#>   colors                 <palette function>
#>   fill                   white , grey35
#>   linetypes              solid  , dotdash
#>   alphas                 1.0, 1.0, 1.0, 1.0, 0.2
#>   shapes                 23, 23
#>   sizes                  3.5, 3.5
#>   linewidths             0.5, 0.5
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
style_forest_pmforest(fill = c(ref_band = "skyblue"))
#> <ggstylekit_style_spec>
#>   colors                 <palette function>
#>   fill                   white  , skyblue
#>   linetypes              solid  , dotdash
#>   alphas                 1.0, 1.0, 1.0, 1.0, 0.2
#>   shapes                 23, 23
#>   sizes                  3.5, 3.5
#>   linewidths             0.5, 0.5
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
style_forest_pmforest(logx = TRUE)
#> <ggstylekit_style_spec>
#>   colors                 <palette function>
#>   fill                   white , grey35
#>   linetypes              solid  , dotdash
#>   alphas                 1.0, 1.0, 1.0, 1.0, 0.2
#>   shapes                 23, 23
#>   sizes                  3.5, 3.5
#>   linewidths             0.5, 0.5
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
