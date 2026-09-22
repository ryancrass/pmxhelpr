# Concentration-time plot style (`plot_dvtime`)

Default
[`ggstylekit::style_spec()`](https://rdrr.io/pkg/ggstylekit/man/style_spec.html)
for
[`plot_dvtime()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvtime.md).
Series names are the plot roles: `obs_point`, `obs_line` (spaghetti),
`cent_point`, `cent_line`, `cent_errorbar`, `ref_line`, `loq_line`.

## Usage

``` r
style_dvtime(...)
```

## Arguments

- ...:

  Fields passed to
  [`ggstylekit::style_spec()`](https://rdrr.io/pkg/ggstylekit/man/style_spec.html),
  overriding the preset defaults; call the preset with no arguments to
  view them. Per-series maps (`colors`, `fill`, `shapes`, `sizes`,
  `linetypes`, `linewidths`, `alphas`) merge entry-wise onto the
  defaults, so setting one role leaves the others unchanged (e.g.
  `shapes = c(obs_point = 16)`); all other fields replace their default
  wholesale (e.g. `title = "..."`). A per-series map may instead be a
  palette `function(n)` (e.g.
  `function(n) grDevices::hcl.colors(n, "Viridis")`), which `ggstylekit`
  calls with the number of mapped groups; a palette has no entries to
  merge, so it replaces the default map.

## Value

A `ggstylekit_style_spec` object.

## Details

The error bar cap width is the `errorbar_width` field (e.g.
`style_dvtime(errorbar_width = 10)`, in x-axis units). The preset leaves
it unset, and
[`plot_dvtime()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvtime.md)
then defaults it to 2.5% of the maximum nominal time so the caps scale
with the time axis.

Like every pmxhelpr preset, it places legend titles above the keys
(`legend.title.position = "top"`) and applies the house
[`theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)-derived
theme.

## See also

Other exploratory analysis:
[`plot_dvconc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvconc.md),
[`plot_dvtime()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvtime.md),
[`style_dvconc()`](https://ryancrass.github.io/pmxhelpr/reference/style_dvconc.md)

## Examples

``` r
style_dvtime()
#> <ggstylekit_style_spec>
#>   colors                 NULL
#>   fill                   NULL
#>   linetypes              solid , solid , dashed, dashed
#>   alphas                 0.5, 0.5, 0.0, 1.0, 1.0, 1.0, 1.0
#>   shapes                  1, 16
#>   sizes                  0.75, 1.25
#>   linewidths             0.50, 0.75, 0.75, 0.50, 0.50
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
#>   legend.position        NULL
#>   legend.title.position  top
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
style_dvtime(shapes = c(obs_point = 16, cent_point = 16))
#> <ggstylekit_style_spec>
#>   colors                 NULL
#>   fill                   NULL
#>   linetypes              solid , solid , dashed, dashed
#>   alphas                 0.5, 0.5, 0.0, 1.0, 1.0, 1.0, 1.0
#>   shapes                 16, 16
#>   sizes                  0.75, 1.25
#>   linewidths             0.50, 0.75, 0.75, 0.50, 0.50
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
#>   legend.position        NULL
#>   legend.title.position  top
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
style_dvtime(errorbar_width = 10)
#> <ggstylekit_style_spec>
#>   colors                 NULL
#>   fill                   NULL
#>   linetypes              solid , solid , dashed, dashed
#>   alphas                 0.5, 0.5, 0.0, 1.0, 1.0, 1.0, 1.0
#>   shapes                  1, 16
#>   sizes                  0.75, 1.25
#>   linewidths             0.50, 0.75, 0.75, 0.50, 0.50
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
#>   errorbar_width         10
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
#>   legend.position        NULL
#>   legend.title.position  top
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
