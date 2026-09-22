# VPC plot style (`plot_vpc_cont` / `plot_vpc_cens`)

Default
[`ggstylekit::style_spec()`](https://rdrr.io/pkg/ggstylekit/man/style_spec.html)
for the VPC family. Line/point roles (`obs_point`, `obs_median_line`,
`obs_pi_line`, `sim_pi_line`, `sim_median_line`, `loq_line`) are styled
by `style_plot()`. The three ribbon roles (`sim_pi_ci`, `sim_pi_area`,
`sim_median_ci`) are `geom_ribbon` layers whose `fill`/`alpha` the VPC
builder sets inline, read from these maps via
[`series_aes()`](https://ryancrass.github.io/pmxhelpr/reference/series_aes.md)
(inline aesthetics take precedence over the `area_*` fields).

## Usage

``` r
style_vpc(...)
```

## Arguments

- ...:

  Fields passed to
  [`ggstylekit::style_spec()`](https://rdrr.io/pkg/ggstylekit/man/style_spec.html),
  overriding the preset defaults; call the preset with no arguments to
  view them. Per-series maps merge entry-wise onto the defaults; all
  other fields replace their default wholesale. A per-series map may
  instead be a palette `function(n)` (e.g.
  `function(n) grDevices::hcl.colors(n, "Viridis")`), which `ggstylekit`
  calls with the number of mapped groups; a palette has no entries to
  merge, so it replaces the default map.

## Value

A `ggstylekit_style_spec` object.

## Details

Stratification facets are built by the VPC plotting functions from
`strat_var` (not by `style_plot()`), but the `facet_scales`,
`facet_nrow`, and `facet_ncol` fields are honored; `facet` is ignored
when `strat_var` is supplied.

Like every pmxhelpr preset, it places legend titles above the keys
(`legend.title.position = "top"`) and applies the house
[`theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)-derived
theme.

## See also

Other vpc:
[`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md),
[`is_pmx_vpc_plot()`](https://ryancrass.github.io/pmxhelpr/reference/is_pmx_vpc_plot.md),
[`is_vpc_stats()`](https://ryancrass.github.io/pmxhelpr/reference/is_vpc_stats.md),
[`plot_build_vpc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_vpc.md),
[`plot_vpc_cens()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cens.md),
[`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md),
[`plot_vpc_legend()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_legend.md),
[`plot_vpc_shown()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_shown.md)

## Examples

``` r
style_vpc()
#> <ggstylekit_style_spec>
#>   colors                 #0000FF, #FF0000, #0000FF, #000000, #000000, #990000
#>   fill                   #0000FF, #0000FF, #FF0000
#>   linetypes              solid , dashed, dotted, dashed, dashed
#>   alphas                 0.70, 0.15, 0.15, 0.30
#>   shapes                 1
#>   sizes                  1
#>   linewidths             1.0, 0.5, 1.0, 1.0, 0.5
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
style_vpc(fill = c(sim_pi_ci = "#3388cc"), facet_scales = "free_y")
#> <ggstylekit_style_spec>
#>   colors                 #0000FF, #FF0000, #0000FF, #000000, #000000, #990000
#>   fill                   #3388cc, #0000FF, #FF0000
#>   linetypes              solid , dashed, dotted, dashed, dashed
#>   alphas                 0.70, 0.15, 0.15, 0.30
#>   shapes                 1
#>   sizes                  1
#>   linewidths             1.0, 0.5, 1.0, 1.0, 0.5
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
#>   facet_scales           free_y
#>   facet_nrow             NULL
#>   facet_ncol             NULL
#>   theme                  <ggplot2 theme>
```
