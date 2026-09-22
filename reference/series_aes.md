# Internal helper: read a series' aesthetics out of a style_spec

Layers the builders set inline rather than tagging with
[`ggstylekit::series_layer()`](https://rdrr.io/pkg/ggstylekit/man/series_layer.html)
(VPC ribbons, the LLOQ line, and the
[`plot_vpc_legend()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_legend.md)
proxies) source their aesthetics from the style_spec's per-series maps
keyed by `series` through this reader.

## Usage

``` r
series_aes(spec, series)
```

## Arguments

- spec:

  A `ggstylekit_style_spec` (or any list with the per-series map fields
  `colors`, `fill`, `alphas`, `shapes`, `sizes`, `linetypes`,
  `linewidths`).

- series:

  Character scalar series/role name (e.g. `"loq_line"`, `"sim_pi_ci"`).

## Value

A named list of the aesthetics set for `series` (unset aesthetics are
dropped), using ggplot2 aesthetic names (`colour`, `fill`, `alpha`,
`shape`, `size`, `linetype`, `linewidth`).
