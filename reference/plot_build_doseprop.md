# Build a dose-proportionality ggplot from a `doseprop_stats` object

Constructs a log-log regression scatter plot from a
[`df_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/df_doseprop.md)
result (or any object satisfying the `doseprop_stats` contract).
Recovers the observation rows and column names from the object's
attributes, builds the faceting label from the per-metric `PowerCI`
text, and renders the scatter

- trend layers.

Most users will reach this function indirectly via
[`plot_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_doseprop.md).
Call `plot_build_doseprop()` directly when working from a manually-
constructed or cached `doseprop_stats` object — for example, plotting a
precomputed result from disk or a custom pipeline that produces
compatible columns and attributes.

## Usage

``` r
plot_build_doseprop(stats, style = NULL, se = TRUE)
```

## Arguments

- stats:

  A `doseprop_stats` object (typically the output of
  [`df_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/df_doseprop.md)).
  Must contain `$obs` and `$config` slots with `metric_name_var`,
  `metric_value_var`, `dose_var`, and `ci`. Validated by
  [`validate_doseprop_stats()`](https://ryancrass.github.io/pmxhelpr/reference/validate_doseprop_stats.md)
  at entry.

- style:

  A
  [`ggstylekit::style_spec()`](https://rdrr.io/pkg/ggstylekit/man/style_spec.html)
  controlling plot aesthetics. Defaults to
  [`style_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/style_doseprop.md);
  view the defaults by running
  [`style_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/style_doseprop.md)
  with no arguments. Customize by passing `style = style_doseprop(...)`,
  or restyle the returned plot with
  [`restyle_plot()`](https://rdrr.io/pkg/ggstylekit/man/restyle_plot.html).
  The per-metric facet layout is taken from the style's `facet_scales`
  (default `"free"`), `facet_nrow`, and `facet_ncol`; the `facet`,
  `logx`, and `logy` fields are ignored because the builder sets the
  facet and log-log axes itself.

- se:

  logical to display confidence interval around regression. Default is
  `TRUE`.

## Value

a `ggplot` plot object

## See also

Other dose proportionality:
[`df_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/df_doseprop.md),
[`is_doseprop_stats()`](https://ryancrass.github.io/pmxhelpr/reference/is_doseprop_stats.md),
[`plot_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_doseprop.md),
[`style_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/style_doseprop.md)

## Examples

``` r
stats <- df_doseprop(dplyr::filter(data_sad_nca, PART == "Part 1-SAD"),
                      metrics = c("aucinf.obs", "cmax"))
plot_build_doseprop(stats)

plot_build_doseprop(stats, se = FALSE)
```
