# pmxhelpr: Helper Functions for Pharmacometrics

Helper functions to make pharmacometrics workflows more standardized,
efficient, and reproducible. Includes helper functions for exploratory
data analysis, goodness-of-fit diagnostics, ands visual predictive check
model evaluation.

## Getting started

Recommended entry points by task:

- Exploratory PK / PK-PD analysis –
  [`plot_dvtime()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvtime.md),
  [`plot_dvconc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvconc.md)

- Goodness-of-fit diagnostics –
  [`plot_gof()`](https://ryancrass.github.io/pmxhelpr/reference/plot_gof.md)

- Visual predictive checks –
  [`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md),
  [`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)

- Dose-proportionality assessment –
  [`plot_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_doseprop.md),
  [`df_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/df_doseprop.md)

- Plot styling – `style_*()` presets returning a
  [`ggstylekit::style_spec()`](https://rdrr.io/pkg/ggstylekit/man/style_spec.html)
  for the `style` argument; adjust finished plots with
  [`restyle_plot()`](https://rdrr.io/pkg/ggstylekit/man/restyle_plot.html),
  [`reveal()`](https://rdrr.io/pkg/ggstylekit/man/reveal.html), and
  [`combine_styled_plots()`](https://rdrr.io/pkg/ggstylekit/man/combine_styled_plots.html)

Full narrative documentation with worked examples lives on the package
website at <https://ryancrass.github.io/pmxhelpr/>.

## See also

Useful links:

- <https://github.com/ryancrass/pmxhelpr>

- <https://ryancrass.github.io/pmxhelpr/>

- Report bugs at <https://github.com/ryancrass/pmxhelpr/issues>

## Author

**Maintainer**: Ryan Crass <ryancrass@gmail.com>
([ORCID](https://orcid.org/0000-0001-5403-4730)) \[copyright holder\]

Authors:

- Ryan Crass <ryancrass@gmail.com>
  ([ORCID](https://orcid.org/0000-0001-5403-4730)) \[copyright holder\]
