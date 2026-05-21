# Package index

## Plot Functions

Top-level functions returning a `ggplot` object.

- [`plot_dvtime()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvtime.md)
  : Plot a dependent variable versus time
- [`plot_dvconc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvconc.md)
  : Plot a dependent variable versus concentration
- [`plot_gof()`](https://ryancrass.github.io/pmxhelpr/reference/plot_gof.md)
  : Plot population overlay goodness-of-fit (GOF) plots
- [`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md)
  : Plot a visual predictive check (VPC) for continuous data with exact
  time bins
- [`plot_vpc_cens()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cens.md)
  : Plot a censoring (BLQ-proportion) VPC with exact time bins
- [`plot_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_doseprop.md)
  : Plot a dose-proportionality assessment via power law (log-log)
  regression
- [`plot_vpc_legend()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_legend.md)
  : Plot a legend for a visual predictive check (VPC)

## Plot Builders

Lower-level renderers that draw plots from precomputed stats objects.

- [`plot_build_vpc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_vpc.md)
  :

  Build a VPC ggplot from a `vpc_stats` object

- [`plot_build_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_doseprop.md)
  :

  Build a dose-proportionality ggplot from a `doseprop_stats` object

## Theme Factories

Per-plot-family factories that return default theme objects for the
`theme` argument.

- [`plot_dvtime_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvtime_theme.md)
  : Concentration-time plot theme
- [`plot_dvconc_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvconc_theme.md)
  : Response versus concentration plot theme
- [`plot_gof_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_gof_theme.md)
  : Population overlay GOF plot theme
- [`plot_doseprop_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_doseprop_theme.md)
  : Dose-proportionality plot theme
- [`plot_vpc_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_theme.md)
  : VPC plot theme

## Element Constructors

Typed constructors for `theme` overrides; map to `ggplot2` geom
aesthetics.

- [`pmx_point()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_point.md)
  : Point aesthetics
- [`pmx_line()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_line.md)
  : Line aesthetics
- [`pmx_ribbon()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_ribbon.md)
  : Ribbon aesthetics
- [`pmx_errorbar()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_errorbar.md)
  : Error bar aesthetics
- [`pmx_trend()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_trend.md)
  : Trend line aesthetics (dvconc loess/linear)
- [`pmx_style()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_style.md)
  : Shared style for point and line layers
- [`pmx_color()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_color.md)
  : GOF overlay color aesthetics

## Layer Visibility

Constructors for the `shown` argument of plot functions.

- [`plot_vpc_shown()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_shown.md)
  : VPC layer visibility settings
- [`plot_gof_shown()`](https://ryancrass.github.io/pmxhelpr/reference/plot_gof_shown.md)
  : GOF layer visibility settings

## VPC Statistics

Compute observed and simulated quantile summaries for VPC plotting.

- [`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)
  : Compute VPC summary statistics from raw simulation data

## Dose-Proportionality Statistics

Power law (log-log) regression helpers.

- [`df_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/df_doseprop.md)
  : Compute and tabulate estimates for log-log regression

## Vectorized Helpers

Vectorized utilities intended for use inside
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).

- [`var_addn()`](https://ryancrass.github.io/pmxhelpr/reference/var_addn.md)
  : Append counts of unique identifiers to group labels
- [`var_dosenorm()`](https://ryancrass.github.io/pmxhelpr/reference/var_dosenorm.md)
  : Internal Helper: Apply dose-normalization to a variable
- [`var_predcorr()`](https://ryancrass.github.io/pmxhelpr/reference/var_predcorr.md)
  : Apply prediction correction

## mrgsolve Wrappers

Convenience wrappers around `mrgsolve` model loading and simulation.

- [`model_mread_load()`](https://ryancrass.github.io/pmxhelpr/reference/model_mread_load.md)
  : Load an mrgsolve model file from the internal model library

- [`df_mrgsim_replicate()`](https://ryancrass.github.io/pmxhelpr/reference/df_mrgsim_replicate.md)
  :

  Execute a visual predictive check (VPC) simulation using `mrgsolve`

- [`df_mrgsim_addpred()`](https://ryancrass.github.io/pmxhelpr/reference/df_mrgsim_addpred.md)
  :

  Add population predictions (`PRED`) to a data.frame

## S3 Class System

Predicates, constructors, operators, and methods for the shared
`pmx_stats` / `pmx_theme` base classes and their `vpc_stats`,
`doseprop_stats`, and `pmx_element` siblings.

- [`pmx_stats()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_stats.md)
  :

  Construct a `pmx_stats` container

- [`pmx_theme()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_theme.md)
  :

  Construct a `pmx_theme`

- [`is_pmx_stats()`](https://ryancrass.github.io/pmxhelpr/reference/is_pmx_stats.md)
  :

  Test whether an object is a `pmx_stats` container

- [`is_pmx_vpc_plot()`](https://ryancrass.github.io/pmxhelpr/reference/is_pmx_vpc_plot.md)
  :

  Test whether an object is a `pmx_vpc_plot`

- [`is_vpc_stats()`](https://ryancrass.github.io/pmxhelpr/reference/is_vpc_stats.md)
  :

  Test whether an object is a `vpc_stats` container

- [`is_doseprop_stats()`](https://ryancrass.github.io/pmxhelpr/reference/is_doseprop_stats.md)
  :

  Test whether an object is a `doseprop_stats` container

- [`is_pmx_element()`](https://ryancrass.github.io/pmxhelpr/reference/is_pmx_element.md)
  : Test whether an object is a pmx theme element

- [`is_pmx_theme()`](https://ryancrass.github.io/pmxhelpr/reference/is_pmx_theme.md)
  : Test whether an object is a pmx plot theme

- [`` `+`( ``*`<pmx_vpc_plot>`*`)`](https://ryancrass.github.io/pmxhelpr/reference/plus-.pmx_vpc_plot.md)
  :

  Add a layer to a `pmx_vpc_plot` with a facet warning

- [`` `+`( ``*`<pmx_theme>`*`)`](https://ryancrass.github.io/pmxhelpr/reference/plus-.pmx_theme.md)
  : Combine two pmx plot themes

- [`` `+`( ``*`<pmx_element>`*`)`](https://ryancrass.github.io/pmxhelpr/reference/plus-.pmx_element.md)
  : Combine two pmx theme elements

- [`print(`*`<pmx_stats>`*`)`](https://ryancrass.github.io/pmxhelpr/reference/print.pmx_stats.md)
  :

  Print method for `pmx_stats`

- [`summary(`*`<pmx_stats>`*`)`](https://ryancrass.github.io/pmxhelpr/reference/summary.pmx_stats.md)
  :

  Summary method for `pmx_stats`

- [`as.data.frame(`*`<pmx_stats>`*`)`](https://ryancrass.github.io/pmxhelpr/reference/as.data.frame.pmx_stats.md)
  :

  Coerce a `pmx_stats` object to a data.frame

- [`print(`*`<vpc_stats>`*`)`](https://ryancrass.github.io/pmxhelpr/reference/print.vpc_stats.md)
  :

  Print method for `vpc_stats`

- [`summary(`*`<vpc_stats>`*`)`](https://ryancrass.github.io/pmxhelpr/reference/summary.vpc_stats.md)
  :

  Summary method for `vpc_stats`

- [`print(`*`<doseprop_stats>`*`)`](https://ryancrass.github.io/pmxhelpr/reference/print.doseprop_stats.md)
  :

  Print method for `doseprop_stats`

- [`summary(`*`<doseprop_stats>`*`)`](https://ryancrass.github.io/pmxhelpr/reference/summary.doseprop_stats.md)
  :

  Summary method for `doseprop_stats`

- [`print(`*`<pmx_element>`*`)`](https://ryancrass.github.io/pmxhelpr/reference/print.pmx_element.md)
  : Print method for pmx theme elements

- [`print(`*`<pmx_theme>`*`)`](https://ryancrass.github.io/pmxhelpr/reference/print.pmx_theme.md)
  : Print method for pmx plot themes

## Datasets

Internal example datasets used in vignettes and worked examples.

- [`data_sad`](https://ryancrass.github.io/pmxhelpr/reference/data_sad.md)
  : Example NONMEM Analysis-Ready Dataset for PK/PD Modeling of a Single
  Ascending Dose Study
- [`data_sad_nca`](https://ryancrass.github.io/pmxhelpr/reference/data_sad_nca.md)
  : Example NCA Parameter Dataset Output from PKNCA
- [`data_sad_pkfit`](https://ryancrass.github.io/pmxhelpr/reference/data_sad_pkfit.md)
  : Example NONMEM Analysis-Ready Dataset for PK Modeling of a Single
  Ascending Dose Study
