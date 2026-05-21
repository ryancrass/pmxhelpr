# Changelog

## pmxhelpr 0.5.0

This release simplifies plotting function interfaces, harmonizes naming
across the package, replaces the flat theme list with a system of theme
and element constructor, and introduces class-tagged stats containers
(`vpc_stats`, `doseprop_stats`) that can be tabulated and re-plotted
with recomputation.

The VPC pipeline is rewritten without the `vpc` package dependency with
comprehensive BLQ handling for standard and prediction- corrected VPCs
of the continuous data range and and a new function for VPC plots of the
proportion of data BLQ over time.

### Breaking changes

#### Removed functions

- `plot_dvtime_dual()` is removed — compose PK and PD panels with
  [`plot_dvtime()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvtime.md) +
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).
- `df_addn()` is removed — replaced by
  [`var_addn()`](https://ryancrass.github.io/pmxhelpr/reference/var_addn.md).
- `df_pcdv()` is removed — replaced by
  [`var_predcorr()`](https://ryancrass.github.io/pmxhelpr/reference/var_predcorr.md).
- `df_nobsbin()` is removed — bin counting is now integrated into
  [`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md).
- `mod_loglog()` and `df_loglog()` are removed — log-log regression is
  performed inside
  [`df_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/df_doseprop.md).
- `dvconc_caption()` and `dvtime_caption()` are no longer exported.

#### Renamed functions

- `df_addpred()` is renamed to
  [`df_mrgsim_addpred()`](https://ryancrass.github.io/pmxhelpr/reference/df_mrgsim_addpred.md).
- `plot_popgof()` / `plot_popgof_theme()` are renamed to
  [`plot_gof()`](https://ryancrass.github.io/pmxhelpr/reference/plot_gof.md)
  /
  [`plot_gof_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_gof_theme.md).
- `plot_vpc_exactbins()` is renamed to
  [`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md)
  (arguments revised — see “Removed and renamed arguments”).
- `plot_vpclegend()` is renamed to
  [`plot_vpc_legend()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_legend.md).
- `breaks_time()` is now an internal helper
  ([`var_timebreaks()`](https://ryancrass.github.io/pmxhelpr/reference/var_timebreaks.md)).

#### Removed and renamed arguments

- [`df_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/df_doseprop.md)
  and
  [`plot_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_doseprop.md):
  `metric_var` is renamed to `metric_name_var`, and `exp_var` is renamed
  to `metric_value_var`, to make the role of each column explicit (i.e.,
  exposure metric label variable vs exposure metric value variable).
- [`df_mrgsim_replicate()`](https://ryancrass.github.io/pmxhelpr/reference/df_mrgsim_replicate.md):
  the named-vector arguments `num_vars` and `char_vars` are removed;
  pass extra simulation columns through `...` to
  [`mrgsolve::mrgsim_df()`](https://mrgsolve.org/docs/reference/mrgsim.html)
  via `carry_out` (numeric) and `recover` (any type but useful for
  character / factor).
- [`plot_dvconc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvconc.md),
  [`plot_dvtime()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvtime.md),
  and
  [`plot_gof()`](https://ryancrass.github.io/pmxhelpr/reference/plot_gof.md):
  the `cfb` (logical) / `cfb_base` (numeric) argument pair is replaced
  by a single numeric `ref` argument (`NULL` = no line). For example,
  `cfb = TRUE, cfb_base = 0` becomes `ref = 0`.
- [`plot_gof()`](https://ryancrass.github.io/pmxhelpr/reference/plot_gof.md):
  `output_colors` is removed — overlay colors for DV, PRED, and IPRED
  are now set via
  [`pmx_color()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_color.md)
  in
  [`plot_gof_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_gof_theme.md).
- [`plot_vpc_legend()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_legend.md):
  `update` is renamed to `theme`, aligning with the theme-argument
  convention used by the other plot functions.
- All plot functions: `x_breaks`, `x_scale`, `x_lab`, and `y_lab` are
  removed — add the equivalent `ggplot2` layers to the returned plot
  object.

#### Behavior changes

- The VPC pipeline is rewritten without the `vpc` package dependency.
  [`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)
  summary column names follow a `<role>_<low|med|hi>` scheme — simulated
  quantiles are `sim_low_*`, `sim_med_*`, `sim_hi_*`; observed quantiles
  are `obs_low/med/hi`; per-bin counts are `obs_n` and `obs_n_blq`
  (previously `nbin` / `nobsblq`); BLQ proportions (std-VPC only) are
  `sim_prop_blq_*`. The `min_bin_count` filter now gates on quantifiable
  observations (`obs_n - obs_n_blq`) for VPCs and total observations
  (`obs_n`) for cens-VPCs. Comprehensive BLQ handling includes censoring
  of observed quantiles without censoring simulated data in VPCs and
  censoring both observed and simulated data at LLOQ before quantile
  aggregation in pcVPCs.
- `plot_vpc_legend(ci)` default changes from `c(0.05, 0.95)` (vector) to
  `0.90` (scalar). `lloq` now accepts a vector and renders one legend
  entry per unique value.
- `vpc` is removed from `Imports` (VPC pipeline no longer depends on
  it).
- `patchwork` moves from `Imports` to `Suggests` — install explicitly if
  you compose multi-panel layouts (e.g. the legend + VPC pattern in the
  README).
- [`var_addn()`](https://ryancrass.github.io/pmxhelpr/reference/var_addn.md)
  now derives factor levels from the order of first appearance in
  `grp_var`, so a pre-sorted input like `c(10, 200, 1000)` yields levels
  in numeric order rather than the alphabetic order returned by
  [`factor()`](https://rdrr.io/r/base/factor.html). Removes the need to
  follow up with
  [`forcats::fct_relevel()`](https://forcats.tidyverse.org/reference/fct_relevel.html)
  for numeric dose / time groupings.
- `plot_build_vpc(loq = …)` supplied explicitly always wins over
  `compute_out$config$loq` and draws one global reference line per
  unique value. Per-facet `(strat_var × LLOQ)` dispatch is reserved for
  the inherited case.

### New features

#### New functions

- [`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md)
  generates a 1-step (plot `vpc_stats`) or 2-step (calculate and plot)
  VPC of the continuous portion of the data range, wrapping both
  `df_vpcstats` and
  [`plot_build_vpc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_vpc.md).
- [`plot_vpc_cens()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cens.md)
  generates a 1-step (plot `vpc_stats`) or 2-step (calculate and plot)
  VPC of the censored portion of the data range, wrapping both
  `df_vpcstats` and
  [`plot_build_vpc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_vpc.md).
- [`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)
  computes VPC summary statistics from raw replicate simulation output
  and returns a `vpc_stats` / `pmx_stats` container. Always emits both
  standard and prediction-corrected statistics so the same object
  supports toggling `pcvpc` when plotting.
- [`plot_build_vpc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_vpc.md)
  renders a VPC ggplot from any `vpc_stats` container. Use directly when
  working from a manually-constructed or cached `vpc_stats` object; most
  users still go through
  [`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md)
  and/or `plot_vpc_cens`.
- [`plot_build_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_doseprop.md)
  renders a dose-proportionality ggplot from any `doseprop_stats`
  container; most users still go through
  [`plot_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_doseprop.md).
- [`plot_doseprop_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_doseprop_theme.md)
  is a theme factory for
  [`plot_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_doseprop.md)
  with keys `obs_point` and `linear`.
- [`plot_gof_shown()`](https://ryancrass.github.io/pmxhelpr/reference/plot_gof_shown.md)
  and
  [`plot_vpc_shown()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_shown.md)
  are constructors for GOF and VPC layer-visibility settings.
- [`var_addn()`](https://ryancrass.github.io/pmxhelpr/reference/var_addn.md)
  generates factor labels with unique-value counts, replacing
  `df_addn()`.
- [`var_dosenorm()`](https://ryancrass.github.io/pmxhelpr/reference/var_dosenorm.md)
  performs dose normalization.
- [`var_predcorr()`](https://ryancrass.github.io/pmxhelpr/reference/var_predcorr.md)
  performs prediction correction, replacing `df_pcdv()`.

#### New arguments

- `df_mrgsim_replicate(parallel = FALSE)` — when `TRUE`, replicate
  simulations run via
  [`future.apply::future_lapply()`](https://future.apply.futureverse.org/reference/future_lapply.html)
  using the active `future` plan, with per-replicate L’Ecuyer-CMRG RNG
  streams so output stays reproducible under a fixed `seed`. Adds
  `future.apply` to `Suggests`.
- [`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)
  and
  [`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md)
  gain `mode = c("auto", "rank", "drop")` controlling how BLQ-encoded
  values flow into quantile aggregation. `"auto"` (default) resolves to
  `"rank"` for std VPC and `"drop"` for pcVPC.
- [`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)
  and
  [`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md)
  accept per-row `LLOQ` (multi-cohort / mid-study assay-update
  datasets): each observation is censored against its own row’s `LLOQ`,
  and stratified plots draw one reference line per `(strat × LLOQ)`
  combination. Unique non-`NA` values are exposed via `config$loq`.
- [`plot_dvtime()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvtime.md)
  and
  [`plot_gof()`](https://ryancrass.github.io/pmxhelpr/reference/plot_gof.md):
  `loq_method` accepts the character aliases `"none"`, `"postdose"`, and
  `"all"` in addition to the existing numeric values `0`, `1`, `2`. The
  legacy numeric form continues to work.
- [`plot_dvtime()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvtime.md)
  emits a message when `loq` is inherited from the `LLOQ` column of
  `data`, mirroring
  [`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md).
  Pass `loq` explicitly to suppress.
- `plot_gof(blq_mode = c("obs", "all"))` scopes BLQ imputation to the
  observed `DV` layer (default) or to observed and model-predicted
  layers.
- Time, prediction, and simulated-DV columns across functions are now
  scalar NSE-friendly arguments (`time_var`, `ntime_var`, `pred_var`,
  `ipred_var`, `sim_dv_var`) instead of named lists (`time_vars`,
  `output_vars`). Pass arguments as bare names or strings.
- `plot_vpc_legend(type = c("cont", "cens"))` selects the label set and
  layer set the legend describes — `"cens"` relabels the
  central-tendency entries to `"Obs Prop BLQ"`, `"Sim Prop BLQ"`, and
  `"Sim <ci>% CI Prop BLQ"` and suppresses all prediction-interval
  entries, matching the
  [`plot_vpc_cens()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cens.md)
  plot.

#### Dual-mode pipelines (replot without recompute)

- [`plot_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_doseprop.md)
  accepts the `doseprop_stats` container returned by
  [`df_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/df_doseprop.md)
  directly. Pass a precomputed result to skip the regression refit and
  re-plot with different `theme` or `se` settings. Pipeline arguments
  (`metrics`, `metric_name_var`, `metric_value_var`, `dose_var`,
  `method`, `ci`, `sigdigits`) cannot be honored on the precomputed path
  and error with a clear message.
- [`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md)
  accepts the `vpc_stats` container returned by
  [`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)
  directly. Pass a precomputed result to re-plot with different
  `min_bin_count`, `shown`, `theme`, or `pcvpc` settings without
  re-summarizing. Pipeline arguments (`strat_var`, `loq`, `mode`, `pi`,
  `ci`, column-name args) cannot be honored on the precomputed path and
  error with a clear message.

#### Theme system

- New
  [`pmx_theme()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_theme.md)
  factory and `pmx_*()` element constructors —
  [`pmx_point()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_point.md),
  [`pmx_line()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_line.md),
  [`pmx_ribbon()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_ribbon.md),
  [`pmx_errorbar()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_errorbar.md),
  and
  [`pmx_trend()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_trend.md)
  — replace the previous flat list of role- and geometry-based theme
  elements.
- Theme factory keys follow a `layer_element` convention
  (e.g. `obs_point`, `cent_errorbar`, `ref_line`, `loq_line`). VPC keys
  follow an `element_statistic` convention aligned with the `shown`
  argument (e.g. `obs_median_line`, `sim_pi_ci`).
- New
  [`pmx_color()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_color.md)
  constructor sets overlay colors for DV, PRED, and IPRED in
  [`plot_gof_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_gof_theme.md)
  — e.g. `plot_gof_theme(cent_color = pmx_color(pred = "purple"))`.
- New
  [`pmx_style()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_style.md)
  convenience constructor applies shared aesthetics (color, alpha) to
  both point and line elements of a role —
  e.g. `plot_dvtime_theme(obs = pmx_style(alpha = 0.3))`.
- `+.pmx_theme()` and `+.pmx_element()` methods compose partial themes;
  [`pmx_style()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_style.md)
  shortcuts are applied first and explicit element-level overrides win.

#### S3 classes and predicates

- `pmx_stats` is a shared container with `vpc_stats` and
  `doseprop_stats` subclasses — slots `$stats`, `$obs`, `$config` —
  returned by
  [`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)
  and
  [`df_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/df_doseprop.md).
- [`print()`](https://rdrr.io/r/base/print.html),
  [`summary()`](https://rdrr.io/r/base/summary.html), and
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) methods
  on `pmx_stats` give a focused REPL view (dimensions, slot row counts,
  run-config keys) and a plain `data.frame` view of `$stats`. Subclasses
  override [`print()`](https://rdrr.io/r/base/print.html) /
  [`summary()`](https://rdrr.io/r/base/summary.html) for richer output
  (column groups for VPC; one-line-per-metric `PowerCI` + interpretation
  for doseprop).
- [`print()`](https://rdrr.io/r/base/print.html) methods on `pmx_theme`
  and `pmx_element` objects render a banner + per-field listing instead
  of the default class-attribute dump.
- `pmx_vpc_plot` is a `ggplot` subclass that warns when `facet_*()`
  layers are added externally, pointing users to the `strat_var`
  argument instead.
- Predicates
  [`is_doseprop_stats()`](https://ryancrass.github.io/pmxhelpr/reference/is_doseprop_stats.md),
  [`is_pmx_element()`](https://ryancrass.github.io/pmxhelpr/reference/is_pmx_element.md),
  [`is_pmx_stats()`](https://ryancrass.github.io/pmxhelpr/reference/is_pmx_stats.md),
  [`is_pmx_theme()`](https://ryancrass.github.io/pmxhelpr/reference/is_pmx_theme.md),
  [`is_pmx_vpc_plot()`](https://ryancrass.github.io/pmxhelpr/reference/is_pmx_vpc_plot.md),
  and
  [`is_vpc_stats()`](https://ryancrass.github.io/pmxhelpr/reference/is_vpc_stats.md)
  are exported. With `strict = TRUE`, the stats and theme predicates run
  the corresponding structural validator.

#### Vignettes and documentation

- New `Getting Started` vignette ships with the package for new-user
  onboarding.
- New `Dose-Proportionality Workflow` vignette covers
  [`df_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/df_doseprop.md)
  →
  [`plot_build_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_doseprop.md)
  →
  [`plot_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_doseprop.md),
  the `doseprop_stats` / `pmx_stats` class system, and the
  precomputed-stats path.
- The VPC, GOF, exploratory PK / PK-PD, and plot-themes vignettes are
  restructured into focused articles with cross-links.
- The `Plot Themes and Aesthetics` article covers inspecting
  `pmx_element` / `pmx_theme` objects and composing partial themes with
  `+`.
- The `Visual Predictive Check Workflow` article covers
  [`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md),
  [`plot_vpc_cens()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cens.md),
  the pipeline column contracts
  ([`df_mrgsim_replicate()`](https://ryancrass.github.io/pmxhelpr/reference/df_mrgsim_replicate.md)
  →
  [`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)
  →
  [`plot_build_vpc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_vpc.md)),
  and inspection of `vpc_stats` objects.
- Narrative vignettes are hosted on the pkgdown site
  (<https://ryancrass.github.io/pmxhelpr/articles/>); the installed
  package no longer ships built vignettes in `inst/doc/`, keeping the
  install footprint small.

### Minor improvements and bug fixes

- All exported functions that take column-name arguments accept bare
  names or strings via a shared NSE-resolution helper.
- Plot functions warn when the input dataset has more than one unique
  compartment after the `EVID == 0` filter, avoiding silently plotting
  observations from an unintended compartment.
- [`df_mrgsim_replicate()`](https://ryancrass.github.io/pmxhelpr/reference/df_mrgsim_replicate.md)
  errors when the user-supplied `carry_out` / `recover` values overlap
  the wrapped baseline columns, instead of silently shadowing them.
- Line-geom aesthetics use `linewidth` instead of the deprecated `size`
  throughout.
- `df_vpcstats(pcvpc = TRUE)` now drops `MDV == 1` records from
  simulated data when `loq = NULL` and the input dataset has no `LLOQ`
  column, mirroring the observed-data path. Previous behavior caused
  minor quantile drift on bins with NA observations.
- Test coverage expanded from ~100 to 800+ tests.

## pmxhelpr 0.4.0

- Rename `data_sad_pd` as `data_sad`. No longer export `data_sad_pd`.
- Rename `model` as `pkmodel` for consistency with `pdmodel`.
- Rename `pmxhelpr_vpc_theme` to `plot_vpc_theme` for consistency with
  other plot theme functions
- Change `dvconc_caption` and `dvtime_caption` to internal functions
  after simplifying
- Added `cfb_base` argument to `plot_gof` (was hardcoded as yintercept =
  0)
- Add helpers for central tendency handing, time variable handling, BLQ
  imputation, and error bar width setting to remove repetition in
  function calls
- Expand test coverage and refactor existing tests to reduce redundancy
- Fix bugs and typos globally in package

## pmxhelpr 0.3.9

- Add NSE support to accept bare column names in all exported functions
  (e.g., dv_var = DV) in addition to strings (e.g., dv_var = “DV”)
  across all exported functions that take column name arguments. Revise
  vignettes to highlight this functionality.
- Fix bug in `plot_vpclegend` that was resulting in alpha not being
  correctly applied

## pmxhelpr 0.3.8

- Fix bug in `plot_dvtime` to ensure closed circles are plotted in place
  of open circles for central tendency points
- Add minimum R version of 4.4.1 given use of native pipe operator
  (\|\>)

## pmxhelpr 0.3.7

- Add `plot_dvconc` to generate plots of response variables versus drug
  concentration with LOESS and/or linear trendlines
- Add `plot_dvtime_dual` wrapper function for `plot_dvtime` to plot two
  dependent variables versus time intended to support simultaneous
  visualization of PK and PD.
- Update `var_addn` (formerly `df_addn`) to offload factor ordering from
  the function to simply output. Handling can be done outside the
  function with alternative packages optimized for dealing with factor
  variables (e.g., `forcats`)
- Update `plot_gof_theme` to include separate line theme elements for
  individual observed lines (e.g., spaghetti plots) and central tendency
  of the observed.
- Revise the `Exploratory Data Analysis` vignette to include new
  functionality and revised workflows with new functions.
- Fix bug that was not scaling error bar caps for the x-axis range when
  `cent = "median_iqr"` across functions

## pmxhelpr 0.3.6

- Add `var_addn` (formerly `df_addn`) helper function to create and
  order factor labels including count of unique values to include counts
  in plot legends
- Add `plot_gof_theme` function to set and adjust default aesthetics for
  `plot_gof`
- Add LLOQ value and linetype to legend in `plot_dvtime`
- Add caption indicating method of BLQ imputation to `plot_dvtime`
- Fix bug in `df_pcdv` that was including missing values (MDV=1) in the
  median PRED calculation for nominal times with missing values,
  resulting in incorrect PRED-correction of observations in these bins
  ONLY. Simulated intervals and observed quantile lines were unaffected.
- Correct error in documentation of default number of breaks in
  `breaks_time`

## pmxhelpr 0.3.5

- Add option to pass the same dataset variable to `time_var` and
  `ntime_var` for all functions
- Add option to control default aesthetics (linewidth, linetype, size,
  shape, alpha) in `plot_dvtime` and `plot_gof`
- Fix bug in `plot_gof` that was preventing upper error bars from
  inheriting the color mapped to “DV”

## pmxhelpr 0.3.4

- Fix bug in `df_mrgsim_replicate` that was precluding use of
  non-default time variable names

## pmxhelpr 0.3.3

- Fix bug in `plot_dvtime` and `plot_gof` where variable for “DOSE” was
  being assessed when dosenorm = FALSE
- Update `breaks_time` to accept abbreviations for time units

## pmxhelpr 0.3.2

- Add `cent = "mean_sdl_upper"` option to `plot_dvtime` and `plot_gof`
  for plotting only the upper error bar
- Add `barwidth` argument to `plot_dvtime` and `plot_gof` to allow user
  to change error bar cap width.

## pmxhelpr 0.3.1

- Add `plot_gof` to generate population overlay goodness-of-fit plots
- Add `data_sad_pkfit` NONMEM PK model fit output dataset for `data_sad`
  based on `model`.
- Fixed bug in `plot_dvtime` that was leading to plotting mean +/- 2\*SD
  instead of mean +/- SD

## pmxhelpr 0.3.0

- Added new functionality for dose-proportionality assessment with
  `df_doseprop` and `plot_doseprop`, including helpers `mod_loglog` and
  `df_loglog` to perform and tabulate power law (log-log) regression of
  exposure versus dose

- Added new internal package dataset `data_sad_nca` for use with
  dose-proportionality assessment functions

- Added helper function `dvtime_caption` to generate the caption for
  `plot_dvtime`

- Added helper function `pmxhelpr_vpc_theme` to capture default VPC plot
  aesthetics \`

- Revised vignettes to describe workflow with new default aesthetics

- Remove use of `log_y` argument in VPC plot examples and added
  rationale to primary vignette for `plot_vpc_exactbins`

- Add unit tests for all primary functions

## pmxhelpr 0.2.4

- Fix bug in `plot_dvtime` to ensure `median + IQR error bars` prints on
  both linear and log-scale axes when `cent = "median_iqr"`

## pmxhelpr 0.2.3

- Added `cent = "median_iqr"` option to `plot_dvtime` to plot median
  with inter-quartile range error bars
- Updated the `ind_dv` logical argument in `plot_dvtime` to `grp_dv` to
  generalize use of grouping beyond connecting points within an
  individual subject
- Added the `grp_var` argument to `plot_dvtime` to allow specification
  of the variable assigned to the group aesthetic by the user. Default
  is `grp_var = "ID"`

## pmxhelpr 0.2.2

- Corrected bug in `plot_vpc_exactbins` when *ordered* factor variables
  are pasted to the `strat_var` argument. Ordered factors cannot be
  passed to the underlying stratification function from the `vpc`
  package (`add_stratification()`); therefore, ordered factors will be
  coerced to unordered factors within `plot_vpc_exactbins`

## pmxhelpr 0.2.1

- Corrected bug in `NA` handling within `breaks_time`

## pmxhelpr 0.2.0

- Added `plot_dvtime` function for exploratory concentration-time
  plotting

- Added `breaks_time` function to automatically determine x-axis breaks
  for time

- Functionality from `breaks_time` added to `plot_vpcexactbins`

- `Exploratory Data Analysis` vignette created to demonstrate
  functionality of new functions

- Added dependency: `labelling`

## pmxhelpr 0.1.4

- Fixed bug leading to error when generating stratified vpcs using the
  `strat_var` argument

## pmxhelpr 0.1.3

- Vignette vpc plot axes updated to time unit breaks
- Vignette bullet point lists updated to correct spacing
- Coerce argument `lloq` argument of `plot_legend()` to character type

## pmxhelpr 0.1.2

- Vignettes copy edited

## pmxhelpr 0.1.1

- Added dependency: `patchwork`

## pmxhelpr 0.1.0

- Full VPC workflow now available in Version 0.1.0 with three vignettes
  to demonstrate this functionality!

- `VPC Plot Aesthetics` vignette demonstrating options for modifying
  plot aesthetics, creating a plot legend using `plot_legend()`, and
  adding the VPC plot and legend objects together into a single object
  using the `patchwork` package

## pmxhelpr 0.0.3

- Additional functionality for `plot_vpc_exactbins`

  - `shown` argument added to facilitate customization of which layers
    are shown in the plot and standardize with new function
    `plot_legend`. Passed to `show` argument of `vpc::vpc()`

- `plot_legend` helper function to generate a legend for a VPC plot
  generated using `plot_vpc_exactbins()`, which can be combined with the
  VPC plot using the `patchwork` package

- `VPC Plots with BLQ Censoring` vignette demonstrating appropriate
  handling of data missing due to assay sensitivity (below the lower
  limit of quantification) using `plot_vpc_exactbins()` and workflow
  leveraging `df_mrgsim_replicate`.

## pmxhelpr 0.0.2

- Additional functionality for `plot_vpc_exactbins`
  - `loq` argument added to facilitate incorporating BLQ censoring of
    observed data in summary quantiles
- Bug fix to allow passing argument `vpcdb = TRUE` to `vpc::vpc()`

## pmxhelpr 0.0.1

- `data_sad` NONMEM and mrgsolve analysis-ready dataset for a
  hypothetical SAD study with assessment of food effect.

- `model` mrgsolve model file (class `mrgmod`) in the internal pmxhelpr
  model library .

- `model_load` function to load internal models from the pmxhelpr model
  library.

- `df_mrgsim_replicate` mrgsim wrapper function to run a simulation that
  replicates the input dataset multiple times, intended for application
  to generating Visual Predictive Check (VPC) simulation-based model
  diagnostics.

- `plot_vpc_exactbins` vpc wrapper function to using an exact time
  binning variable in the input dataset (e.g. nominal time) with
  plotting of observed data points using actual time.

- `VPC Plots with Exact Bins` vignette demonstrating use case for
  `plot_vpc_exactbins()` and workflow leveraging `df_mrgsim_replicate`.
