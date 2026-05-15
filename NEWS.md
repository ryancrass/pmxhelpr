# pmxhelpr (development version)

This is a major refactor of the package focused on simplifying function interfaces, standardizing naming conventions, unifying the theme system across all plot families, and introducing class-tagged objects for both stats outputs and theme building blocks, with predicates and `print()`/`summary()` methods for interactive inspection and programmatic validation.

## Breaking Changes

### Removed Functions
* Remove `plot_dvtime_dual`. Users can compose PK and PD panels directly with `plot_dvtime` + `patchwork`, which provides independent control of each panel's arguments.
* Remove `df_addn`. Replaced by exported vectorized helper `var_addn`.
* Remove `df_pcdv`. Replaced by exported vectorized helper `var_predcorr`.
* Remove `df_nobsbin`. Bin count is now computed within `df_vpcstats`.

### Renamed Functions
* Rename `plot_popgof` / `plot_popgof_theme` to `plot_gof` / `plot_gof_theme`.
* Rename `plot_vpc_exactbins` to `plot_vpc_cont`.
* Rename `plot_vpclegend` to `plot_vpc_legend` (interface updated; see usage docs).
* Rename `df_addpred` to `df_mrgsim_addpred`.
* Rename `breaks_time` to internal helper `var_timebreaks`.

### Behavior Changes
* `plot_doseprop()` default point appearance changed from filled black circles (ggplot2 default) to open circles at `alpha = 0.7`, matching the package design language. Pass `theme = plot_doseprop_theme(obs_point = pmx_point(shape = 19, size = 1.5, alpha = 1, color = "black"))` to restore prior appearance.
* `df_doseprop()` returns a `pmx_stats` container (subclass `doseprop_stats`) instead of a plain `data.frame`. The container has three slots: `stats` (the per-metric regression body — `Intercept`, `Power`, `LCL`, `UCL`, `PowerCI`, etc., previously the entire return), `obs` (filtered observation rows for the scatter overlay), and `config` (regression configuration `metric_name_var`, `metric_value_var`, `dose_var`, `ci`, `method`). Code that treated the result as a flat frame should use `as.data.frame(result)` or `result$stats`.
* `pmx_*()` element constructors (`pmx_point`, `pmx_line`, `pmx_ribbon`, `pmx_errorbar`, `pmx_trend`, `pmx_style`, `pmx_color`) now return objects with class `c("pmx_<type>", "pmx_element")` (was `"pmx_<type>"`). Code that asserts class equality with `==` should switch to `inherits()`; `expect_s3_class()`-based tests are unaffected.
* `plot_*_theme()` factories (`plot_dvtime_theme`, `plot_gof_theme`, `plot_dvconc_theme`, `plot_doseprop_theme`, `plot_vpc_theme`) now return objects with class `c("plot_<type>_theme", "pmx_theme")` (was a plain unclassed list). Code that asserts the result is a plain `list` will now see the additional class tags but will continue to work for any operation that treats it as a list (named-element access, iteration).
* `plot_gof()` now applies the `EVID == 0` filter via `df_prep_dvtime()` (was previously letting dose rows through silently when callers forgot to pre-filter).
* `errorbar_width()` warns and returns `NA_real_` instead of aborting on empty / all-NA NTIME input.

### Removed Exported Functions
* `dvconc_caption` and `dvtime_caption` removed.

### Theme System Overhaul
* Replaced flat list of role and geometry-based elements with ggplot geometry-based constructor functions: `pmx_point`, `pmx_line`, `pmx_ribbon`, `pmx_errorbar`, `pmx_trend`.
* Exploratory and Diagnostic Plot theme factory keys now follow a `role_element` naming convention  
  * `plot_dvtime_theme`, theme factory for `plot_dvtime`, includes keys: `obs_point`, `obs_line`, `cent_point`, `cent_line`, `cent_errorbar`, `ref_line`, `loq_line`.
  * `plot_gof_theme`,  theme factory for `plot_gof`, includes keys: `obs_point`, `obs_line`, `cent_point`, `cent_line`, `cent_errorbar`, `cent_color`, `ref_line`, `loq_line`.
  * `plot_dvconc_theme`, theme factory for `plot_dvconc`, includes keys: `obs_point`, `ref_line`, `loess`, `linear`.
  * `plot_doseprop_theme`, theme factory for `plot_doseprop`, includes keys: `obs_point`, `linear`.
* VPC plot theme factory keys now follow an `element_statistic` naming convention aligned with the `shown` argument
  * `plot_vpc_theme`, theme factory for `plot_vpc_cont`, includes keys: `obs_point`, `obs_median_line`, `obs_pi_line`, `sim_pi_line`, `sim_pi_ci`, `sim_pi_area`, `sim_median_line`, `sim_median_ci`, `loq_line`. 

### Simplified Plot Function Arguments
* Remove `x_breaks`, `x_scale`, `x_lab`, and `y_lab` arguments from plot functions. Users add these ggplot2 layers directly to the returned plot object.
* Remove `output_colors` argument from `plot_gof`. Colors are now controlled via `pmx_color()` in `plot_gof_theme()`.
* Replace `cfb` (logical) and `cfb_base` (numeric) arguments with a single `ref` argument (`NULL` = no line, numeric = draw horizontal reference line at that value) in `plot_dvtime`, `plot_gof`, and `plot_dvconc`. For example, `cfb = TRUE, cfb_base = 0` becomes `ref = 0`.
* `plot_gof()` BLQ imputation is now scoped by the new `blq_mode = c("obs", "all")` argument (default `"obs"`). Default behavior now imputes the observed `DV` layer only — predictions follow the model's natural decay through and below the LLOQ. Pass `blq_mode = "all"` to opt into the prior pred-censoring behavior (useful when matching the GOF visual to an estimation engine that censored predictions to LLOQ).

## New Features

### Unified Stats Container (`pmx_stats`)
* `df_vpcstats()` and `df_doseprop()` now return a shared `pmx_stats` container — a class-tagged list with three slots: `stats` (the per-row summary frame), `obs` (observation overlay), and `config` (run configuration). The class vector is `c(<subclass>, "pmx_stats")` where `<subclass>` is `vpc_stats` or `doseprop_stats`. Builders read columns from `$stats` / `$obs` and configuration from `$config` (e.g. `result$config$loq` instead of `attr(result$stats, "loq")`).
* Constructor `pmx_stats(stats, obs, config, subclass)` and validator `validate_pmx_stats(x)` back the class. Adding a future stats pipeline reuses this constructor and the base S3 methods.
* Base S3 methods: `print` / `summary` show object dimensions, slot row counts, and the run-config keys; `as.data.frame()` returns `$stats` as a plain `data.frame`. Subclasses (`vpc_stats`, `doseprop_stats`) override `print` / `summary` for richer output.
* `is_pmx_stats(x, strict = FALSE)`, `is_vpc_stats(x, strict = FALSE)`, `is_doseprop_stats(x, strict = FALSE)` predicates. With `strict = TRUE`, each runs the corresponding validator and returns `FALSE` on structural failure. Plot builders (`plot_build_vpc()`, `plot_build_doseprop()`) call the validators directly at entry.

### New Exported Functions
* `var_addn`: Vectorized helper to create factor labels with counts of unique values.
* `var_dosenorm`: Vectorized dose normalization helper.
* `var_predcorr`: Vectorized prediction correction helper.
* `df_vpcstats`: Exported VPC summary statistics function. Takes raw simulation output (e.g. from `df_mrgsim_replicate()`) and returns a `vpc_stats` / `pmx_stats` container (see "Unified Stats Container" above). Bin counting is integrated, replacing the previous `df_nobsbin` dependency.
* `plot_vpc_shown`: Constructor for VPC layer visibility settings.
* `plot_build_vpc`: Public renderer that builds a VPC ggplot from any `vpc_stats` container. Most users still go through `plot_vpc_cont()`; call `plot_build_vpc()` directly when working from a manually-constructed or cached `vpc_stats` object (e.g. external preprocessing, custom flavors, snapshot fixtures). `strat_var` accepts bare names or strings and inherits from `compute_out$config$strat_var` when not passed; `loq` similarly inherits from `compute_out$config$loq` when omitted (pass `loq = NULL` explicitly to suppress the reference line).
* `plot_build_doseprop`: Public renderer that builds a dose-proportionality ggplot from any `doseprop_stats` container. Most users still go through `plot_doseprop()`; call `plot_build_doseprop()` directly when working from a manually-constructed or cached `doseprop_stats` object. The required column-name strings, observation rows, and `ci` level are recovered from the object's `$obs` and `$config` slots; the only plotting-time arguments are `theme` and `se`.
* `vpc_stats` and `doseprop_stats` subclass-specific S3 print and summary methods: `print()` shows a focused summary (dimensions, run-config values, column groups for VPC / per-metric stats body for doseprop); `summary()` is more compact (no head preview for VPC; one line per metric for doseprop using `PowerCI` + `Interpretation`).
* `is_pmx_element`: Predicate for the shared `pmx_element` class returned by every `pmx_*()` element constructor. For specific-type checks (e.g. `pmx_point` vs. `pmx_line`), use `inherits(x, "pmx_point")`.
* `is_pmx_theme(x, strict = FALSE)`: Predicate for the shared `pmx_theme` class returned by every `plot_*_theme()` factory. With `strict = TRUE`, additionally validates that every named entry of the theme is a `pmx_element`. For specific-type checks, use `inherits(x, "plot_vpc_theme")` (or whichever subclass).
* S3 print methods on the `pmx_element` and `pmx_theme` classes: `pmx_*()` element values render as `<pmx_<type>>` banner + inline `name = value` field listing; `plot_*_theme()` values render as a `<plot_<type>_theme>` banner + one line per theme key showing the inner element type and its set fields. Replaces the default list-with-class-attribute REPL dump.

### Theme System
* New public composition API for themes:
  * `pmx_theme(elements, subclass = NULL)` — public factory for partial themes. Takes a named list of `pmx_element` objects and a class tag; validates and compacts. Used internally by every `plot_*_theme()` factory and externally to build override bundles for the `+` operator.
  * `+.pmx_theme(a, b)` — left-side class wins; merges `b`'s entries onto `a`. `theme + NULL` returns `theme` unchanged. Errors when the right side is not a `pmx_theme`.
  * `+.pmx_element(a, b)` — same-subclass requirement (so `pmx_style + pmx_point` errors); merges `b`'s set fields onto `a`. `element + NULL` returns `element` unchanged.
  * `is_pmx_theme(x, strict = FALSE)` — `strict = TRUE` validates that every named entry is a `pmx_element`.
* New `plot_doseprop_theme` constructor for `plot_doseprop` aesthetics with keys `obs_point` and `linear` (plus `obs` role shortcut). `plot_doseprop()` now accepts a `theme` argument; previously its appearance was not customizable.
* New `pmx_color` constructor controls overlay colors for DV, PRED, and IPRED in `plot_gof_theme()` (e.g., `plot_gof_theme(cent_color = pmx_color(pred = "purple"))`).
* New `plot_gof_shown` constructor for GOF layer visibility settings, paralleling `plot_vpc_shown` (e.g., `plot_gof_shown(pred = FALSE)`).
* New `pmx_style` convenience constructor applies shared aesthetics (color, alpha) to both point and line elements of a role (e.g., `plot_dvtime_theme(obs = pmx_style(alpha = 0.3))`).
* Theme factories support role-level shortcuts (`obs`, `cent`) alongside granular element-level overrides.
* `merge_theme` correctly composes `pmx_style` shortcuts with element-level overrides (style applied first, explicit overrides win).

### VPC Pipeline
* VPC pipeline refactored to remove dependency on `vpc` package.
* VPC pipeline restructured into three internal stages with two thin user-facing wrappers:
  1. `df_vpcpreprocess()` — validates inputs, resolves `loq` (inheritance from `LLOQ` column) and `mode` (`"auto"` resolution), filters `EVID == 0`, standardizes column names, and applies BLQ encoding via `var_loqcens`.
  2. `df_vpccompute()` — applies prediction-correction (when `pcvpc = TRUE`), computes the two-stage simulated quantile summary plus observed quantiles, masks `-Inf` BLQ artifacts to `NA`, and builds the observation overlay subset. Returns `list(stats, obs)`.
  3. `plot_build_vpc()` — applies the `min_bin_count` filter, draws ribbons / lines, overlays observed scatter, draws the LOQ reference line, applies stratification facets, and adds the replicates caption + panel theme.
  `df_vpcstats()` is a wrapper around stages 1–2; `plot_vpc_cont()` is a wrapper around all three. The previous internal helpers `vpc_pipeline()` and `vpc_build_plot()` are removed.
* `plot_vpc_cont()` now accepts the container returned by `df_vpcstats()` directly (in addition to raw simulation data). Pass a precomputed `df_vpcstats()` result to skip the preprocess + compute steps and re-plot the same data with different `min_bin_count` / `shown` / `theme` settings without paying the summarization cost again. Plot-only arguments (`min_bin_count`, `show_rep`, `shown`, `theme`, `pcvpc`) are honored on both paths; pipeline arguments (`strat_var`, `loq`, `mode`, `pi`, `ci`, column-name args) cannot be honored on the precomputed path and abort with a clear error pointing back at `df_vpcstats()`.
* `df_vpcstats()` always emits both standard and prediction-corrected statistics in a single call. The `stats` data.frame gains 12 `pc_*`-prefixed columns (3 obs + 9 sim quantile CIs) alongside the existing std-mode columns; `obs_n / obs_n_blq / obs_prop_blq / sim_prop_blq_* / ci / pi_low / pi_hi` are not duplicated (counts and run-config are flavor-independent; `sim_prop_blq_*` is std-only because LOQ has no meaning on the prediction-corrected scale). The `obs` data.frame gains a `PC_OBSDV` column alongside the existing `OBSDV`. Users can now compute summary statistics once and re-plot under either VPC flavor by toggling `plot_vpc_cont(out, pcvpc = ...)`.
* `df_vpcstats` accepts combined simulation output directly from `df_mrgsim_replicate`.
* Add `loq` handling to VPC plots with observed quantile censoring.
* `plot_vpc_cont` inherits `loq` from `LLOQ` column in `data` when not explicitly provided.
* `plot_vpc_cont` ignores `loq` when `pcvpc = TRUE` (LLOQ not meaningful on prediction-corrected scale).
* `plot_vpc_cont` warns when `loq` is inherited from the `LLOQ` column and `pcvpc = TRUE`, noting that BLQ censoring is applied before prediction-correction and that no LOQ reference line is drawn on the PC scale. Pass `loq` explicitly to acknowledge and suppress the warning.
* Unified BLQ pipeline: all censoring (`MDV == 1`, `is.na(OBSDV)`, `OBSDV < loq`) is applied in `df_vpcpreprocess` via `var_loqcens`. In pcVPC mode, encoded `-Inf` values are converted to `NA` before `var_predcorr` runs. `df_vpcstats` no longer dispatches on `loq` and always uses `stats::quantile(na.rm = TRUE)`. Std-VPC observed quantiles below LOQ are returned as `-Inf` from `df_vpcstats` and converted to `NA_real_` by a new `var_infna` helper before plotting. Std-VPC simulated quantiles are unaffected by `loq` (BLQ encoding applied to OBSDV only).
* Per-row LLOQ inheritance: when `loq = NULL` and the input dataset's `LLOQ` column carries multiple unique values (e.g., assay update mid-study, pooled multi-cohort dataset), each observation is censored against its own row's `LLOQ`. The unique non-NA values are exposed via `config$loq` (a numeric vector) and made available to downstream consumers. Replaces the prior "scalar-or-refuse" inheritance that warned and silently disabled BLQ handling on multi-valued `LLOQ`.
* Per-facet LLOQ reference lines: when a stratified VPC plot has per-row `LOQ` available, `plot_build_vpc()` draws one ref line per facet using the `(strat_var × LOQ)` distinct combinations from `compute_out$obs`. Each facet shows only the LLOQ(s) applicable to its strat-level rows (e.g., Part 1 shows LLOQ = 1, Part 2 shows LLOQ = 2). Falls back to global ref lines when unstratified or when the container lacks `obs$LOQ`.
* `plot_vpc_legend()` now accepts vector `lloq` — one legend entry is registered per unique LLOQ value, formatted as `LLOQ = <value>` and rendered with the theme's `loq_line$linetype` (was hard-coded `"solid"`). Pass `compute_out$config$loq` directly to mirror the multi-LLOQ ref-line set on the VPC plot.
* `plot_build_vpc()` no longer attaches an LLOQ legend internally; the legend is now built in `plot_vpc_legend()`. The visual reference line(s) on the main plot panel are unchanged. Composing `plot_build_vpc(out) + plot_vpc_legend(lloq = out$config$loq)` via `patchwork` produces the prior single-figure layout.
* `var_loqcens` rewritten as a vector encoder (`var_loqcens(x, loq, mdv)`) returning `-Inf` at BLQ positions; previously computed a censored quantile.
* `df_vpcstats` summary statistics column names harmonized to a `<role>_<low|med|hi>` scheme. Per-bin counts are `obs_n` and `obs_n_blq` (previously `nbin` / `nobsblq`; the `nobs` column was removed and `nmiss` previously renamed to `nobsblq`). Simulated quantile CIs are `sim_low_low/med/hi`, `sim_med_low/med/hi`, `sim_hi_low/med/hi` (previously `q5_*`, `q50_*`, `q95_*`); observed quantiles are `obs_low/med/hi` (previously `obs5/50/95`). When `loq` is supplied, the result also includes `sim_prop_blq_low/med/hi`. The `min_bin_count` filter gates on `obs_n - obs_n_blq` (quantifiable observations). Trailing `ci`, `pi_low`, `pi_hi` columns echo the configuration so consumers can interpret the `_low/_med/_hi` CI suffix and the `sim_low_*` / `sim_hi_*` PI prefix groups.
* `plot_vpc_cont` and `df_vpcpreprocess` accept a new `mode` argument (`"auto"` (default), `"rank"`, or `"drop"`) that controls how BLQ-encoded values flow into quantile aggregation. `"rank"` keeps `-Inf` encoding so BLQ rows rank low at `stats::quantile`; fully-censored bins return `-Inf` and are masked to `NA` before plotting. `"drop"` converts BLQ to `NA` so those rows are excluded from quantile computation entirely. `"auto"` resolves to `"rank"` for std VPC and `"drop"` for pcVPC, matching prior package behavior — no numerical change for existing users.

### Dose-Proportionality Pipeline
* `plot_doseprop()` now accepts the `doseprop_stats` container returned by `df_doseprop()` directly (in addition to raw observation data). Pass a precomputed `df_doseprop()` result to skip the regression refit and re-plot the same data with different `theme` / `se` settings without paying the fitting cost again. Plot-only arguments (`theme`, `se`) are honored on both paths; pipeline arguments (`metrics`, `metric_name_var`, `metric_value_var`, `dose_var`, `method`, `ci`, `sigdigits`) cannot be honored on the precomputed path and abort with a clear error pointing back at `df_doseprop()`. Mirrors the dual-mode pattern used by `plot_vpc_cont()`.
* `df_doseprop()` carries the plotting context in the container's `$obs` and `$config` slots so the precomputed object is self-contained: `obs` (filtered observation rows), and `config` (`metric_name_var`, `metric_value_var`, `dose_var`, `ci`, `method`). `plot_build_doseprop()` reads these to render without needing the original data.

## Internal Improvements
* The five `plot_*_theme()` factories (`plot_dvtime_theme`, `plot_gof_theme`, `plot_dvconc_theme`, `plot_doseprop_theme`, `plot_vpc_theme`) now delegate their final class-tag + element-validation step to the public `pmx_theme()` factory. Behavior is identical; the change centralizes the boilerplate so adding a new plot family no longer requires copying it. The internal `merge_theme()` / `merge_element()` / `apply_style()` helpers continue to back the operators.
* Standardize NSE handling across all exported functions via `resolve_var` helper.
* Extract shared plot-building helpers: `add_cent_layers`, `add_obs_layers`, `add_blq_layers`, `add_ref_layers`, `add_trend_layers`, `init_plot`, `prep_plot_env`.
* Standardize `TRUE`/`FALSE` evaluation and error messaging across all functions.
* Centralize input validation in `utils_check.R`.
* Update `size` to `linewidth` for line geom aesthetics throughout.
* Expand test coverage from ~100 to 510+ tests.
* VPC pipeline column-group registry: internal vectors (`.vpc_count_cols`, `.vpc_blq_cols`, `.vpc_obs_quantile_cols`, `.vpc_sim_quantile_cols`, `.vpc_meta_cols`) replace the duplicated column lists previously inlined inside `df_vpccompute()` (relocate, rename_with, var_infna across-mutate). Adding a new column group is now a single-site change.
* Centralize the bin-variable name as the internal constant `BIN_MID_VAR` instead of repeating the `"BIN_MID"` literal across `df_vpcpreprocess`, `df_vpccompute`, `df_vpcstats`, `plot_vpc_cont`, and `plot_build_vpc`.
* Internal `validate_pmx_stats()` helper checks the structural shape of the shared base class (`stats`/`obs`/`config` slots, types). The subclass validators `validate_vpc_stats()` and `validate_doseprop_stats()` delegate to it and add their own column / config-key checks. Called at the top of `plot_build_vpc()` and `plot_build_doseprop()`, surfacing clear errors before plotting deep-fails inside `aes()`.
* Unified panel theme application across all three plot families via internal `apply_panel_theme(plot, white_panel)` helper. All families blank `panel.grid.minor` and `panel.grid.major.x`; the VPC family additionally passes `white_panel = TRUE` for the white-background variant. Replaces three separate inline `theme()` blocks with a single helper. No visual change.
* Direct unit tests for the internal `compute_vpcstat()` helper (column-group output, `loq = NULL` drops `sim_prop_blq_*`, `pcvpc` flag drives BLQ-detection branch, `var_infna` is applied, `obs_n` matches records-per-bin).

## Documentation
* Restructure vignettes: combined PK and PK/PD exploratory analyses, GOF diagnostics, VPC workflow, dose-proportionality workflow, and plot themes & aesthetics into dedicated vignettes with cross-links.
* New `Dose-Proportionality Workflow` vignette covering the `df_doseprop()` / `plot_build_doseprop()` / `plot_doseprop()` pipeline, the `doseprop_stats` / `pmx_stats` class system with `print()` / `summary()` / `as.data.frame()` methods, the dual-mode precomputed-stats path, and theme customization. The exploratory PK/PD vignette cross-links to this dedicated workflow rather than embedding a dose-proportionality section.
* `Plot Themes and Aesthetics` vignette gains an "Inspecting and Validating Themes" section covering the `pmx_element` / `pmx_theme` S3 system (`print()` output, the `is_pmx_element()` / `is_pmx_theme()` predicates, and `inherits()`-based specific-type checks) and a "Composing themes with `+`" section showing partial-theme construction via `pmx_theme()` and `+.pmx_theme` / `+.pmx_element` composition.
* `Visual Predictive Check Workflow` vignette gains an "Inspecting the `vpc_stats` object" section demonstrating `print()`, `summary()`, `as.data.frame()`, and `is_vpc_stats()` on the `df_vpcstats()` return, and a "Pipeline Column Contracts" reference section documenting the input/output column expectations of `df_mrgsim_replicate()` → `df_vpcstats()` → `plot_build_vpc()`.
* Move all narrative vignettes to the pkgdown site (`https://ryancrass.github.io/pmxhelpr/articles/`). The package no longer ships built vignettes in `inst/doc/`, which keeps the installed package small. Content is unchanged.

# pmxhelpr 0.4.0

* Rename `data_sad_pd` as `data_sad`. No longer export `data_sad_pd`. 
* Rename `model` as `pkmodel` for consistency with `pdmodel`. 
* Rename `pmxhelpr_vpc_theme` to `plot_vpc_theme` for  consistency with other plot theme functions
* Change `dvconc_caption` and `dvtime_caption` to internal functions after simplifying
* Added `cfb_base` argument to `plot_gof` (was hardcoded as yintercept = 0)
* Add helpers for central tendency handing, time variable handling, BLQ imputation, and error bar width setting to remove repetition in function calls
* Expand test coverage and refactor existing tests to reduce redundancy
* Fix bugs and typos globally in package

# pmxhelpr 0.3.9

* Add NSE support to accept bare column names in all exported functions (e.g., dv_var = DV) in addition
to strings (e.g., dv_var = "DV") across all exported functions that take column name arguments. Revise vignettes to highlight this functionality.
* Fix bug in `plot_vpclegend` that was resulting in alpha not being correctly applied

# pmxhelpr 0.3.8

* Fix bug in `plot_dvtime` to ensure closed circles are plotted in place of open circles for central tendency points
* Add minimum R version of 4.4.1 given use of native pipe operator (|>)

# pmxhelpr 0.3.7

* Add `plot_dvconc` to generate plots of response variables versus drug concentration with LOESS and/or linear trendlines
* Add `plot_dvtime_dual` wrapper function for `plot_dvtime` to plot two dependent variables versus time intended to support simultaneous visualization of PK and PD.
* Update `var_addn` (formerly `df_addn`) to offload factor ordering from the function to simply output. Handling can be done outside the function with alternative packages optimized for dealing with factor variables (e.g., `forcats`)
* Update `plot_gof_theme` to include separate line theme elements for individual observed lines (e.g., spaghetti plots) and central tendency of the observed.
* Revise the `Exploratory Data Analysis` vignette to include new functionality and revised workflows with new functions.
* Fix bug that was not scaling error bar caps for the x-axis range when `cent = "median_iqr"` across functions

# pmxhelpr 0.3.6

* Add `var_addn` (formerly `df_addn`) helper function to create and order factor labels including count of unique values to include counts in plot legends
* Add `plot_gof_theme` function to set and adjust default aesthetics for `plot_gof`
* Add LLOQ value and linetype to legend in `plot_dvtime`
* Add caption indicating method of BLQ imputation to `plot_dvtime`
* Fix bug in `df_pcdv` that was including missing values (MDV=1) in the median PRED calculation for nominal times with missing values, resulting in incorrect PRED-correction of observations in these bins ONLY. Simulated intervals and observed quantile lines were unaffected.
* Correct error in documentation of default number of breaks in `breaks_time`

# pmxhelpr 0.3.5

* Add option to pass the same dataset variable to `time_var` and `ntime_var` for all functions
* Add option to control default aesthetics (linewidth, linetype, size, shape, alpha) in `plot_dvtime` and `plot_gof`
* Fix bug in `plot_gof` that was preventing upper error bars from inheriting the color mapped to "DV"

# pmxhelpr 0.3.4

* Fix bug in `df_mrgsim_replicate` that was precluding use of non-default time variable names

# pmxhelpr 0.3.3

* Fix bug in `plot_dvtime` and `plot_gof` where variable for "DOSE" was being assessed when dosenorm = FALSE
* Update `breaks_time` to accept abbreviations for time units

# pmxhelpr 0.3.2

* Add `cent = "mean_sdl_upper"` option to `plot_dvtime` and `plot_gof` for plotting only the upper error bar
* Add `barwidth` argument to `plot_dvtime` and `plot_gof` to allow user to change error bar cap width. 

# pmxhelpr 0.3.1

* Add `plot_gof` to generate population overlay goodness-of-fit plots
* Add `data_sad_pkfit` NONMEM PK model fit output dataset for `data_sad` based on `model`.
* Fixed bug in `plot_dvtime` that was leading to plotting mean +/- 2*SD instead of mean +/- SD

# pmxhelpr 0.3.0

* Added new functionality for dose-proportionality assessment with `df_doseprop` and `plot_doseprop`, including helpers `mod_loglog` and `df_loglog` to perform and tabulate power law (log-log) regression of exposure versus dose
* Added new internal package dataset `data_sad_nca` for use with dose-proportionality assessment functions
* Added helper function `dvtime_caption` to generate the caption for `plot_dvtime`
* Added helper function `pmxhelpr_vpc_theme` to capture default VPC plot aesthetics `
* Revised vignettes to describe workflow with new default aesthetics
* Remove use of `log_y` argument in VPC plot examples and added rationale to primary vignette for `plot_vpc_exactbins`

* Add unit tests for all primary functions

# pmxhelpr 0.2.4

* Fix bug in `plot_dvtime` to ensure `median + IQR error bars` prints on both linear and log-scale axes when `cent = "median_iqr"`

# pmxhelpr 0.2.3

* Added `cent = "median_iqr"` option to `plot_dvtime` to plot median with inter-quartile range error bars
* Updated the `ind_dv` logical argument in `plot_dvtime` to `grp_dv` to generalize use of grouping beyond connecting points within an individual subject
* Added the `grp_var` argument to `plot_dvtime` to allow specification of the variable assigned to the group aesthetic by the user. Default is `grp_var = "ID"`

# pmxhelpr 0.2.2

* Corrected bug in `plot_vpc_exactbins` when *ordered* factor variables are pasted to the `strat_var` argument.
  Ordered factors cannot be passed to the underlying stratification function from the `vpc` package (`add_stratification()`); therefore, ordered factors will be coerced to unordered factors within `plot_vpc_exactbins`

# pmxhelpr 0.2.1

* Corrected bug in `NA` handling within `breaks_time`

# pmxhelpr 0.2.0

* Added `plot_dvtime` function for exploratory concentration-time plotting
* Added `breaks_time` function to automatically determine x-axis breaks for time
* Functionality from `breaks_time` added to `plot_vpcexactbins`

* `Exploratory Data Analysis` vignette created to demonstrate functionality of new functions

* Added dependency: `labelling`

# pmxhelpr 0.1.4

* Fixed bug leading to error when generating stratified vpcs using the `strat_var` argument

# pmxhelpr 0.1.3

* Vignette vpc plot axes updated to time unit breaks
* Vignette bullet point lists updated to correct spacing
* Coerce argument `lloq` argument of `plot_legend()` to character type

# pmxhelpr 0.1.2

* Vignettes copy edited

# pmxhelpr 0.1.1

* Added dependency: `patchwork`

# pmxhelpr 0.1.0

* Full VPC workflow now available in Version 0.1.0 with three vignettes to demonstrate this functionality! 

* `VPC Plot Aesthetics` vignette demonstrating options for modifying plot aesthetics, creating a plot legend using `plot_legend()`, and adding the VPC plot and legend objects together into a single object using the `patchwork` package

# pmxhelpr 0.0.3

* Additional functionality for `plot_vpc_exactbins`
  + `shown` argument added to facilitate customization of which layers are shown in the plot and standardize with new function `plot_legend`. Passed to `show` argument of `vpc::vpc()`
* `plot_legend` helper function to generate a legend for a VPC plot generated using `plot_vpc_exactbins()`, which can be combined with the VPC plot using the `patchwork` package

* `VPC Plots with BLQ Censoring` vignette demonstrating appropriate handling of data missing due to assay sensitivity (below the lower limit of quantification) using `plot_vpc_exactbins()` and workflow leveraging `df_mrgsim_replicate`.

# pmxhelpr 0.0.2

* Additional functionality for `plot_vpc_exactbins`
  + `loq` argument added to facilitate incorporating BLQ censoring of observed data in summary quantiles 
* Bug fix to allow passing argument `vpcdb = TRUE` to `vpc::vpc()`

# pmxhelpr 0.0.1

* `data_sad` NONMEM and mrgsolve analysis-ready dataset for a hypothetical SAD study with assessment of food effect.
* `model` mrgsolve model file (class `mrgmod`) in the internal pmxhelpr model library .
* `model_load` function to load internal models from the pmxhelpr model library.
* `df_mrgsim_replicate` mrgsim wrapper function to run a simulation that replicates the input dataset multiple times, intended for application to generating Visual Predictive Check (VPC) simulation-based model diagnostics.
* `plot_vpc_exactbins` vpc wrapper function to using an exact time binning variable in the input dataset (e.g. nominal time) with plotting of observed data points using actual time.

* `VPC Plots with Exact Bins` vignette demonstrating use case for `plot_vpc_exactbins()` and workflow leveraging `df_mrgsim_replicate`.
