# pmxhelpr (development version)

This is a major refactor of the package focused on simplifying function interfaces, standardizing naming conventions, and unifying the theme system across all plot families.

## Breaking Changes

### Removed Functions
* Remove `plot_dvtime_dual`. Users can compose PK and PD panels directly with `plot_dvtime` + `patchwork`, which provides independent control of each panel's arguments.
* Remove `df_addn`. Replaced by exported vectorized helper `var_addn`.
* Remove `df_pcdv`. Replaced by exported vectorized helper `var_pc`.
* Remove `df_nobsbin`. Bin count is now computed within `df_vpcstats`.

### Renamed Functions
* Rename `plot_popgof` / `plot_popgof_theme` to `plot_gof` / `plot_gof_theme`.
* Rename `plot_vpc_exactbins` to `plot_vpc_cont`.
* Rename `plot_vpclegend` to `plot_vpc_legend`.
* Rename `df_addpred` to `df_mrgsim_addpred`.
* Rename `breaks_time` to internal helper `var_timebreaks`.

### Removed Exported Functions
* `dvconc_caption` and `dvtime_caption` removed.

### Theme System Overhaul
* Replaced flat list of role and geometry-based elements with ggplot geometry-based constructor functions: `pmx_point`, `pmx_line`, `pmx_ribbon`, `pmx_errorbar`, `pmx_trend`.
* Exploratory and Diagnostic Plot theme factory keys now follow a `role_element` naming convention  
  * `plot_dvtime_theme`, theme factory for `plot_dvtime`, includes keys: `obs_point`, `obs_line`, `cent_point`, `cent_line`, `cent_errorbar`, `ref_line`, `loq_line`.
  * `plot_gof_theme`,  theme factory for `plot_gof`, includes keys: `obs_point`, `obs_line`, `cent_point`, `cent_line`, `cent_errorbar`, `cent_color`, `ref_line`, `loq_line`.
  * `plot_dvconc_theme`, theme factory for `plot_dvconc`, includes keys: `obs_point`, `ref_line`, `loess`, `linear`.
* VPC plot theme factory keys now follow an `element_statistic` naming convention aligned with the `shown` argument
  * `plot_vpc_theme`, theme factory for `plot_vpc_cont`, includes keys: `obs_point`, `obs_median_line`, `obs_pi_line`, `sim_pi_line`, `sim_pi_ci`, `sim_pi_area`, `sim_median_line`, `sim_median_ci`, `loq_line`. 

### Simplified Plot Function Arguments
* Remove `x_breaks`, `x_scale`, `x_lab`, and `y_lab` arguments from plot functions. Users add these ggplot2 layers directly to the returned plot object.
* Remove `output_colors` argument from `plot_gof`. Colors are now controlled via `pmx_color()` in `plot_gof_theme()`.
* Consolidate `plot_gof_theme` per-overlay keys (`dv_point`, `dv_line`, `pred_point`, `pred_line`, `ipred_point`, `ipred_line`) into shared `cent_point`/`cent_line` with a `pmx_color()` element for overlay colors.
* Replace `cfb` (logical) and `cfb_base` (numeric) arguments with a single `ref` argument (`NULL` = no line, numeric = draw horizontal reference line at that value) in `plot_dvtime`, `plot_gof`, and `plot_dvconc`. For example, `cfb = TRUE, cfb_base = 0` becomes `ref = 0`.

## New Features

### New Exported Functions
* `var_addn`: Vectorized helper to create factor labels with counts of unique values.
* `var_dosenorm`: Vectorized dose normalization helper.
* `var_pc`: Vectorized prediction correction helper.
* `df_vpcstats`: Exported VPC summary statistics function with integrated bin counting (replaces `df_nobsbin` dependency).
* `plot_vpc_shown`: Constructor for VPC layer visibility settings.
* `plot_vpc_legend`: Renamed from `plot_vpclegend` with updated interface.

### Theme System
* New `pmx_color` constructor controls overlay colors for DV, PRED, and IPRED in `plot_gof_theme()` (e.g., `plot_gof_theme(cent_color = pmx_color(pred = "purple"))`).
* New `plot_gof_shown` constructor for GOF layer visibility settings, paralleling `plot_vpc_shown` (e.g., `plot_gof_shown(pred = FALSE)`).
* New `pmx_style` convenience constructor applies shared aesthetics (color, alpha) to both point and line elements of a role (e.g., `plot_dvtime_theme(obs = pmx_style(alpha = 0.3))`).
* Theme factories support role-level shortcuts (`obs`, `cent`) alongside granular element-level overrides.
* `merge_theme` correctly composes `pmx_style` shortcuts with element-level overrides (style applied first, explicit overrides win).

### VPC Pipeline
* VPC pipeline refactored to remove dependency on `vpc` package.
* VPC pre-processing (variable renaming, prediction-correction) handled internally within `df_vpcstats`.
* `df_vpcstats` accepts combined simulation output directly from `df_mrgsim_replicate`.
* Add `loq` handling to VPC plots with observed quantile censoring.
* `plot_vpc_cont` inherits `loq` from `LLOQ` column in `sim` when not explicitly provided.
* `plot_vpc_cont` ignores `loq` when `pcvpc = TRUE` (LLOQ not meaningful on prediction-corrected scale).

## Internal Improvements
* Standardize NSE handling across all exported functions via `resolve_var` helper.
* Extract shared plot-building helpers: `add_cent_layers`, `add_obs_layers`, `add_blq_layers`, `add_ref_layers`, `add_trend_layers`, `init_plot`, `prep_plot_env`.
* Standardize `TRUE`/`FALSE` evaluation and error messaging across all functions.
* Centralize input validation in `utils_check.R`.
* Update `size` to `linewidth` for line geom aesthetics throughout.
* Expand test coverage from ~100 to 415+ tests.

## Documentation
* Restructure vignettes: separate PK EDA, PK/PD EDA, GOF diagnostics, VPC workflow, and plot aesthetics into dedicated vignettes with cross-links.

# pmxhelpr 0.4.0

* Rename `data_sad_pd` as `data_sad`. No longer export `data_sad_pd`. 
* Rename `model` as `pkmodel` for consistency with `pdmodel`. 
* Rename `pmxhelpr_vpc_theme` to `plot_vpc_theme` for  consistency with other plot theme functions
* Change `dvconc_caption` and `dvtime_caption` to internal functions after simplifying
* Added `cfb_base` argument to `plot_gof` (was hardcoded as yintercept = 0)
* Add helpers for central tendency handing, time variable handling, BLQ imputation, and error bar width setting to remove repetition in function calls
* Expand test coverage and refactor existing tests to reduce redundancy
* Fix bugs and typoes globally in package

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
* Fix bug that was not scaling error bar caps for the x-axis range when `cent = "median_iqer` across functions

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
* Added new internal package dataset `data_sad_nca` for use with dose-proportinality assessment functions
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

* `VPC Plot Aeesthetics` vignette demonstrating options for modifying plot aesthetics, creating a plot legend using `plot_legend()`, and adding the VPC plot and legend objects together into a single object using the `patchwork` package

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

* `VPC Plots with Exact Bins` vignette demostrating use case for `plot_vpc_exactbins()` and workflow leveraging `df_mrgsim_replicate`.
