# pmxhelpr (development version)

# pmxhelpr 0.3.1

* Add `plot_popgof` to generate population overlay goodness-of-fit plots
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
