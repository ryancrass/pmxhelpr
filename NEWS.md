# pmxhelpr 0.1.1

* Added dependency: `patchwork`
* Vignettes copy-edited

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
