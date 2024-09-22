# pmxhelpr (development version)

# pmxhelpr 0.0.2

# pmxhelpr 0.0.1

* `data_sad` NONMEM and mrgsolve analysis-ready dataset for a hypothetical SAD study with assessment of food effect.
* `model` mrgsolve model file (class `mrgmod`) in the internal pmxhelpr model library .
* `model_load` function to load internal models from the pmxhelpr model library.
* `df_mrgsim_replicate` mrgsim wrapper function to run a simulation that replicates the input dataset multiple times, intended for application to generating Visual Predictive Check (VPC) simulation-based model diagnostics.
* `plot_vpc_exactbins` vpc wrapper function to using an exact time binning variable in the input dataset (e.g. nominal time) with plotting of observed data points using actual time.
