# pmxhelpr (development version)

# pmxhelpr 0.0.0.9000

* `data_sad` NONMEM and mrgsolve analysis-ready dataset for a hypothetical SAD study with assessment of food effect.
* `model` mrgsolve model file in the internal pmxhelpr model library .
* `model_load` function to load internal models from the pmxhelpr model library.
* `df_addpred` mrgsim wrapper function to add population predictions (PRED) to a modeling dataset.
* `df_mrgsim_replicate` mrgsim wrapper function to run a simulation that replicates the input dataset multiple times, intended for application to generating Visual Predictive Check (VPC) simulation-based model diagnostics.
* `plot_vpc_exactbins` vpc wrapper function to using an exact time binning variable in the input dataset (e.g. nominal time) with plotting of observed data points using actual time.
