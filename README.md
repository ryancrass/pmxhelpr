
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pmxhelpr

<!-- badges: start -->
<!-- badges: end -->

The goal of pmxhelpr is to make pharmacometrics workflows more
standardized, efficient, and reproducible. This package provides helper
and wrapper functions for common steps in the pharmacometrics analysis
workflow, such as exploratory data analysis, model development, model
evaluation, and model application.

## Installation

You can install the development version of pmxhelpr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ryancrass/pmxhelpr")
```

## Example

This is a basic example which illusstrates a simple VPC workflow using
pmxhelpr:

``` r
library(pmxhelpr)

#Read internal analysis-ready dataset for an example Phase 1 study
glimpse(data_sad)

#Read internal mrgsolve model file
model <- model_load("model")

#Add population predictions to the dataset (note: this step is not required when using mrgsim_vpc)
data_pred <- df_add_pred(data_sad, model)

#Simulated replicates of the dataset using mrgsim (note: df_add_pred will be called within this step so data_sad can be passed in directly)
simout <- mrgsim_vpc(data = data_sad, model = model,replicates = 100,
                     output_vars = c(DV = "ODV"),
                     num_vars = c("CMT", "LLOQ", "EVID", "MDV", "WTBL", "FOOD"),
                     char_vars = c("USUBJID", "PART"))
glimpse(simout)

#Plot output in a Prediction-corrected Visual Predictive Check (VPC)
  #Exact nominal time bins present in data_sad ("NTIME") are used to plot summary statistics
  #Actual time ("TIME") is used to plot observed data points, which are also prediction-corrected if pcVPC=TRUE
vpc_exact_food <- vpc_plot_exactbins(
  sim = mutate(simout, FOOD_f = factor(FOOD, levels = c(0,1), labels = c("Fasted", "Fed"))), 
  strat_vars = "FOOD_f",
  pcvpc = TRUE,
  time_vars = c(TIME = "TIME", NTIME = "NTIME"),
  output_vars = c(PRED = "PRED", IPRED = "IPRED", SIMDV = "SIMDV", OBSDV = "OBSDV"),
  pi = c(0.05, 0.95),
  ci = c(0.05, 0.95),
  log_y = TRUE
)

vpc_exact_food
```
