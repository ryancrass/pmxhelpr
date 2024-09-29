
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pmxhelpr <a href="https://ryancrass.github.io/pmxhelpr/"><img src="man/figures/logo.png" align="right" height="139" alt="pmxhelpr website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/ryancrass/pmxhelpr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ryancrass/pmxhelpr/actions/workflows/R-CMD-check.yaml)
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

## Function Naming Conventions

Functions in this package use the following naming conventions:

- Wrapper functions: *ReturnObject*\_*WrappedFunction*\_*Purpose*
  - `model_mread_load()` wraps `mrgsolve::mread_cache()` to read
    internal package model files returning an `mrgmod` object.
  - `df_mrgsim_addpred()` wraps `mrgsolve::mrgsim()` including
    `mrgsolve::zero_re()` to add population predictions (PRED) to the
    input data returning a `data.frame`.
  - `df_mrgsim_replicate()` wraps `mrgsolve::mrgsim()` to replicate the
    input data returning a `data.frame`.
  - `plot_vpc_exactbins()` wraps `vpc::vpc()` to generate a VPC plot
    using exact time bins returning a `ggplot` object.
- Helper functions: *ReturnObject*\_*Purpose*
  - `df_nobsbin` returns a summary `data.frame` with counts of the
    number of missing and non-missing observations per bin.
  - `df_pcdv` returns a `data.frame` containing the prediction-corrected
    dependent variable.
  - `plot_legend` returns a `ggplot` object containing a legend for a
    VPC plot generated using `plot_vpc_exactbins`

## Example Visual Predictive Check Workflow

This is a basic example which illustrates a simple VPC workflow using
pmxhelpr:

``` r
library(pmxhelpr)
library(dplyr)
library(ggplot2)
library(mrgsolve)
library(vpc)
library(patchwork)
library(withr)

#Read internal analysis-ready dataset for an example Phase 1 study
glimpse(data_sad)

#Read internal mrgsolve model file
model <- model_mread_load("model")

#Simulated replicates of the dataset using mrgsim 
simout <- df_mrgsim_replicate(data = data_sad, model = model,replicates = 100,
                              output_vars = c(DV = "ODV"),
                              num_vars = c("CMT", "LLOQ", "EVID", "MDV", "WTBL", "FOOD"),
                              char_vars = c("USUBJID", "PART"))
glimpse(simout)

#Plot output in a Prediction-corrected Visual Predictive Check (VPC)
  #Exact nominal time bins present in data_sad ("NTIME") are used to plot summary statistics
  #Actual time ("TIME") is used to plot observed data points, which are also prediction-corrected if pcvpc=TRUE
  #LLOQ in the dataset is 1 ng/mL
  
plot_obj_food <- plot_vpc_exactbins(
  sim = mutate(simout, FOOD_f = factor(FOOD, levels = c(0,1), labels = c("Fasted", "Fed"))), 
  strat_vars = "FOOD_f",
  pcvpc = TRUE,
  time_vars = c(TIME = "TIME", NTIME = "NTIME"),
  output_vars = c(PRED = "PRED", IPRED = "IPRED", SIMDV = "SIMDV", OBSDV = "OBSDV"),
  pi = c(0.05, 0.95),
  ci = c(0.05, 0.95),
  loq = 1,
  log_y = TRUE
)

plot_obj_food

#Add Legend
plot_obj_leg <- plot_legend(pi = c(0.05, 0.95), ci = c(0.05, 0.95))
plot_obj_leg

plot_obj_food_wleg <- plot_obj_food + plot_obj_leg + plot_layout(heights = c(2,1))
plot_obj_food_wleg
```
