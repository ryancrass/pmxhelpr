
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
  - `plot_dvtime` returns a `ggplot` object with a dependent variable
    plotted versus time
  - `plot_popgof` returns a `ggplot` object with observed,
    population-predicted, and individual-predicted values plotted versus
    time
  - `df_nobsbin` returns a summary `data.frame` with counts of the
    number of missing and non-missing observations per bin.
  - `df_pcdv` returns a `data.frame` containing the prediction-corrected
    dependent variable.
  - `plot_vpclegend` returns a `ggplot` object containing a legend for a
    VPC plot generated using `plot_vpc_exactbins`

## Example Exploratory Data Analysis Workflow

This is a basic example which illustrates a simple exploratory data
analysis workflow using pmxhelpr:

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

#Plot data
data <- data_sad %>% 
  mutate(Dose = paste(DOSE, "mg"), 
         Dose_f = factor(Dose, levels = c("10 mg", "50 mg", "100 mg", "200 mg", "400 mg")))
         
plot_dvtime(data = data, dv_var = c(DV = "ODV"), cent = "median", col_var = "Dose_f",
            ylab = "Concentration (ng/mL)", timeu = "hours")

#Assess dose proportionality in the fasted state
glimpse(data_sad_nca)
data_sad_nca_part1 <- filter(data_sad_nca, PART == "Part 1-SAD")

#Tabulated dose-proportionality
table <- df_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"))
table

#Visualize dose-proportionality
plot_doseprop(data_sad_nca_part1, metrics = c("aucinf.obs", "cmax"))
```

## Example Visual Exploratory Data Analysis Workflow

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

#Read internal mrgsolve model file
model <- model_mread_load("model")

#Simulated replicates of the dataset using mrgsim 
simout <- df_mrgsim_replicate(data = data, model = model,replicates = 100,
                              dv_var = "ODV",
                              num_vars = c("CMT", "LLOQ", "EVID", "MDV", "WTBL", "FOOD"),
                              char_vars = c("USUBJID", "PART", "Dose_f"))
glimpse(simout)

#Plot output in a Prediction-corrected Visual Predictive Check (VPC)
  #Exact nominal time bins present in data_sad ("NTIME") are used to plot summary statistics
  #Actual time ("TIME") is used to plot observed data points, which are also prediction-corrected if pcvpc=TRUE


plot_obj_food <- plot_vpc_exactbins(
  sim = mutate(simout, FOOD_f = factor(FOOD, levels = c(0,1), labels = c("Fasted", "Fed"))), 
  strat_var = "FOOD_f",
  pcvpc = TRUE
) + 
  scale_y_log10(guide = "axis_logticks")

plot_obj_food

#Add Legend
plot_obj_leg <- plot_vpclegend()
plot_obj_leg

plot_obj_food_wleg <- plot_obj_food + plot_obj_leg + plot_layout(heights = c(2,1))
plot_obj_food_wleg
```
