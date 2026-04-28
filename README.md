
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
  - `df_mrgsim_replicate()` wraps `mrgsolve::mrgsim()` to replicate the
    input data returning a `data.frame`.
  - `plot_vpc_cont()` generates a VPC plot using exact time bins
    returning a `ggplot` object.
- Helper functions: *ReturnObject*\_*Purpose*
  - `var_addn` returns a factor vector with group labels appended with
    counts of unique values of an ID variable.
  - `df_addpred` returns a `data.frame` with population model
    predictions (PRED) appended.
  - `df_doseprop` returns a `data.frame` containing parameters from
    log-log regression of multiple exposure metrics versus dose
  - `df_loglog` returns a `data.frame` containing parameters from a
    log-log regression of a single exposure metric versus dose
  - `df_pcdv` returns a `data.frame` containing the prediction-corrected
    dependent variable.
  - `mod_loglog` returns a `lm` object from a log-log regression of a
    single exposure metric versus dose
  - `plot_dvtime` returns a `ggplot` object with a dependent variable
    plotted versus time
  - `plot_doseprop` returns a `ggplot` object with log-log regression of
    exposure metrics versus dose
  - `plot_popgof` returns a `ggplot` object with observed,
    population-predicted, and individual-predicted values plotted versus
    time
  - `plot_vpc_legend` returns a `ggplot` object containing a legend for
    a VPC plot generated using `plot_vpc_cont`
- Plot theme lists
  - `plot_dvconc_theme()`, `plot_dvtime_theme()`, `plot_popgof_theme()`,
    `plot_vpc_theme()`, and `plot_vpc_shown()` return a named `list`
    with elements of modifiable elements of the plot theme or elements
    shown.
- Data sets
  - `data_sad` a dataset for a single ascending dose (SAD) study with
    parallel food effect cohort formatted for non-linear mixed effects
    (NMLE) population PK and PK/PD modeling
  - `data_sad_nca` a dataset containing PK parameters derived using
    non-compartmental analysis (NCA)
  - `data_sad_pkfit` a dataset consistent with `data_sad` including
    individual (IPRED) and population (PRED) PK model predictions

## Example Exploratory Data Analysis Workflow

This is a basic example which illustrates a simple exploratory data
analysis workflow using pmxhelpr:

``` r
#Read internal analysis-ready dataset for an example Phase 1 study
glimpse(data_sad)

#Pre-process data for plotting
data <- data_sad %>% 
  mutate(Regimen = var_addn(DOSE, ID, sep = "mg x1")) %>%
  mutate(DoseReg = fct_relevel(Regimen, "50 mg x1 (n=6)", after = 1))

#Plot drug concentration-time
plot_dvtime(data = filter(data, CMT == 2), dv_var = "ODV", cent = "mean_sdl", 
            col_var = "Regimen", ylab = "Concentration (ng/mL)", 
            log_y = TRUE, obs_dv = FALSE)

#Plot drug concentration and biomarker response versus time
plot_dvtime_dual(data = data, dv_var1 = "ODV", dv_var2 = "CFB", dvid_var = "CMT",
            cent = "mean_sdl", col_var = "Regimen", 
            ylab1 = "Drug Conc. (ng/mL)", ylab2 = "Response (% Change)", 
            log_y1 = TRUE)

#Plot response versus concentration
plot_dvconc(data = filter(data, CMT == 3), dv_var = "CFB",  
            col_var = "Regimen", ylab = "Response (% Change)", 
            loess = TRUE, linear = TRUE)

#Assess dose proportionality in the fasted state
glimpse(data_sad_nca)
data_sad_nca_part1 <- filter(data_sad_nca, PART == "Part 1-SAD")

#Tabulated dose-proportionality
table <- df_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"))
table

#Visualize dose-proportionality
plot_doseprop(data_sad_nca_part1, metrics = c("aucinf.obs", "cmax"))
```

## Example Population Overly Goodness-of-fit Plot Workflow

This is a basic example which illustrates a simple model diagnostic
workflow using pmxhelpr:

``` r
library(pmxhelpr)
library(dplyr)
library(ggplot2)
library(mrgsolve)
library(patchwork)
library(withr)

##Pre-process Data for Plotting
data <- data_sad_pkfit %>% 
  mutate(Food = ifelse(FOOD == 1, "Fed", "Fasted"), 
         DoseGroup = paste0(DOSE, " mg ", Food)) %>% 
  mutate(DoseGroup = var_addn(DoseGroup, ID),
         Food = var_addn(Food, ID))
unique(data$DoseGroup)
unique(data$Food)

data <- data %>% 
  mutate(DoseGroup = fct_relevel(DoseGroup, "50 mg Fasted (n=6)", after = 1))
unique(data$DoseGroup)

##Generate Population Overlay Goodness-of-fit Fit Plots by Food Status
plot_popgof(data = data, output_vars = c(DV ="ODV"), dosenorm = TRUE, 
            ylab = "Dose-normalized Conc. (ng/mL)") +
  facet_wrap(~Food)

plot_popgof(data = data, output_vars = c(DV ="ODV"), 
            ylab = "Dose-normalized Conc. (ng/mL)", log_y = TRUE) +
  facet_wrap(~DoseGroup)
```

## Example Visual Predictive Check Analysis Workflow

This is a basic example which illustrates a simple VPC workflow using
pmxhelpr:

``` r
library(pmxhelpr)
library(dplyr)
library(ggplot2)
library(mrgsolve)
library(patchwork)
library(withr)

#Read internal mrgsolve model file
model <- model_mread_load("pkmodel")

#Process Data
data <- data_sad%>% 
  mutate(Food = ifelse(FOOD == 1, "Fed", "Fasted")) %>% 
  mutate(Food = var_addn(Food, ID))

#Simulated replicates of the dataset using mrgsim 
simout <- df_mrgsim_replicate(data = data, model = model,replicates = 100,
                              dv_var = "ODV",
                              num_vars = c("CMT", "LLOQ", "EVID", "MDV", "WTBL", "FOOD"),
                              char_vars = c("USUBJID", "PART", "Food"))
glimpse(simout)

#Plot output in a Prediction-corrected Visual Predictive Check (VPC)
  #Exact nominal time bins present in data_sad ("NTIME") are used to plot summary statistics
  #Actual time ("TIME") is used to plot observed data points, which are also prediction-corrected if pcvpc=TRUE


plot_obj_food <- plot_vpc_cont(
  sim = simout, 
  strat_var = "Food",
  pcvpc = TRUE
) + 
  scale_y_log10(guide = "axis_logticks")

plot_obj_food

#Add Legend
plot_obj_leg <- plot_vpc_legend()
plot_obj_leg

plot_obj_food_wleg <- plot_obj_food + plot_obj_leg + plot_layout(heights = c(2,1))
plot_obj_food_wleg
```
