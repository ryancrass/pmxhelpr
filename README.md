
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pmxhelpr <a href="https://ryancrass.github.io/pmxhelpr/"><img src="man/figures/logo.png" align="right" height="139" alt="pmxhelpr website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/ryancrass/pmxhelpr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ryancrass/pmxhelpr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of pmxhelpr is to make pharmacometrics workflows more
standardized, efficient, and reproducible. This package provides helper
and wrapper functions for common steps in the modeling analysis workflow
outside of model parameter estimation.

These include such as exploratory data analysis, model evaluation, and
model application.

## Installation

You can install the development version of pmxhelpr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ryancrass/pmxhelpr")
```

## Function Naming Conventions

Exported functions follow a `ReturnType_Purpose` naming convention where
the prefix indicates what the function returns:

- `plot_*` — returns a `ggplot` object
- `df_*` — returns a `data.frame`
- `mod_*` — returns a model object
- `var_*` — returns a vector (vectorized helpers for use inside
  `mutate`)
- `pmx_*` — returns a theme element constructor (for use with `*_theme`
  factories)

### Exploratory Data Analysis

- Longitudinal PK and PK/PD Analysis:
  - `plot_dvtime()` — dependent variable versus time (e.g.,
    concentration-time profiles)
  - `plot_dvconc()` — dependent variable versus a continuous independent
    variable (e.g., response vs concentration)
- Dose Proportionality:
  - `df_doseprop()` — log-log regression parameters for multiple
    exposure metrics versus dose
  - `plot_doseprop()` — log-log regression plots of exposure metric(s)
    versus dose
  - `df_loglog()` — log-log regression parameters for a single exposure
    metric
  - `mod_loglog()` — `lm` object from a single log-log regression

### Model Evaluation

- Overlay Goodness-of-Fit Diagnostics:
  - `plot_gof()` — observed, population-, and individual-predicted
    values overlaid versus time
- Visual Predictive Check (VPC):
  - `df_mrgsim_replicate()` — simulated replicates of an input dataset
    via `mrgsolve`
  - `df_vpcstats()` — VPC summary statistics (observed/simulated
    quantiles and confidence intervals)
  - `plot_vpc_cont()` — VPC plot from simulated data with exact time
    bins
  - `plot_vpc_legend()` — legend for a VPC plot

### Theme System

Plot aesthetics are controlled through theme factories and element
constructors. Each plot function has a corresponding `*_theme()`
factory:

- `plot_dvtime_theme()`, `plot_dvconc_theme()`, `plot_gof_theme()`,
  `plot_vpc_theme()` — return named lists of default element objects for
  the `theme` argument of their corresponding plot function
- `plot_vpc_shown()` — controls which VPC layers are visible via the
  `shown` argument

Element constructors create typed objects that map to `ggplot2` geom
aesthetics:

- `pmx_point()` — point aesthetics (shape, size, alpha, color)
- `pmx_line()` — line aesthetics (linewidth, linetype, alpha, color)
- `pmx_ribbon()` — ribbon aesthetics (fill, alpha, color, linetype,
  linewidth)
- `pmx_errorbar()` — error bar aesthetics (linewidth, linetype, alpha,
  width)
- `pmx_trend()` — trend line aesthetics (linewidth, linetype, color,
  se_color, se_alpha)
- `pmx_color()` — GOF overlay color mapping (dv, pred, ipred)
- `pmx_style()` — convenience shortcut to set shared aesthetics (color,
  alpha) on both point and line elements of a role

### Vectorized Helpers

- `var_addn()` — factor labels with counts of unique identifiers per
  group
- `var_pc()` — prediction-corrected values
- `var_dosenorm()` — dose-normalized values

### `mrgsolve` Wrappers

- `model_mread_load()` — reads internal package model files returning an
  `mrgmod` object
- `df_mrgsim_replicate()` — replicates input data via simulation
  returning a `data.frame`
- `df_mrgsim_addpred()` — adds population model predictions (PRED) to a
  dataset

### Internal Datasets

- `data_sad` — single ascending dose (SAD) study with parallel food
  effect cohort formatted for NLME modeling
- `data_sad_nca` — PK parameters derived using non-compartmental
  analysis (NCA)
- `data_sad_pkfit` — `data_sad` with individual (IPRED) and population
  (PRED) PK model predictions

## Example Exploratory Data Analysis Workflow

This is a basic example which illustrates a simple exploratory data
analysis workflow using pmxhelpr:

``` r
#Read internal analysis-ready dataset for an example Phase 1 study
glimpse(data_sad)

#Pre-process data for plotting
data <- data_sad %>% 
  mutate(Food = ifelse(FOOD == 1, "Fed", "Fasted"), 
         DoseFood = paste(DOSE,"mg x1", Food), 
         Regimen = var_addn(DoseFood, ID, sep = "mg x1")) %>%
  mutate(Regimen = fct_relevel(Regimen, "50 mg x1 (n=6)", after = 1)) #Correctly order factor levels

#Plot drug concentration-time
plot_dvtime(data = filter(data, CMT == 2), dv_var = "ODV", cent = "mean_sdl",
            col_var = "Regimen", log_y = TRUE, obs_dv = FALSE) +
  labs(y = "Concentration (ng/mL)")

#Plot response versus concentration
plot_dvconc(data = filter(data, CMT == 3), dv_var = "CFB",
            col_var = "Regimen", loess = TRUE, linear = TRUE) +
  labs(y = "Response (% Change)")

#Assess dose proportionality in the fasted state
glimpse(data_sad_nca)
data_sad_nca_part1 <- filter(data_sad_nca, PART == "Part 1-SAD")

#Tabulated dose-proportionality
table <- df_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"))
table

#Visualize dose-proportionality
plot_doseprop(data_sad_nca_part1, metrics = c("aucinf.obs", "cmax"))
```

## Example Population Overlay Goodness-of-Fit Plot Workflow

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
         DoseFood = paste(DOSE,"mg x1", Food), 
         Regimen = var_addn(DoseFood, ID, sep = "mg x1")) %>%
  mutate(Regimen = fct_relevel(Regimen, "50 mg x1 (n=6)", after = 1)) #Correctly order factor levels

##Generate Population Overlay Goodness-of-fit Fit Plots by Food Status
plot_gof(data = data, dv_var = "ODV", dosenorm = TRUE) +
  facet_wrap(~Food) +
  labs(y = "Dose-normalized Conc. (ng/mL)")

plot_gof(data = data, dv_var = "ODV", log_y = TRUE) +
  facet_wrap(~Regimen) +
  labs(y = "Concentration (ng/mL)")
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
data <- data_sad %>% 
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
