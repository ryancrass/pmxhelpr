
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pmxhelpr <a href="https://ryancrass.github.io/pmxhelpr/"><img src="man/figures/logo.png" align="right" height="139" alt="pmxhelpr website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/ryancrass/pmxhelpr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ryancrass/pmxhelpr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of pmxhelpr is to make pharmacometrics workflows more
standardized, efficient, and reproducible. This package provides helper
and wrapper functions for common steps in the modeling analysis
workflow, outside of model parameter estimation. Currently, workflows
cover key steps in exploratory data analysis and model evaluation.

## Documentation

Full narrative documentation, including worked examples for every
plotting workflow, lives on the [pmxhelpr
website](https://ryancrass.github.io/pmxhelpr/). Start with the
articles:

- [Getting Started with
  pmxhelpr](https://ryancrass.github.io/pmxhelpr/articles/getting-started.html)
  — a lean tour of every core workflow in one sitting.
- [Exploratory Analyses of PK and PK/PD
  Data](https://ryancrass.github.io/pmxhelpr/articles/eda-pk-pkpd-workflow.html)
- [Dose-Proportionality
  Workflow](https://ryancrass.github.io/pmxhelpr/articles/doseprop-workflow.html)
- [Goodness-of-Fit
  Diagnostics](https://ryancrass.github.io/pmxhelpr/articles/gof-diagnostics.html)
- [Visual Predictive Check
  Workflow](https://ryancrass.github.io/pmxhelpr/articles/vpc-workflow.html)
- [Plot Themes and
  Aesthetics](https://ryancrass.github.io/pmxhelpr/articles/plot-themes.html)

## Installation

You can install the most recent tagged version of pmxhelpr from
[GitHub](https://github.com/ryancrass/pmxhelpr/releases/latest) with:

``` r
# install.packages("devtools")
devtools::install_github("ryancrass/pmxhelpr@v0.5.0")
```

The README examples use a few packages from `Suggests` that aren’t
installed automatically:

``` r
install.packages(c("dplyr", "ggplot2", "forcats", "patchwork"))
```

## Function Naming Conventions

Exported functions follow a `ReturnType_Purpose` naming convention where
the prefix indicates what the function returns:

- `plot_*` — returns a `ggplot` object
- `df_*` — returns a `data.frame`
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

### Model Evaluation

- Overlay Goodness-of-Fit Diagnostics:
  - `plot_gof()` — observed, population-, and individual-predicted
    values overlaid versus time
- Visual Predictive Check (VPC):
  - `df_mrgsim_replicate()` — simulated replicates of an input dataset
    via `mrgsolve`
  - `df_vpcstats()` — VPC summary statistics
  - `plot_vpc_cont()` — VPC plot for continuous data range from
    simulated data with exact time bins
  - `plot_vpc_cens()` — VPC plot for censored data range from simulated
    data with exact time bins
  - `plot_vpc_legend()` — legend for a VPC plot

### Theme System

Plot aesthetics are controlled through theme factories and element
constructors. Each plot function has a corresponding `plot_*_theme()`
factory:

- `plot_dvtime_theme()`, `plot_dvconc_theme()`, `plot_doseprop_theme()`,
  `plot_gof_theme()`, `plot_vpc_theme()` — return named lists of default
  element objects for the `theme` argument of their corresponding plot
  function
- `plot_vpc_shown()` / `plot_gof_shown()` — control which VPC / GOF
  layers are visible via the `shown` argument

Element constructors (`pmx_*`) create typed objects that map to
`ggplot2` geom aesthetics:

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

### S3 Class System

`pmxhelpr` returns class-tagged objects for both stats outputs and theme
building blocks, with predicates and `print()`/`summary()` methods for
interactive inspection and programmatic validation.

- Stats classes — `df_vpcstats()` returns a `vpc_stats` list and
  `df_doseprop()` returns a `doseprop_stats` data.frame. Each carries
  `print()`, `summary()`, and `as.data.frame()` methods plus a predicate
  (`is_vpc_stats()`, `is_doseprop_stats()`).
- Element / theme classes — every `pmx_*()` element and `plot_*_theme()`
  factory is class-tagged (`pmx_element` and `pmx_theme` shared, plus
  per-type tags). Predicates `is_pmx_element()`, `is_pmx_theme()`, and
  per-type variants (`is_pmx_point()`, `is_plot_dvtime_theme()`, …) test
  class membership; `print()` methods render the type and set fields.
- Dual-mode plotting — `plot_vpc_cont()` and `plot_doseprop()` accept
  either raw input data or a precomputed stats object. The precomputed
  path skips the summarization / regression refit, enabling compute-once
  / replot-many workflows (e.g. flipping `pcvpc = TRUE/FALSE`, varying
  `theme`, trying different `min_bin_count`). The lower-level renderers
  `plot_build_vpc()` / `plot_build_doseprop()` are exported for custom
  workflows that produce class-compatible objects outside `pmxhelpr`.

`pmxhelpr` returns a class tagged object for VPC plot outputs to warn
users via the `+.pmx_vpc_plot` method when `facet_*()` layers are added
outside the returned object directing users to the correct
stratification method using the `strat_var` argument.

See the [Plot Themes and
Aesthetics](https://ryancrass.github.io/pmxhelpr/articles/plot-themes.html#inspecting-and-validating-themes)
and
[VPC](https://ryancrass.github.io/pmxhelpr/articles/vpc-workflow.html) /
[Dose-Proportionality](https://ryancrass.github.io/pmxhelpr/articles/doseprop-workflow.html)
workflow articles for worked examples.

### Vectorized Helpers

- `var_addn()` — factor labels with counts of unique identifiers per
  group
- `var_predcorr()` — prediction-corrected values
- `var_dosenorm()` — dose-normalized values

### `mrgsolve` Wrappers

- `model_mread_load()` — reads internal package model files returning an
  `mrgmod` object
- `df_mrgsim_replicate()` — replicates input data via simulation
  returning a `data.frame`
- `df_mrgsim_addpred()` — adds population model predictions (PRED) to
  the returned `data.frame`

### Internal Datasets

- `data_sad` — single ascending dose (SAD) study with parallel food
  effect cohort formatted for NLME modeling
- `data_sad_nca` — PK parameters derived using non-compartmental
  analysis (NCA)
- `data_sad_pkfit` — `data_sad` with individual (IPRED) and population
  (PRED) PK model predictions

**Note on bundled data.** `data_sad` and `data_sad_pkfit` use `ODV`
(original DV), to note that it is provided in original units from the
source data. Functions like `plot_dvtime()` default `dv_var = DV` to
match the standard NONMEM convention; when running examples on the
bundled data, one may derive a variable `DV` or pass the argument
`dv_var = "ODV"` (as every example below does).

## Example Exploratory Data Analysis Workflow

This is a basic example which illustrates a simple exploratory data
analysis workflow using pmxhelpr:

``` r
library(pmxhelpr)
library(dplyr)
library(ggplot2)
library(forcats)

#Read internal analysis-ready dataset for an example Phase 1 study
glimpse(data_sad)

#Pre-process data for plotting
data <- data_sad %>%
  mutate(Food = ifelse(FOOD == 1, "Fed", "Fasted"),
         DoseFood = paste(DOSE,"mg x1", Food),
         Regimen = var_addn(DoseFood, ID)) %>%
  mutate(Regimen = fct_relevel(Regimen, "50 mg x1 Fasted (n=6)", after = 1)) #Correctly order factor levels

#Plot drug concentration-time
plot_dvtime(data = filter(data, CMT == 2), dv_var = "ODV", cent = "mean_sdl",
            col_var = "Regimen", log_y = TRUE,
            theme = plot_dvtime_theme(obs_point = pmx_point(alpha = 0))) +
  labs(y = "Concentration (ng/mL)")

#Plot response versus concentration
plot_dvconc(data = filter(data, CMT == 3), dv_var = "CFB", ref = 0,
            col_var = "Regimen", loess = TRUE, linear = TRUE) +
  labs(y = "Response (% Change)")

#Assess dose proportionality in the fasted state
glimpse(data_sad_nca)
data_sad_nca_part1 <- filter(data_sad_nca, PART == "Part 1-SAD")

#Tabulated dose-proportionality
table <- df_doseprop(data_sad_nca_part1, metrics = c("aucinf.obs", "cmax"))
table

#Visualize dose-proportionality
plot_doseprop(table)
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
         Regimen = var_addn(DoseFood, ID)) %>%
  mutate(Regimen = fct_relevel(Regimen, "50 mg x1 Fasted (n=6)", after = 1)) #Correctly order factor levels

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
  #The input `dv_var` is preserved in the output as `OBSDV` (observed) and the
  #simulated values are written to `SIMDV` --- these are the columns `plot_vpc_cont()` reads.
  #Pass any input columns you want carried to the output via `carry_out` (numeric)
  #and `recover` (character / factor), which flow through to `mrgsolve::mrgsim_df()`.
simout <- df_mrgsim_replicate(data = data, model = model, replicates = 100,
                              dv_var = "ODV",
                              carry_out = c("DOSE", "FOOD", "BLQ", "LLOQ"),
                              recover  = c("PART", "Food"))
glimpse(simout)

#Plot output in a Censored Visual Predictive Check (VPC)

plot_obj_cens <- plot_vpc_cens(
  data = simout,
  strat_var = "DOSE"
) +
scale_x_continuous(breaks = c(0,24,72,120,168)) +
labs(y = "Proportion BLQ", x = "Time (hours)")

plot_obj_cens

#Add Legend
shown_cens <- plot_vpc_shown(obs_pi_line = FALSE, sim_pi_ci = FALSE, obs_point = FALSE)
plot_obj_cens_leg <- plot_vpc_legend(shown = shown_cens)
plot_obj_cens_leg

plot_obj_cens_wleg <- plot_obj_cens + plot_obj_cens_leg + plot_layout(heights = c(2,1))
plot_obj_cens_wleg

#Plot output in a Prediction-corrected Visual Predictive Check (VPC)
  #Exact nominal time bins present in data_sad ("NTIME") are used to plot summary statistics
  #Actual time ("TIME") is used to plot observed data points (prediction-corrected if pcvpc=TRUE)


plot_obj_food <- plot_vpc_cont(
  data = simout, 
  strat_var = "Food",
  pcvpc = TRUE
) + 
 scale_x_continuous(breaks = c(0,24,72,120,168)) +
 scale_y_log10(guide = "axis_logticks") +
 labs(y = "Pred-corrected Conc. (ng/mL)", x = "Time (hours)")

plot_obj_food

#Add Legend
plot_obj_leg <- plot_vpc_legend()
plot_obj_leg

plot_obj_food_wleg <- plot_obj_food + plot_obj_leg + plot_layout(heights = c(2,1))
plot_obj_food_wleg
```
