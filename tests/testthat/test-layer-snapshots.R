#####Built-layer snapshots per plot family#####

## Regression net for layer construction: each snapshot records every layer's
## geom/stat, row count, fixed aesthetics, and position ranges, plus scales,
## facets, and labels (see layer_summary() in helper-plots.R). Each plot uses
## the configuration that draws the most layers in its family.

pk_dose <- dplyr::mutate(dplyr::filter(data_sad, CMT %in% c(1, 2)),
                         Dose = var_addn(DOSE, ID, sep = "mg"))
pd_dose <- dplyr::mutate(dplyr::filter(data_sad, CMT == 3),
                         Dose = var_addn(DOSE, ID, sep = "mg"))
forest_auc <- dplyr::filter(data_sad_pkforest, metric == "AUCRATIO")

test_that("plot_dvtime: colored mean +/- SD, per-row LLOQ imputation, log y", {
  p <- suppressMessages(plot_dvtime(pk_dose, dv_var = ODV, col_var = Dose,
                                    cent = "mean_sdl", loq_method = 2,
                                    log_y = TRUE))
  expect_snapshot(layer_summary(p))
})

test_that("plot_dvtime: median + IQR, dose-normalized, reference line", {
  p <- suppressMessages(plot_dvtime(pk_dose, dv_var = ODV, cent = "median_iqr",
                                    dosenorm = TRUE, ref = 1))
  expect_snapshot(layer_summary(p))
})

test_that("plot_dvconc: colored loess and linear trends with loess SE", {
  p <- suppressMessages(plot_dvconc(pd_dose, dv_var = ODV, idv_var = CONC,
                                    col_var = Dose, col_trend = TRUE,
                                    loess = TRUE, linear = TRUE,
                                    se_loess = TRUE))
  expect_snapshot(layer_summary(p))
})

test_that("plot_gof: DV/IPRED/PRED overlays, median + IQR, LLOQ method 1", {
  p <- suppressMessages(plot_gof(data_sad_pkfit, dv_var = ODV, loq = 1,
                                 loq_method = 1, cent = "median_iqr"))
  expect_snapshot(layer_summary(p))
})

test_that("plot_doseprop: power-model fits for AUC and Cmax", {
  p <- plot_doseprop(dplyr::filter(data_sad_nca, PART == "Part 1-SAD"),
                     metrics = c("aucinf.obs", "cmax"))
  expect_snapshot(layer_summary(p))
})

test_that("plot_forest: reference row distributed into each covariate panel", {
  p <- plot_forest(forest_auc,
                   cov_level_ref = attr(data_sad_pkforest, "cov_level_ref"))
  expect_snapshot(layer_summary(p))
})

test_that("plot_forest: consolidated reference panel with a reference band", {
  p <- plot_forest(forest_auc, ref_band = c(0.8, 1.25))
  expect_snapshot(layer_summary(p))
})

test_that("plot_vpc_cont: standard VPC stratified by FOOD with per-row LLOQ", {
  sim <- sim_pk(carry_out = c("LLOQ", "FOOD"))
  p <- suppressMessages(plot_vpc_cont(sim, strat_var = FOOD))
  expect_snapshot(layer_summary(p))
})

test_that("plot_vpc_cont: prediction-corrected VPC stratified by FOOD", {
  sim <- sim_pk(carry_out = c("LLOQ", "FOOD"))
  p <- suppressMessages(plot_vpc_cont(sim, strat_var = FOOD, pcvpc = TRUE))
  expect_snapshot(layer_summary(p))
})

test_that("plot_vpc_cens: BLQ-proportion VPC", {
  p <- suppressMessages(plot_vpc_cens(sim_pk(carry_out = "LLOQ")))
  expect_snapshot(layer_summary(p))
})

test_that("plot_vpc_legend: default legend panel", {
  expect_snapshot(layer_summary(plot_vpc_legend()))
})
