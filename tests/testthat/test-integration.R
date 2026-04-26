#####Integration Tests####
## Cross-function workflow tests

test_that("df_addn -> plot_dvtime: factor col_var from df_addn is accepted", {
  data <- df_addn(dplyr::mutate(data_sad, Dose = DOSE), grp_var = Dose, sep = "mg")
  p <- plot_dvtime(data, dv_var = "ODV", col_var = "Dose")
  expect_s3_class(p, "ggplot")
})

test_that("df_addpred -> plot_vpc_exactbins: prediction-corrected DV pipeline produces valid output", {
  model <- model_mread_load("pkmodel")
  data_pred <- df_addpred(data_sad, model)
  expect_s3_class(data_pred, "data.frame")
  sim <- df_mrgsim_replicate(data = data_sad, model = model,
                             replicates = 1, dv_var = "ODV")
  expect_s3_class(sim, "data.frame")
  expect_equal(nrow(data_pred), nrow(sim))
  p <- plot_vpc_exactbins(sim = sim, pcvpc = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("df_addn -> plot_dvconc: factor col_var works in dvconc with col_trend", {
  data <- df_addn(dplyr::mutate(data_sad, Dose = DOSE), grp_var = Dose, sep = "mg")
  p <- plot_dvconc(data, dv_var = "ODV", idv_var = "CONC",
                   col_var = "Dose", col_trend = TRUE)
  expect_s3_class(p, "ggplot")
  expect_true("colour" %in% names(p$mapping))
})

test_that("df_mrgsim_replicate -> plot_vpc_exactbins: simulated data produces VPC plot", {
  model <- model_mread_load("pkmodel")
  sim <- df_mrgsim_replicate(data = data_sad, model = model,
                             replicates = 5, dv_var = "ODV")
  p <- plot_vpc_exactbins(sim = sim)
  expect_s3_class(p, "ggplot")
})

test_that("df_doseprop + plot_doseprop: same data produces consistent table and plot", {
  tbl <- df_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"))
  p <- plot_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"))
  expect_s3_class(tbl, "data.frame")
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(tbl), 2)
})
