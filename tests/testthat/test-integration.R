#####Integration Tests####
## Cross-function workflow tests

test_that("var_addn -> plot_dvtime: factor col_var maps to the color aesthetic", {
  data <- data_sad |>
    dplyr::filter(CMT != 3) |>
    dplyr::mutate(Dose = var_addn(DOSE, ID, sep = "mg"))
  p <- plot_dvtime(data, dv_var = "ODV", col_var = "Dose")
  expect_s3_class(p, "ggplot")
  expect_true("colour" %in% names(p$mapping))
  expect_equal(rlang::quo_name(p$mapping$colour), "Dose")
})

test_that("df_mrgsim_addpred -> plot_vpc_cont: prediction-corrected pipeline shifts pc medians vs std", {
  model <- model_mread_load("pkmodel")
  data_pred <- df_mrgsim_addpred(data_sad, model)
  expect_s3_class(data_pred, "data.frame")
  expect_true("PRED" %in% colnames(data_pred))
  sim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3), model = model,
                              replicates = 1, dv_var = "ODV")
  stats <- df_vpcstats(sim, loq = 1)
  ## pc-flavor pred-correction must produce different sim medians than std
  expect_false(identical(stats$stats$sim_med_med,
                          stats$stats$pc_sim_med_med))
  ## And the plot still builds
  p <- plot_vpc_cont(data = sim, pcvpc = TRUE, loq = 1)
  expect_s3_class(p, "ggplot")
})

test_that("var_addn -> plot_dvconc: factor col_var works in dvconc with col_trend", {
  data <- dplyr::mutate(data_sad, Dose = var_addn(DOSE, ID, sep = "mg"))
  p <- plot_dvconc(data, dv_var = "ODV", idv_var = "CONC",
                   col_var = "Dose", col_trend = TRUE)
  expect_s3_class(p, "ggplot")
  expect_true("colour" %in% names(p$mapping))
})

test_that("df_mrgsim_replicate -> plot_vpc_cont: simulated data produces a VPC with sim and obs geoms", {
  model <- model_mread_load("pkmodel")
  sim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3), model = model,
                              replicates = 5, dv_var = "ODV")
  p <- plot_vpc_cont(data = sim)
  expect_s3_class(p, "ggplot")
  has_geom <- function(p, cls) any(vapply(p$layers,
                                            function(l) inherits(l$geom, cls),
                                            logical(1)))
  expect_true(has_geom(p, "GeomRibbon"))   # sim PI CI ribbon
  expect_true(has_geom(p, "GeomPoint"))    # obs points
})

test_that("df_doseprop + plot_doseprop: same data produces consistent table and plot", {
  tbl <- df_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"))
  p <- plot_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"))
  expect_s3_class(tbl, "doseprop_stats")
  expect_s3_class(tbl, "pmx_stats")
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(tbl$stats), 2)
})
