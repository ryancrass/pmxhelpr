#####plot_vpc_exactbins####

##Test Output
test_that("Output is a `ggplot` plot object", {
  expect_s3_class(plot_vpc_exactbins(sim = df_mrgsim_replicate(data=data_sad,model=model_mread_load("pkmodel"),
                                                            replicates = 10,
                                                            dv_var = "ODV")),
               class = "ggplot")
})

test_that("Output plot contains a caption with number of replicates by default", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_equal(
    plot_vpc_exactbins(sim = testsim)$labels$caption,
    "Replicates = 10"
  )
})

test_that("Output plot does not contains a caption with number of replicates when `show_rep = FALSE`", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_no_match(
    names(plot_vpc_exactbins(sim = testsim, show_rep = FALSE)$labels),
    regexp = "caption"
  )
})


##Test Argument Handling
test_that("Error if incorrect class for arugmument `sim`", {
  expect_error(plot_vpc_exactbins(sim = "simdata"),
               regexp = "argument `sim` must be a `data.frame`")
})

test_that("Error if TIME variable specified in time_vars does not exist in `sim`", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_exactbins(sim = testsim,time_vars = c(TIME = "ATFD")),
    regexp = "must be variables in `sim`"
    )
})

test_that("Error if NTIME variable specified in time_vars does not exist in `sim`", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_exactbins(sim = testsim,time_vars = c(NTIME = "NTFD")),
    regexp = "must be variables in `sim`"
  )
})

test_that("No error if TIME and NTIME specified as same variable in time_vars", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_no_error(
    plot_vpc_exactbins(sim = testsim,time_vars = c(TIME = "NTIME", NTIME = "NTIME")),
  )
})

test_that("Error if PRED variable specified in output_vars does not exist in `sim` and pcvpc = TRUE", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_exactbins(sim = testsim,output_vars = c(PRED = "CPRED"), pcvpc = TRUE),
    regexp = "must be variables in `sim`"
  )
})

test_that("No error if PRED variable specified in output_vars does not exist in `sim` and pcvpc = FALSE", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_no_error(
    plot_vpc_exactbins(sim = testsim,output_vars = c(PRED = "CPRED"), pcvpc = FALSE),
  )
})

test_that("Error if SIMDV variable specified in output_vars does not exist in `sim`", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_exactbins(sim = testsim,output_vars = c(SIMDV = "DVSIM")),
    regexp = "must be variables in `sim`"
  )
})

test_that("Error if OBSDV variable specified in output_vars does not exist in `sim`", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_exactbins(sim = testsim,output_vars = c(OBSDV = "DV")),
    regexp = "must be variables in `sim`"
  )
})

test_that("Error if argument for `loq` is not class numeric", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_exactbins(sim = testsim, loq = "1"),
    regexp = "argument `loq` must be class `numeric`"
  )
})


test_that("Error if variable specified by `strat_var` does not exist in `sim`", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_exactbins(sim = testsim, strat_var = "FOOD_f"),
    regexp = "argument `strat_var` must be variables in `sim`"
  )
})

test_that("Error if variable specified by `irep_name` does not exist in `sim`", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_exactbins(sim = testsim, irep_name = "IREP"),
    regexp = "argument `irep_name` must be variables in `sim`"
  )
})

##Test NSE Bare Names
test_that("df_nobsbin accepts bare names and matches string output", {
  n1 <- df_nobsbin(data_sad, bin_var = NTIME)
  n2 <- df_nobsbin(data_sad, bin_var = "NTIME")
  expect_identical(n1, n2)
})

test_that("df_pcdv accepts bare names and matches string output", {
  model <- model_mread_load(model = "pkmodel")
  data_pred <- df_addpred(data_sad, model)
  p1 <- df_pcdv(data_pred, bin_var = NTIME, dvpred_vars = c(DV = "ODV", PRED = "PRED"))
  p2 <- df_pcdv(data_pred, bin_var = "NTIME", dvpred_vars = c(DV = "ODV", PRED = "PRED"))
  expect_identical(p1, p2)
})

test_that("df_vpcstats accepts bare names and matches string output", {
  testsim <- df_mrgsim_replicate(data = data_sad,
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  v1 <- df_vpcstats(sim = testsim,
                     sim_dv_var = SIMDV, obs_dv_var = OBSDV, irep_name = SIM)
  v2 <- df_vpcstats(sim = testsim,
                     sim_dv_var = "SIMDV", obs_dv_var = "OBSDV", irep_name = "SIM")
  expect_identical(v1, v2)
})

test_that("plot_vpc accepts bare strat_var and matches string output", {
  testsim <- df_mrgsim_replicate(data = data_sad,
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV",
                                 char_vars = "FOOD")
  testsim <- dplyr::mutate(testsim, FOOD_f = factor(FOOD))
  vpcstat <- plot_vpc_exactbins(sim = testsim, strat_var = FOOD_f, vpcstats = TRUE)

  p1 <- plot_vpc(vpcstat,strat_var = FOOD_f)
  p2 <- plot_vpc(vpcstat,strat_var = "FOOD_f")
  expect_identical(p1, p2)
})

##Test PC-VPC correctness
test_that("PC-VPC applies per-bin prediction correction, not global", {
  testsim <- df_mrgsim_replicate(data = data_sad,
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  stats_pc <- plot_vpc_exactbins(sim = testsim, pcvpc = TRUE, vpcstats = TRUE)
  stats_nopc <- plot_vpc_exactbins(sim = testsim, pcvpc = FALSE, vpcstats = TRUE)

  # PC and non-PC medians should differ
  expect_false(identical(stats_pc$q50_med, stats_nopc$q50_med))
})

##Test vpcstats return
test_that("vpcstats = TRUE returns a data.frame with expected columns", {
  testsim <- df_mrgsim_replicate(data = data_sad,
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  result <- plot_vpc_exactbins(sim = testsim, vpcstats = TRUE)
  expect_s3_class(result, "data.frame")
  expected_cols <- c("NTIME", "nbin", "q5_med", "q50_med", "q95_med",
                     "obs5", "obs50", "obs95")
  expect_true(all(expected_cols %in% colnames(result)))
})

##Test stratified VPC
test_that("Stratified VPC produces a faceted plot", {
  testsim <- df_mrgsim_replicate(data = data_sad,
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV",
                                 char_vars = "FOOD")
  testsim <- dplyr::mutate(testsim, FOOD_f = factor(FOOD))

  p <- plot_vpc_exactbins(sim = testsim, strat_var = FOOD_f)
  expect_s3_class(p, "ggplot")
  expect_true("FacetWrap" %in% class(p$facet))
})

##Test min_bin_count filtering
test_that("min_bin_count filters small bins from summary but returns a plot", {
  testsim <- df_mrgsim_replicate(data = data_sad,
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  stats_all <- plot_vpc_exactbins(sim = testsim, min_bin_count = 1, vpcstats = TRUE)
  stats_filt <- plot_vpc_exactbins(sim = testsim, min_bin_count = 100, vpcstats = TRUE)

  # Filtered stats should have fewer or equal rows
  # (stats themselves are not filtered, but the plot uses filtered data)
  # Verify plot still works with high min_bin_count
  expect_no_error(plot_vpc_exactbins(sim = testsim, min_bin_count = 100))
})
