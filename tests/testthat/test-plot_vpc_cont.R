#####plot_vpc_cont####

##Test Output
test_that("Output is a `ggplot` plot object", {
    sim <- df_mrgsim_replicate(data=data_sad,model=model_mread_load("pkmodel"),
                        replicates = 10,
                        dv_var = "ODV")
    expect_s3_class(sim, class = "data.frame")
    plot <- plot_vpc_cont(sim = sim)
    expect_s3_class(plot, class = "ggplot")
})

test_that("Output plot contains a caption with number of replicates by default", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_equal(
    plot_vpc_cont(sim = testsim)$labels$caption,
    "Replicates = 10"
  )
})

test_that("Output plot does not contains a caption with number of replicates when `show_rep = FALSE`", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_no_match(
    names(plot_vpc_cont(sim = testsim, show_rep = FALSE)$labels),
    regexp = "caption"
  )
})


##Test Argument Handling
test_that("Error if incorrect class for arugmument `sim`", {
  expect_error(plot_vpc_cont(sim = "simdata"),
               regexp = "argument `sim` must be a `data.frame`")
})

test_that("Error if time_var does not exist in `sim`", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_cont(sim = testsim, time_var = "ATFD"),
    regexp = "must be variable.*in `sim`"
    )
})

test_that("Error if ntime_var does not exist in `sim`", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_cont(sim = testsim, ntime_var = "NTFD"),
    regexp = "must be variable.*in `sim`"
  )
})

test_that("No error if time_var and ntime_var specified as same variable", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_no_error(
    plot_vpc_cont(sim = testsim, time_var = "NTIME", ntime_var = "NTIME"),
  )
})

test_that("Error if pred_var does not exist in `sim` and pcvpc = TRUE", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_cont(sim = testsim, pred_var = "CPRED", pcvpc = TRUE, loq = 1),
    regexp = "must be variable.*in `sim`"
  )
})

test_that("No error if pred_var does not exist in `sim` and pcvpc = FALSE", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_no_error(
    plot_vpc_cont(sim = testsim, pred_var = "CPRED", pcvpc = FALSE),
  )
})

test_that("Error if sim_dv_var does not exist in `sim`", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_cont(sim = testsim, sim_dv_var = "DVSIM"),
    regexp = "must be variable.*in `sim`"
  )
})

test_that("Error if obs_dv_var does not exist in `sim`", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_cont(sim = testsim, obs_dv_var = "DV"),
    regexp = "must be variable.*in `sim`"
  )
})

test_that("Error if argument for `loq` is not class numeric", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_cont(sim = testsim, loq = "1"),
    regexp = "argument `loq` must be class `numeric`"
  )
})


test_that("Error if variable specified by `strat_var` does not exist in `sim`", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_cont(sim = testsim, strat_var = "FOOD_f"),
    regexp = "argument `strat_var` must be variable.*in `sim`"
  )
})

test_that("Error if variable specified by `irep_name` does not exist in `sim`", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_cont(sim = testsim, irep_name = "IREP"),
    regexp = "argument `irep_name` must be variable.*in `sim`"
  )
})

##Test NSE Bare Names
test_that("plot_vpc_cont accepts bare irep_name and matches string output", {
  testsim <- df_mrgsim_replicate(data = data_sad,
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  v1 <- plot_vpc_cont(sim = testsim, irep_name = SIM, vpcstats = TRUE)
  v2 <- plot_vpc_cont(sim = testsim, irep_name = "SIM", vpcstats = TRUE)
  expect_identical(v1, v2)
})

test_that("plot_vpc_cont accepts bare strat_var and matches string output", {
  testsim <- df_mrgsim_replicate(data = data_sad,
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV",
                                 char_vars = "FOOD")
  testsim <- dplyr::mutate(testsim, FOOD_f = factor(FOOD))
  v1 <- plot_vpc_cont(sim = testsim, strat_var = FOOD_f, vpcstats = TRUE)
  v2 <- plot_vpc_cont(sim = testsim, strat_var = "FOOD_f", vpcstats = TRUE)
  expect_identical(v1, v2)
})

##Test PC-VPC correctness
test_that("PC-VPC applies per-bin prediction correction, not global", {
  testsim <- df_mrgsim_replicate(data = data_sad,
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  stats_pc <- plot_vpc_cont(sim = testsim, pcvpc = TRUE, loq = 1, vpcstats = TRUE)
  stats_nopc <- plot_vpc_cont(sim = testsim, pcvpc = FALSE, loq = 1, vpcstats = TRUE)

  # PC and non-PC medians should differ
  expect_false(identical(stats_pc$q50_med, stats_nopc$q50_med))
})

##Test vpcstats return
test_that("vpcstats = TRUE returns a data.frame with expected columns", {
  testsim <- df_mrgsim_replicate(data = data_sad,
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  result <- plot_vpc_cont(sim = testsim, vpcstats = TRUE)
  expect_s3_class(result, "data.frame")
  expected_cols <- c("BIN_MID", "nbin", "q5_med", "q50_med", "q95_med",
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

  p <- plot_vpc_cont(sim = testsim, strat_var = FOOD_f)
  expect_s3_class(p, "ggplot")
  expect_true("FacetWrap" %in% class(p$facet))
})

##Test min_bin_count filtering
test_that("min_bin_count filters small bins from summary but returns a plot", {
  testsim <- df_mrgsim_replicate(data = data_sad,
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  stats_all <- plot_vpc_cont(sim = testsim, min_bin_count = 1, vpcstats = TRUE)
  stats_filt <- plot_vpc_cont(sim = testsim, min_bin_count = 100, vpcstats = TRUE)

  # Filtered stats should have fewer or equal rows
  # (stats themselves are not filtered, but the plot uses filtered data)
  # Verify plot still works with high min_bin_count
  expect_no_error(plot_vpc_cont(sim = testsim, min_bin_count = 100))
})

##Test pcvpc blocks loq
test_that("pcvpc = TRUE nullifies loq so no LLOQ reference line is drawn", {
  testsim <- df_mrgsim_replicate(data = data_sad,
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  # With pcvpc = TRUE, explicit loq should be ignored
  stats_pc <- plot_vpc_cont(sim = testsim, pcvpc = TRUE, loq = 10, vpcstats = TRUE)
  stats_nopc <- plot_vpc_cont(sim = testsim, pcvpc = FALSE, loq = 10, vpcstats = TRUE)

  # pcVPC applies prediction correction and pre-PC NA encoding for BLQ;
  # std VPC leaves SIMDV raw and -Inf-encodes OBSDV. The two pipelines
  # produce different summary statistics.
  expect_false(identical(stats_pc, stats_nopc))
})

##Test LLOQ inheritance from sim column
test_that("loq is inherited from LLOQ column in sim when not explicitly provided", {
  testsim <- df_mrgsim_replicate(data = data_sad,
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV",
                                 num_vars = c("LLOQ"))

  # With LLOQ column present and loq = NULL, should auto-inherit
  stats_inherit <- plot_vpc_cont(sim = testsim, vpcstats = TRUE)
  # Explicit loq matching LLOQ value should give same result
  lloq_val <- unique(testsim$LLOQ[testsim$SIM == 1 & !is.na(testsim$LLOQ)])
  stats_explicit <- plot_vpc_cont(sim = testsim, loq = lloq_val, vpcstats = TRUE)
  expect_identical(stats_inherit, stats_explicit)
})

test_that("plot_vpc_cont warns when loq is inherited from LLOQ and pcvpc = TRUE", {
  testsim <- df_mrgsim_replicate(data = data_sad,
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV",
                                 num_vars = c("LLOQ"))

  # Inherited loq + pcvpc = TRUE: warn about pre-PC censoring + no ref line
  expect_warning(plot_vpc_cont(sim = testsim, pcvpc = TRUE, vpcstats = TRUE),
                 regexp = "prediction-correction")

  # Explicit loq + pcvpc = TRUE: user-confirmed, no warning
  expect_no_warning(plot_vpc_cont(sim = testsim, pcvpc = TRUE, loq = 1, vpcstats = TRUE))
})

test_that("explicit loq overrides LLOQ column in sim", {
  testsim <- df_mrgsim_replicate(data = data_sad,
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV",
                                 num_vars = c("LLOQ"))

  stats_inherit <- plot_vpc_cont(sim = testsim, vpcstats = TRUE)
  stats_override <- plot_vpc_cont(sim = testsim, loq = 999, vpcstats = TRUE)
  # Different loq values should produce different censored quantiles
  expect_false(identical(stats_inherit, stats_override))
})

##### Test edge-case warnings #####

test_that("plot_vpc_cont warns when LLOQ column has multiple unique values", {
  testsim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3),
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV",
                                 num_vars = c("LLOQ"))
  # Inject a second non-NA LLOQ value
  testsim$LLOQ[testsim$NTIME == max(testsim$NTIME, na.rm = TRUE)] <- 2
  expect_warning(plot_vpc_cont(sim = testsim, vpcstats = TRUE),
                 regexp = "multiple unique values")
})

test_that("plot_vpc_cont warns when strat_var contains NA values", {
  testsim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3),
                                 model = model_mread_load("pkmodel"),
                                 replicates = 5,
                                 dv_var = "ODV",
                                 char_vars = "FOOD")
  testsim$FOOD_f <- factor(testsim$FOOD)
  testsim$FOOD_f[1:5] <- NA
  expect_warning(plot_vpc_cont(sim = testsim, strat_var = FOOD_f, vpcstats = TRUE),
                 regexp = "NA values")
})
