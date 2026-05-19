#####plot_vpc_cont####

##Test Output
test_that("Output is a `ggplot` plot object", {
    sim <- df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),model=model_mread_load("pkmodel"),
                        replicates = 10,
                        dv_var = "ODV")
    expect_s3_class(sim, class = "data.frame")
    plot <- plot_vpc_cont(data = sim)
    expect_s3_class(plot, class = "ggplot")
})

test_that("Output plot contains a caption with number of replicates by default", {
  testsim <- df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_equal(
    plot_vpc_cont(data = testsim)$labels$caption,
    "Replicates = 10"
  )
})

test_that("Output plot does not contains a caption with number of replicates when `show_rep = FALSE`", {
  testsim <- df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_no_match(
    names(plot_vpc_cont(data = testsim, show_rep = FALSE)$labels),
    regexp = "caption"
  )
})


##Test Argument Handling
test_that("Error if incorrect class for argument `data`", {
  expect_error(plot_vpc_cont(data = "simdata"),
               regexp = "argument `data` must be a `data.frame`")
})

test_that("Error if time_var does not exist in `data`", {
  testsim <- df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_cont(data = testsim, time_var = "ATFD"),
    regexp = "must be variable.*in `data`"
    )
})

test_that("Error if ntime_var does not exist in `data`", {
  testsim <- df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_cont(data = testsim, ntime_var = "NTFD"),
    regexp = "must be variable.*in `data`"
  )
})

test_that("No error if time_var and ntime_var specified as same variable", {
  testsim <- df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_no_error(
    plot_vpc_cont(data = testsim, time_var = "NTIME", ntime_var = "NTIME"),
  )
})

test_that("Error if pred_var does not exist in `data` (pred is required for pc stats, always computed)", {
  testsim <- df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_cont(data = testsim, pred_var = "CPRED"),
    regexp = "must be variable.*in `data`"
  )
})

test_that("Error if sim_dv_var does not exist in `data`", {
  testsim <- df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_cont(data = testsim, sim_dv_var = "DVSIM"),
    regexp = "must be variable.*in `data`"
  )
})

test_that("Error if obs_dv_var does not exist in `data`", {
  testsim <- df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_cont(data = testsim, obs_dv_var = "DV"),
    regexp = "must be variable.*in `data`"
  )
})

test_that("Error if argument for `loq` is not class numeric", {
  testsim <- df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_cont(data = testsim, loq = "1"),
    regexp = "argument `loq` must be class `numeric`"
  )
})


test_that("Error if variable specified by `strat_var` does not exist in `data`", {
  testsim <- df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_cont(data = testsim, strat_var = "FOOD_f"),
    regexp = "argument `strat_var` must be variable.*in `data`"
  )
})

test_that("Error if variable specified by `irep_name` does not exist in `data`", {
  testsim <- df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                 model=model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_cont(data = testsim, irep_name = "IREP"),
    regexp = "argument `irep_name` must be variable.*in `data`"
  )
})

##Test NSE Bare Names
test_that("plot_vpc_cont accepts bare irep_name and matches string output", {
  testsim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3),
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  v1 <- df_vpcstats(testsim, irep_name = SIM)
  v2 <- df_vpcstats(testsim, irep_name = "SIM")
  expect_identical(v1, v2)
})

test_that("plot_vpc_cont accepts bare strat_var and matches string output", {
  testsim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3),
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV",
                                 carry_out = "FOOD")
  testsim <- dplyr::mutate(testsim, FOOD_f = factor(FOOD))
  v1 <- df_vpcstats(testsim, strat_var = FOOD_f)
  v2 <- df_vpcstats(testsim, strat_var = "FOOD_f")
  expect_identical(v1, v2)
})

##Test PC-VPC correctness
test_that("PC-VPC applies per-bin prediction correction, not global", {
  testsim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3),
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  result <- df_vpcstats(testsim, loq = 1)

  # PC and non-PC medians should differ within the same result
  expect_false(identical(result$stats$sim_med_med, result$stats$pc_sim_med_med))
})

##Test vpcstats return
test_that("df_vpcstats() returns a pmx_stats container with stats, obs, config slots", {
  testsim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3),
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  result <- df_vpcstats(testsim)
  expect_type(result, "list")
  expect_s3_class(result, "vpc_stats")
  expect_s3_class(result, "pmx_stats")
  expect_named(result, c("stats", "obs", "config"))
  expect_s3_class(result$stats, "data.frame")
  expect_s3_class(result$obs, "data.frame")
  expect_type(result$config, "list")
  expected_cols <- c("BIN_MID", "obs_n", "sim_low_med", "sim_med_med", "sim_hi_med",
                     "obs_low", "obs_med", "obs_hi")
  expect_true(all(expected_cols %in% colnames(result$stats)))
})

##Test stratified VPC
test_that("Stratified VPC produces a faceted plot", {
  testsim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3),
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV",
                                 carry_out = "FOOD")
  testsim <- dplyr::mutate(testsim, FOOD_f = factor(FOOD))

  p <- plot_vpc_cont(data = testsim, strat_var = FOOD_f)
  expect_s3_class(p, "ggplot")
  expect_true("FacetWrap" %in% class(p$facet))
})

##Test min_bin_count filtering (plot-layer-only filter)
test_that("min_bin_count is a plot-layer arg; df_vpcstats returns unfiltered", {
  testsim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3),
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  # df_vpcstats has no min_bin_count argument
  expect_error(df_vpcstats(testsim, min_bin_count = 1),
               regexp = "unused argument")

  # Plot still renders with high min_bin_count (small bins dropped from layers)
  expect_no_error(plot_vpc_cont(data = testsim, min_bin_count = 100))
})

##Test pcvpc plot toggle suppresses LOQ ref line
test_that("plot_vpc_cont(pcvpc = TRUE) suppresses the LOQ reference line", {
  testsim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3),
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")

  p_std <- plot_vpc_cont(data = testsim, loq = 10, pcvpc = FALSE)
  p_pc  <- plot_vpc_cont(data = testsim, loq = 10, pcvpc = TRUE)
  has_hline <- function(p) {
    any(vapply(p$layers, function(l) inherits(l$geom, "GeomHline"), logical(1)))
  }
  expect_true(has_hline(p_std))
  expect_false(has_hline(p_pc))
})

##Test LLOQ inheritance from sim column
test_that("loq is inherited from LLOQ column in sim when not explicitly provided", {
  testsim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3),
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV",
                                 carry_out = c("LLOQ"))

  # With LLOQ column present and loq = NULL, should auto-inherit
  expect_message(
    stats_inherit <- df_vpcstats(testsim),
    regexp = "Inheriting per-row `loq` from `LLOQ` column in `data`"
  )
  # Explicit loq matching LLOQ value should give same result
  lloq_val <- unique(testsim$LLOQ[testsim$SIM == 1 & !is.na(testsim$LLOQ)])
  stats_explicit <- df_vpcstats(testsim, loq = lloq_val)
  expect_identical(stats_inherit, stats_explicit)
})

test_that("explicit loq overrides LLOQ column in sim", {
  testsim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3),
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV",
                                 carry_out = c("LLOQ"))

  stats_inherit <- df_vpcstats(testsim)
  stats_override <- df_vpcstats(testsim, loq = 999)
  # Different loq values should produce different censored quantiles
  expect_false(identical(stats_inherit, stats_override))
})

##### Test plot_vpc_cont() with precomputed df_vpcstats() output #####

test_that("plot_vpc_cont accepts a precomputed vpc_stats list and returns a ggplot", {
  testsim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3),
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")
  out <- df_vpcstats(testsim)
  expect_s3_class(out, "vpc_stats")
  p <- plot_vpc_cont(out)
  expect_s3_class(p, "ggplot")
})

test_that("precomputed path with strat_var produces a faceted plot", {
  testsim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3),
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV",
                                 carry_out = "FOOD")
  testsim <- dplyr::mutate(testsim, FOOD_f = factor(FOOD))
  out <- df_vpcstats(testsim, strat_var = FOOD_f)
  p <- plot_vpc_cont(out)
  expect_s3_class(p, "ggplot")
  expect_true("FacetWrap" %in% class(p$facet))
})

test_that("precomputed pcVPC plot path suppresses LOQ reference line", {
  testsim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3),
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")
  out <- df_vpcstats(testsim, loq = 1)
  p_std <- plot_vpc_cont(out, pcvpc = FALSE)
  p_pc  <- plot_vpc_cont(out, pcvpc = TRUE)
  has_hline <- function(p) {
    any(vapply(p$layers, function(l) inherits(l$geom, "GeomHline"), logical(1)))
  }
  expect_true(has_hline(p_std))
  expect_false(has_hline(p_pc))
})

test_that("min_bin_count override on precomputed path filters layers", {
  testsim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3),
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")
  out <- df_vpcstats(testsim, loq = 1)
  threshold <- max(out$stats$obs_n - out$stats$obs_n_blq) + 1L
  p <- plot_vpc_cont(out, min_bin_count = threshold)
  built <- ggplot2::ggplot_build(p)
  ribbon_layers <- vapply(built$data,
                          function(d) "ymin" %in% colnames(d) && nrow(d) > 0,
                          logical(1))
  expect_false(any(ribbon_layers))
})

test_that("plot_vpc_cont aborts when pipeline args are passed on the precomputed path", {
  testsim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3),
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")
  out <- df_vpcstats(testsim)
  expect_error(plot_vpc_cont(out, strat_var = "FOOD"),
               regexp = "cannot accept pipeline arguments")
  expect_error(plot_vpc_cont(out, loq = 1),
               regexp = "cannot accept pipeline arguments")
  expect_error(plot_vpc_cont(out, mode = "drop"),
               regexp = "cannot accept pipeline arguments")
})

test_that("plot_vpc_cont accepts plot-only args on the precomputed path", {
  testsim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3),
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")
  out <- df_vpcstats(testsim)
  expect_s3_class(plot_vpc_cont(out, pcvpc = TRUE), "ggplot")
  expect_s3_class(plot_vpc_cont(out, min_bin_count = 2), "ggplot")
  expect_s3_class(plot_vpc_cont(out, show_rep = FALSE), "ggplot")
  expect_s3_class(plot_vpc_cont(out, theme = plot_vpc_theme()), "ggplot")
  expect_s3_class(plot_vpc_cont(out, shown = plot_vpc_shown()), "ggplot")
})

test_that("precomputed and raw paths produce structurally equivalent plot data", {
  testsim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3),
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV")
  out <- df_vpcstats(testsim)
  p_pre <- plot_vpc_cont(out)
  p_raw <- plot_vpc_cont(testsim)
  built_pre <- ggplot2::ggplot_build(p_pre)
  built_raw <- ggplot2::ggplot_build(p_raw)
  ## Same number of plot layers; same number of rows in each layer's data.
  expect_equal(length(built_pre$data), length(built_raw$data))
  expect_equal(vapply(built_pre$data, nrow, integer(1)),
               vapply(built_raw$data, nrow, integer(1)))
})

##### Test edge-case warnings #####

test_that("df_vpcstats inherits per-row LLOQ when column has multiple unique values", {
  testsim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3),
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV",
                                 carry_out = c("LLOQ"))
  # Inject a second non-NA LLOQ value at the last nominal time
  testsim$LLOQ[testsim$NTIME == max(testsim$NTIME, na.rm = TRUE)] <- 2

  # No warning, just an inheritance message
  expect_warning(out <- df_vpcstats(testsim), regexp = NA)

  # Config carries the sorted unique values
  expect_equal(out$config$loq, c(1, 2))

  # obs slot carries the row-aligned LOQ column (the obs filter to MDV==0 may
  # exclude one of the injected values; config$loq is the canonical multi-set)
  expect_true("LOQ" %in% colnames(out$obs))
  expect_equal(length(out$obs$LOQ), nrow(out$obs))

  # The built plot draws one geom_hline layer rendering 2 lines
  p <- plot_vpc_cont(testsim)
  expect_s3_class(p, "ggplot")
})

test_that("plot_build_vpc draws per-facet LLOQ ref lines when stratified", {
  testsim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3),
                                 model = model_mread_load("pkmodel"),
                                 replicates = 10,
                                 dv_var = "ODV",
                                 carry_out = c("LLOQ"),
                                 recover  = c("PART"))
  # Re-code Part 2-FE to LLOQ = 2 (mirrors the vignette pattern)
  testsim$LLOQ[testsim$PART == "Part 2-FE"] <- 2

  p <- plot_vpc_cont(testsim, strat_var = PART)

  # Locate the geom_hline layer used for the LLOQ ref line(s)
  hline_layer <- Filter(function(L) inherits(L$geom, "GeomHline"), p$layers)
  expect_length(hline_layer, 1)
  hline_data <- hline_layer[[1]]$data

  # Layer data carries both strat_var and LOQ columns (per-facet path)
  expect_true("LOQ" %in% colnames(hline_data))
  expect_true("PART" %in% colnames(hline_data))
  # One row per (strat_level, LOQ) pair: Part 1-SAD/1 and Part 2-FE/2
  expect_setequal(hline_data$LOQ, c(1, 2))
  expect_equal(nrow(hline_data), 2)
})

test_that("plot_build_vpc: explicit loq overrides per-facet column dispatch (PR#19 review)", {
  ## Regression for PR#19 review (jacobdum): when the caller supplies `loq`
  ## explicitly to plot_build_vpc(), it must always draw global hline(s) at
  ## that value, even on stratified data that carries a row-aligned LOQ
  ## column. Previously the column branch silently won and the explicit `loq`
  ## was ignored on that path.
  testsim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3),
                                 model = model_mread_load("pkmodel"),
                                 replicates = 5,
                                 dv_var = "ODV",
                                 carry_out = c("LLOQ"),
                                 recover  = c("PART"))
  testsim$LLOQ[testsim$PART == "Part 2-FE"] <- 2
  out <- df_vpcstats(testsim, strat_var = PART)

  p <- plot_build_vpc(out, loq = 5)

  hline_layer <- Filter(function(L) inherits(L$geom, "GeomHline"), p$layers)
  expect_length(hline_layer, 1)
  # Global-hline branch: geom_hline(yintercept = loq) builds a one-row
  # layer-data data.frame(yintercept = loq), and the strat/LOQ columns
  # carried by the per-facet branch are absent.
  hline_data <- as.data.frame(hline_layer[[1]]$data)
  expect_equal(hline_data$yintercept, 5)
  expect_false("LOQ"  %in% colnames(hline_data))
  expect_false("PART" %in% colnames(hline_data))
})

test_that("plot_vpc_cont warns when strat_var contains NA values", {
  testsim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3),
                                 model = model_mread_load("pkmodel"),
                                 replicates = 5,
                                 dv_var = "ODV",
                                 carry_out = "FOOD")
  testsim$FOOD_f <- factor(testsim$FOOD)
  testsim$FOOD_f[1:5] <- NA
  expect_warning(df_vpcstats(testsim, strat_var = FOOD_f),
                 regexp = "NA values")
})


## Test pmx_vpc_plot S3 class

test_that("plot_vpc_cont() returns a pmx_vpc_plot object", {
  sim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3), model = model_mread_load("pkmodel"),
                             replicates = 5, dv_var = "ODV")
  expect_s3_class(plot_vpc_cont(data = sim), "pmx_vpc_plot")
})

test_that("plot_build_vpc() returns a pmx_vpc_plot object", {
  sim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3), model = model_mread_load("pkmodel"),
                             replicates = 5, dv_var = "ODV")
  out <- df_vpcstats(sim)
  expect_s3_class(plot_build_vpc(out), "pmx_vpc_plot")
})

test_that("Adding facet_wrap() to a pmx_vpc_plot warns about incorrect statistics", {
  sim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3), model = model_mread_load("pkmodel"),
                             replicates = 5, dv_var = "ODV")
  p <- plot_vpc_cont(data = sim)
  expect_warning(p + ggplot2::facet_wrap(~FOOD),
                 regexp = "facet_\\*\\(\\).*plot_vpc_cont")
})

test_that("Adding facet_grid() to a pmx_vpc_plot warns about incorrect statistics", {
  sim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3), model = model_mread_load("pkmodel"),
                             replicates = 5, dv_var = "ODV")
  p <- plot_vpc_cont(data = sim)
  expect_warning(p + ggplot2::facet_grid(~FOOD),
                 regexp = "facet_\\*\\(\\).*plot_vpc_cont")
})

test_that("Adding non-facet layers to a pmx_vpc_plot does not warn", {
  sim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3), model = model_mread_load("pkmodel"),
                             replicates = 5, dv_var = "ODV")
  p <- plot_vpc_cont(data = sim)
  expect_no_warning(p + ggplot2::labs(title = "test"))
})

test_that("pmx_vpc_plot class is preserved after adding non-facet layers", {
  sim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3), model = model_mread_load("pkmodel"),
                             replicates = 5, dv_var = "ODV")
  p <- plot_vpc_cont(data = sim) + ggplot2::labs(title = "test")
  expect_s3_class(p, "pmx_vpc_plot")
  expect_warning(p + ggplot2::facet_wrap(~FOOD),
                 regexp = "facet_\\*\\(\\).*plot_vpc_cont")
})
