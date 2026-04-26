#####dvtime_caption####

test_that("dvtime_caption for cent = 'mean' contains 'mean'", {
  cap <- dvtime_caption(cent = "mean")
  expect_match(cap, "mean")
})

test_that("dvtime_caption for cent = 'median' contains 'median'", {
  cap <- dvtime_caption(cent = "median")
  expect_match(cap, "median")
})

test_that("dvtime_caption for cent = 'mean' with log_y = TRUE contains 'geometric mean'", {
  cap <- dvtime_caption(cent = "mean", log_y = TRUE)
  expect_match(cap, "geometric mean")
})

test_that("dvtime_caption for cent = 'none' omits 'Solid circles' prefix", {
  cap <- dvtime_caption(cent = "none")
  expect_no_match(cap, "Solid circles")
})

test_that("dvtime_caption for obs_dv = TRUE, grp_dv = FALSE contains 'Open circles are observations'", {
  cap <- dvtime_caption(cent = "mean", obs_dv = TRUE, grp_dv = FALSE)
  expect_match(cap, "Open circles are observations")
})

test_that("dvtime_caption for obs_dv = FALSE, grp_dv = TRUE contains 'lines connect observations'", {
  cap <- dvtime_caption(cent = "mean", obs_dv = FALSE, grp_dv = TRUE)
  expect_match(cap, "lines connect observations within an individual")
})

##Test Argument Handling
test_that("Error if incorrect class for arugmument `data`", {
  expect_error(plot_dvtime(data = "data_sad", dv_var = "ODV"),
               regexp = "argument `data` must be a `data.frame`")
})

test_that("Error if TIME variable specified in time_vars does not exist in `sim`", {
  expect_error(plot_dvtime(data = dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", time_vars = c(TIME = "ATFD")),
               regexp = "must be variable.*in `data`")
})

test_that("Error if NTIME variable specified in time_vars does not exist in `sim`", {
  expect_error(plot_dvtime(data = dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", time_vars = c(TIME = "NTFD")),
               regexp = "must be variable.*in `data`")
})

test_that("No Error if TIME and NTIME variables are specified as the same variable in time_vars", {
  expect_no_error(plot_dvtime(data = dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", time_vars = c(TIME = "NTIME", NTIME = "NTIME")))
})

test_that("Error if `col_var` does not exist in `data`", {
  expect_error(plot_dvtime(data = dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", col_var = "FOOD_f"),
               regexp = "argument `col_var` must be variable.*in `data`")
})

test_that("Error if `grp_var` does not exist in `data` and `grp_dv'` == TRUE", {
  expect_error(plot_dvtime(data = dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", grp_var = "SUBJID", grp_dv = TRUE),
               regexp = "argument `grp_var` must be variable.*in `data`")
})

test_that("No error if `grp_var` does not exist in `data` and `grp_dv'` == FALSE", {
  expect_no_error(plot_dvtime(data = dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", grp_var = "SUBJID", grp_dv = FALSE))
})

test_that("Error if `dose_var` does not exist in `data` and `dosenorm'` == TRUE", {
  expect_error(plot_dvtime(data = dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", dose_var = "DOSEN", dosenorm = TRUE),
               regexp = "argument `dose_var` must be variable.*in `data`")
})

test_that("No error if `dose_var` does not exist in `data` and `dosenorm'` == FALSE", {
  expect_no_error(plot_dvtime(data = dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", dose_var = "DOSEN", dosenorm = FALSE))
})

test_that("Error if argument `loq_method` is not one of 0, 1, 2", {
  expect_error(plot_dvtime(data = dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", loq_method = 3),
               regexp = "argument `loq_method` must be 0, 1, or 2")
})

test_that("Error if argument `loq` is not coercible to numeric and `loq_method` = 1", {
  expect_error(plot_dvtime(data = dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", loq_method = 1, loq = "$"),
               regexp = "If `loq_method` is 1 or 2, then a numeric variable `LLOQ` must be present in `data` \n or argument `loq` must specified and numeric or coercible to numeric")
})

test_that("Error if argument `loq` is not coercible to numeric and `loq_method` = 2", {
  expect_error(plot_dvtime(data = dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", loq_method = 2, loq = "$"),
               regexp = "If `loq_method` is 1 or 2, then a numeric variable `LLOQ` must be present in `data` \n or argument `loq` must specified and numeric or coercible to numeric")
})

test_that("No error if variable `LLOQ` exists in `data` and `loq` = NULL and `loq_method` != 0", {
  expect_no_error(plot_dvtime(data = dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", loq_method = 1))
})

test_that("Error if variable `LLOQ` does not exist in `data` and `loq` = NULL and `loq_method` = 1", {
  expect_error(plot_dvtime(data = dplyr::select(dplyr::filter(data_sad, CMT != 3), -LLOQ), dv_var = "ODV", loq_method = 1),
               regexp = "If `loq_method` is 1 or 2, then a numeric variable `LLOQ` must be present in `data` \n or argument `loq` must specified and numeric or coercible to numeric")
})

test_that("Error if variable `LLOQ` does not exist in `data` and `loq` = NULL and `loq_method` = `", {
  expect_error(plot_dvtime(data = dplyr::select(dplyr::filter(data_sad, CMT != 3), -LLOQ), dv_var = "ODV", loq_method = 2),
               regexp = "If `loq_method` is 1 or 2, then a numeric variable `LLOQ` must be present in `data` \n or argument `loq` must specified and numeric or coercible to numeric")
})

test_that("No error if variable `LLOQ` does not exist in `data` and `loq` = a numeric value and `loq_method` = 1", {
  expect_no_error(plot_dvtime(data = dplyr::select(dplyr::filter(data_sad, CMT != 3), -LLOQ), dv_var = "ODV", loq = 1, loq_method = 1))
})

test_that("No error if variable `LLOQ` does not exist in `data` and `loq` = a numeric value and `loq_method` = 2", {
  expect_no_error(plot_dvtime(data = dplyr::select(dplyr::filter(data_sad, CMT != 3), -LLOQ), dv_var = "ODV", loq = 1, loq_method = 2))
})


#####dvconc_caption####

test_that("Caption for loess only returns correct text", {
  cap <- dvconc_caption(cfb = FALSE, loess = TRUE, linear = FALSE,
                        se_loess = FALSE, se_linear = FALSE)
  expect_match(cap, "LOESS fit overlaid")
  expect_no_match(cap, "Linear")
})

test_that("Caption for linear only returns correct text", {
  cap <- dvconc_caption(cfb = FALSE, loess = FALSE, linear = TRUE,
                        se_loess = FALSE, se_linear = FALSE)
  expect_match(cap, "Linear fit overlaid")
  expect_no_match(cap, "LOESS")
})

test_that("Caption for both loess and linear returns correct text", {
  cap <- dvconc_caption(cfb = FALSE, loess = TRUE, linear = TRUE,
                        se_loess = FALSE, se_linear = FALSE)
  expect_match(cap, "LOESS and linear fits overlaid")
})

test_that("Caption includes cfb reference line text when cfb = TRUE", {
  cap <- dvconc_caption(cfb = TRUE, loess = FALSE, linear = FALSE,
                        se_loess = FALSE, se_linear = FALSE)
  expect_match(cap, "null response")
})

test_that("Caption for no fits returns points only text", {
  cap <- dvconc_caption(cfb = FALSE, loess = FALSE, linear = FALSE,
                        se_loess = FALSE, se_linear = FALSE)
  expect_match(cap, "Points are observations")
  expect_no_match(cap, "overlaid")
})

##Test Edge Cases
test_that("linear = TRUE, loess = FALSE produces only linear smooth (no loess)", {
  p <- plot_dvconc(data_sad, dv_var = "ODV", idv_var = "CONC",
                   linear = TRUE, loess = FALSE)
  smooth_methods <- vapply(p$layers, function(l) {
    if (inherits(l$stat, "StatSmooth")) l$stat_params$method else NA_character_
  }, character(1))
  smooth_methods <- smooth_methods[!is.na(smooth_methods)]
  expect_true("lm" %in% smooth_methods)
  expect_false("loess" %in% smooth_methods)
})

test_that("linear = TRUE, loess = TRUE produces both smooth layers", {
  p <- plot_dvconc(data_sad, dv_var = "ODV", idv_var = "CONC",
                   linear = TRUE, loess = TRUE)
  smooth_count <- sum(vapply(p$layers, function(l) inherits(l$stat, "StatSmooth"), logical(1)))
  expect_equal(smooth_count, 2)
})

test_that("col_var = NULL works without error and has no color aesthetic", {
  p <- plot_dvconc(data_sad, dv_var = "ODV", idv_var = "CONC", col_var = NULL)
  expect_s3_class(p, "ggplot")
  expect_false("colour" %in% names(p$mapping))
})

##Test Argument Handling
test_that("Error if incorrect class for argument `data`", {
  expect_error(plot_dvconc(data = "data_sad", dv_var = "ODV", idv_var = "CONC"),
               regexp = "must be a `data.frame`")
})

test_that("Error if dv_var does not exist in data", {
  expect_error(plot_dvconc(data = data_sad, dv_var = "NONEXIST", idv_var = "CONC"),
               regexp = "must be variable.*in `data`")
})

test_that("Error if idv_var does not exist in data", {
  expect_error(plot_dvconc(data = data_sad, dv_var = "ODV", idv_var = "NONEXIST"),
               regexp = "must be variable.*in `data`")
})

test_that("Error if col_var does not exist in data", {
  expect_error(plot_dvconc(data = data_sad, dv_var = "ODV", idv_var = "CONC",
                           col_var = "NONEXIST"),
               regexp = "must be variable.*in `data`")
})

