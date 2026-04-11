#####plot_dvtime####

##Test Output
test_that("Output is a `ggplot` plot object", {
  expect_s3_class(plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = "ODV"),
               class = "ggplot")
})

test_that("Output plot maps variable TIME to the x aesthetic", {
  expect_equal(
    rlang::quo_name(plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = "ODV")$mapping$x),
    "TIME"
  )
})

test_that("Output plot maps variable DV to the y aesthetic", {
  expect_equal(
    rlang::quo_name(plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = "ODV")$mapping$y),
    "DV"
  )
})

test_that("Output plot contains a caption when argument `show_caption` = TRUE", {
  expect_equal(
    plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = "ODV")$labels$caption,
    "Solid circles and thick lines are the mean\nOpen circles are observations"
  )
})

test_that("Output plot does not contain a caption when `show_caption = FALSE`", {
  expect_no_match(
    names(plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", show_caption = FALSE)$labels),
    "caption"
  )
})

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
               regexp = "must be variables in `data`")
})

test_that("Error if NTIME variable specified in time_vars does not exist in `sim`", {
  expect_error(plot_dvtime(data = dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", time_vars = c(TIME = "NTFD")),
               regexp = "must be variables in `data`")
})

test_that("No Error if TIME and NTIME variables are specified as the same variable in time_vars", {
  expect_no_error(plot_dvtime(data = dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", time_vars = c(TIME = "NTIME", NTIME = "NTIME")))
})

test_that("Error if `col_var` does not exist in `data`", {
  expect_error(plot_dvtime(data = dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", col_var = "FOOD_f"),
               regexp = "argument `col_var` must be variables in `data`")
})

test_that("Error if `grp_var` does not exist in `data` and `grp_dv'` == TRUE", {
  expect_error(plot_dvtime(data = dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", grp_var = "SUBJID", grp_dv = TRUE),
               regexp = "argument `grp_var` must be variables in `data`")
})

test_that("No error if `grp_var` does not exist in `data` and `grp_dv'` == FALSE", {
  expect_no_error(plot_dvtime(data = dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", grp_var = "SUBJID", grp_dv = FALSE))
})

test_that("Error if `dose_var` does not exist in `data` and `dosenorm'` == TRUE", {
  expect_error(plot_dvtime(data = dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", dose_var = "DOSEN", dosenorm = TRUE),
               regexp = "argument `dose_var` must be variables in `data`")
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

##Test NSE Bare Names
test_that("plot_dvtime accepts bare names and produces ggplot", {
  expect_s3_class(plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = ODV), "ggplot")
})

test_that("plot_dvtime bare names match string output aesthetics", {
  p1 <- plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = ODV)
  p2 <- plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = "ODV")
  expect_equal(p1$labels, p2$labels)
})

test_that("plot_dvtime accepts bare col_var", {
  data <- df_addn(dplyr::mutate(dplyr::filter(data_sad, CMT != 3), Dose = DOSE), grp_var = Dose, sep = "mg")
  expect_s3_class(plot_dvtime(data, dv_var = ODV, col_var = Dose), "ggplot")
})
