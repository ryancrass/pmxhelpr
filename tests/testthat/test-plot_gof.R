#####plot_gof####

##Test Output
test_that("Output is a `ggplot` plot object", {
  expect_s3_class(plot_gof(data_sad_pkfit, dv_var = "ODV"),
               class = "ggplot")
})

test_that("plot_gof rejects invalid `cent`", {
  expect_error(plot_gof(data_sad_pkfit, dv_var = "ODV", cent = "wat"),
               regexp = "should be one of")
})

test_that("Output plot maps variable TIME to the x aesthetic", {
  expect_equal(
    rlang::quo_name(plot_gof(data_sad_pkfit, dv_var = "ODV")$mapping$x),
    "TIME"
  )
})

test_that("Output plot maps variable DV to the y aesthetic", {
  expect_equal(
    rlang::quo_name(plot_gof(data_sad_pkfit, dv_var = "ODV")$mapping$y),
    "DV"
  )
})

test_that("Output plot contains a caption when argument `show_caption` = TRUE", {
  expect_equal(
    plot_gof(data_sad_pkfit, dv_var = "ODV")$labels$caption,
    "Solid circles and thick lines are the mean"
  )
})

test_that("Output plot does not contain a caption when `show_caption = FALSE`", {
  expect_no_match(
    names(plot_gof(data_sad_pkfit, dv_var = "ODV", show_caption = FALSE)$labels),
    "caption"
  )
})

##Test Argument Handling
test_that("Error if incorrect class for arugmument `data`", {
  expect_error(plot_gof(data = "data_sad_pkfit", dv_var = "ODV"),
               regexp = "argument `data` must be a `data.frame`")
})

test_that("Error if time_var does not exist in `data`", {
  expect_error(plot_gof(data = data_sad_pkfit, dv_var = "ODV", time_var = "ATFD"),
               regexp = "must be variable.*in `data`")
})

test_that("Error if ntime_var does not exist in `data`", {
  expect_error(plot_gof(data = data_sad_pkfit, dv_var = "ODV", ntime_var = "NTFD"),
               regexp = "must be variable.*in `data`")
})

test_that("No error if time_var and ntime_var specified as the same variable", {
  expect_no_error(plot_gof(data = data_sad_pkfit, dv_var = "ODV",
                           time_var = "NTIME", ntime_var = "NTIME"))
})

test_that("Error if `id_var` does not exist in `data`", {
  expect_error(plot_gof(data = data_sad_pkfit, dv_var = "ODV", id_var = "SUBJID"),
               regexp = "argument `id_var` must be variable.*in `data`")
})

test_that("No error when `id_var` is NULL (default)", {
  expect_no_error(plot_gof(data = data_sad_pkfit, dv_var = "ODV"))
})

test_that("Error if `dose_var` does not exist in `data` and `dosenorm'` == TRUE", {
  expect_error(plot_gof(data = data_sad_pkfit, dv_var = "ODV", dose_var = "DOSEN", dosenorm = TRUE),
               regexp = "argument `dose_var` must be variable.*in `data`")
})

test_that("No error if `dose_var` does not exist in `data` and `dosenorm'` == FALSE", {
  expect_no_error(suppressWarnings(plot_gof(data = data_sad_pkfit, dv_var = "ODV", dose_var = "DOSEN", dosenorm = FALSE)))
})

test_that("Error if dv_var does not exist in `data`", {
  expect_error(plot_gof(data = data_sad_pkfit, dv_var = "NONEXIST"),
               regexp = "must be variable.*in `data`")
})

##Test NSE Bare Names
test_that("plot_gof accepts bare names", {
  expect_s3_class(plot_gof(data_sad_pkfit, dv_var = "ODV",
                               id_var = ID, dose_var = DOSE, dosenorm = TRUE),
                  "ggplot")
})

test_that("plot_gof warns when shown has no active elements", {
  shown <- plot_gof_shown(obs = FALSE, dv = FALSE, pred = FALSE, ipred = FALSE)
  expect_warning(plot_gof(data_sad_pkfit, dv_var = "ODV", shown = shown),
                 regexp = "no overlay layers")
})

