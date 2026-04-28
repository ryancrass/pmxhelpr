#####plot_popgof####

##Test Output
test_that("Output is a `ggplot` plot object", {
  expect_s3_class(plot_popgof(data_sad_pkfit, dv_var = "ODV"),
               class = "ggplot")
})

test_that("Output plot maps variable TIME to the x aesthetic", {
  expect_equal(
    rlang::quo_name(plot_popgof(data_sad_pkfit, dv_var = "ODV")$mapping$x),
    "TIME"
  )
})

test_that("Output plot maps variable DV to the y aesthetic", {
  expect_equal(
    rlang::quo_name(plot_popgof(data_sad_pkfit, dv_var = "ODV")$mapping$y),
    "DV"
  )
})

test_that("Output plot contains a caption when argument `show_caption` = TRUE", {
  expect_equal(
    plot_popgof(data_sad_pkfit, dv_var = "ODV")$labels$caption,
    "Solid circles and thick lines are the mean\nOpen circles are observations"
  )
})

test_that("Output plot does not contain a caption when `show_caption = FALSE`", {
  expect_no_match(
    names(plot_popgof(data_sad_pkfit, dv_var = "ODV", show_caption = FALSE)$labels),
    "caption"
  )
})

#test_that("Output plot maps variable specified in `grp_var` to the group aesthetic", {
#  expect_equal(
#    plot_popgof(data_sad_pkfit, dv_var = "ODV", grp_dv = TRUE)$labels$group,
#    "ID"
#  )
#})


##Test Argument Handling
test_that("Error if incorrect class for arugmument `data`", {
  expect_error(plot_popgof(data = "data_sad_pkfit", dv_var = "ODV"),
               regexp = "argument `data` must be a `data.frame`")
})

test_that("Error if time_var does not exist in `data`", {
  expect_error(plot_popgof(data = data_sad_pkfit, dv_var = "ODV", time_var = "ATFD"),
               regexp = "must be variable.*in `data`")
})

test_that("Error if ntime_var does not exist in `data`", {
  expect_error(plot_popgof(data = data_sad_pkfit, dv_var = "ODV", ntime_var = "NTFD"),
               regexp = "must be variable.*in `data`")
})

test_that("No error if time_var and ntime_var specified as the same variable", {
  expect_no_error(plot_popgof(data = data_sad_pkfit, dv_var = "ODV",
                           time_var = "NTIME", ntime_var = "NTIME"))
})

test_that("Error if `grp_var` does not exist in `data` and `grp_dv'` == TRUE", {
  expect_error(plot_popgof(data = data_sad_pkfit, dv_var = "ODV", grp_var = "SUBJID", grp_dv = TRUE),
               regexp = "argument `grp_var` must be variable.*in `data`")
})

test_that("No error if `grp_var` does not exist in `data` and `grp_dv'` == FALSE", {
  expect_no_error(plot_popgof(data = data_sad_pkfit, dv_var = "ODV", grp_var = "SUBJID", grp_dv = FALSE))
})

test_that("Error if `dose_var` does not exist in `data` and `dosenorm'` == TRUE", {
  expect_error(plot_popgof(data = data_sad_pkfit, dv_var = "ODV", dose_var = "DOSEN", dosenorm = TRUE),
               regexp = "argument `dose_var` must be variable.*in `data`")
})

test_that("No error if `dose_var` does not exist in `data` and `dosenorm'` == FALSE", {
  expect_no_error(plot_popgof(data = data_sad_pkfit, dv_var = "ODV", dose_var = "DOSEN", dosenorm = FALSE))
})

test_that("Error if dv_var does not exist in `data`", {
  expect_error(plot_popgof(data = data_sad_pkfit, dv_var = "NONEXIST"),
               regexp = "must be variable.*in `data`")
})

##Test NSE Bare Names
test_that("plot_popgof accepts bare names", {
  expect_s3_class(plot_popgof(data_sad_pkfit, dv_var = "ODV",
                               grp_var = ID, dose_var = DOSE, dosenorm = TRUE),
                  "ggplot")
})

