#####plot_popgof####

##Test Output
test_that("Output is a `ggplot` plot object", {
  expect_s3_class(plot_popgof(data_sad_pkfit, output_vars = c(DV = "ODV")),
               class = "ggplot")
})

test_that("Output plot maps variable TIME to the x aesthetic", {
  expect_equal(
    rlang::quo_name(plot_popgof(data_sad_pkfit, output_vars = c(DV = "ODV"))$mapping$x),
    "TIME"
  )
})

test_that("Output plot maps variable DV to the y aesthetic", {
  expect_equal(
    rlang::quo_name(plot_popgof(data_sad_pkfit, output_vars = c(DV = "ODV"))$mapping$y),
    "DV"
  )
})

test_that("Output plot contains a caption when argument `show_caption` = TRUE", {
  expect_equal(
    plot_popgof(data_sad_pkfit, output_vars = c(DV = "ODV"))$labels$caption,
    "Solid circles and thick lines are the mean\nOpen circles are observations"
  )
})

test_that("Output plot does not contain a caption when `show_caption = FALSE`", {
  expect_no_match(
    names(plot_popgof(data_sad_pkfit, output_vars = c(DV = "ODV"), show_caption = FALSE)$labels),
    "caption"
  )
})

#test_that("Output plot maps variable specified in `grp_var` to the group aesthetic", {
#  expect_equal(
#    plot_popgof(data_sad_pkfit, output_vars = c(DV = "ODV"), grp_dv = TRUE)$labels$group,
#    "ID"
#  )
#})


##Test Argument Handling
test_that("Error if incorrect class for arugmument `data`", {
  expect_error(plot_popgof(data = "data_sad_pkfit", output_vars = c(DV = "ODV")),
               regexp = "argument `data` must be a `data.frame`")
})

test_that("Error if TIME variable specified in time_vars does not exist in `sim`", {
  expect_error(plot_popgof(data = data_sad_pkfit, output_vars = c(DV = "ODV"), time_vars = c(TIME = "ATFD")),
               regexp = "must be variables in `data`")
})

test_that("Error if NTIME variable specified in time_vars does not exist in `sim`", {
  expect_error(plot_popgof(data = data_sad_pkfit, output_vars = c(DV = "ODV"), time_vars = c(TIME = "NTFD")),
               regexp = "must be variables in `data`")
})

test_that("No error if TIME and NTIME variables specified as the same variable in time_vars", {
  expect_no_error(plot_popgof(data = data_sad_pkfit, output_vars = c(DV = "ODV"),
                           time_vars = c(TIME = "NTIME", NTIME = "NTIME")))
})

test_that("Error if `grp_var` does not exist in `data` and `grp_dv'` == TRUE", {
  expect_error(plot_popgof(data = data_sad_pkfit, output_vars = c(DV = "ODV"), grp_var = "SUBJID", grp_dv = TRUE),
               regexp = "argument `grp_var` must be variables in `data`")
})

test_that("No error if `grp_var` does not exist in `data` and `grp_dv'` == FALSE", {
  expect_no_error(plot_popgof(data = data_sad_pkfit, output_vars = c(DV = "ODV"), grp_var = "SUBJID", grp_dv = FALSE))
})

test_that("Error if `dose_var` does not exist in `data` and `dosenorm'` == TRUE", {
  expect_error(plot_popgof(data = data_sad_pkfit, output_vars = c(DV = "ODV"), dose_var = "DOSEN", dosenorm = TRUE),
               regexp = "argument `dose_var` must be variables in `data`")
})

test_that("No error if `dose_var` does not exist in `data` and `dosenorm'` == FALSE", {
  expect_no_error(plot_popgof(data = data_sad_pkfit, output_vars = c(DV = "ODV"), dose_var = "DOSEN", dosenorm = FALSE))
})

test_that("Error if DV variable specified in output_vars does not exist in `data`", {
  expect_error(plot_popgof(data = data_sad_pkfit, output_vars = c(DV = "NONEXIST")),
               regexp = "must be variables in `data`")
})

##Test NSE Bare Names
test_that("plot_popgof accepts bare names", {
  expect_s3_class(plot_popgof(data_sad_pkfit, output_vars = c(DV = "ODV"),
                               grp_var = ID, dose_var = DOSE, dosenorm = TRUE),
                  "ggplot")
})

