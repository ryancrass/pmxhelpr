#####plot_dvtime####

##Test Output
test_that("Output is a `ggplot` plot object", {
  expect_s3_class(plot_dvtime(data_sad, dv_var = "ODV"),
               class = "ggplot")
})

test_that("Output plot maps variable TIME to the x aesthetic", {
  expect_equal(
    rlang::quo_name(plot_dvtime(data_sad, dv_var = "ODV")$mapping$x),
    "TIME"
  )
})

test_that("Output plot maps variable DV to the y aesthetic", {
  expect_equal(
    rlang::quo_name(plot_dvtime(data_sad, dv_var = "ODV")$mapping$y),
    "DV"
  )
})

test_that("Output plot contains a caption when argument `show_caption` = TRUE", {
  expect_equal(
    plot_dvtime(data_sad, dv_var = "ODV")$labels$caption,
    "Solid circles and thick lines are the mean\nOpen circles are observations"
  )
})

test_that("Output plot does not contain a caption when `show_caption = FALSE`", {
  expect_no_match(
    names(plot_dvtime(data_sad, dv_var = "ODV", show_caption = FALSE)$labels),
    "caption"
  )
})

#test_that("Output plot maps variable specified in `col_var` to the color aesthetic", {
#  expect_equal(
#    plot_dvtime(data_sad, dv_var = "ODV", col_var = "DOSE")$labels$colour,
#    "DOSE"
#  )
#})

#test_that("Output plot maps variable specified in `grp_var` to the group aesthetic", {
#  expect_equal(
#    plot_dvtime(data_sad, dv_var = "ODV", grp_dv = TRUE)$labels$group,
#    "ID"
#  )
#})

##Test Argument Handling
test_that("Error if incorrect class for arugmument `data`", {
  expect_error(plot_dvtime(data = "data_sad", dv_var = "ODV"),
               regexp = "argument `data` must be a `data.frame`")
})

test_that("Error if TIME variable specified in time_vars does not exist in `data`", {
  expect_error(plot_dvtime(data = data_sad, dv_var = "ODV", time_vars = c(TIME = "ATFD")),
               regexp = "must be variables in `data`")
})

test_that("Error if NTIME variable specified in time_vars does not exist in `data`", {
  expect_error(plot_dvtime(data = data_sad, dv_var = "ODV", time_vars = c(TIME = "NTFD")),
               regexp = "must be variables in `data`")
})

test_that("No Error if TIME variable specified as NA in time_vars", {
  expect_no_error(plot_dvtime(data = data_sad, dv_var = "ODV", time_vars = c(TIME = NULL)))
})

test_that("Error if NTIME variable specified as NA in time_vars", {
  expect_error(plot_dvtime(data = data_sad, dv_var = "ODV", time_vars = c(NTIME = NA)),
               regexp = "Selections can't have missing values.")
})

test_that("Error if `col_var` does not exist in `data`", {
  expect_error(plot_dvtime(data = data_sad, dv_var = "ODV", col_var = "FOOD_f"),
               regexp = "argument `col_var` must be variables in `data`")
})

test_that("Error if `grp_var` does not exist in `data` and `grp_dv'` == TRUE", {
  expect_error(plot_dvtime(data = data_sad, dv_var = "ODV", grp_var = "SUBJID", grp_dv = TRUE),
               regexp = "argument `grp_var` must be variables in `data`")
})

test_that("No error if `grp_var` does not exist in `data` and `grp_dv'` == FALSE", {
  expect_no_error(plot_dvtime(data = data_sad, dv_var = "ODV", grp_var = "SUBJID", grp_dv = FALSE))
})

test_that("Error if `dose_var` does not exist in `data` and `dosenorm'` == TRUE", {
  expect_error(plot_dvtime(data = data_sad, dv_var = "ODV", dose_var = "DOSEN", dosenorm = TRUE),
               regexp = "argument `dose_var` must be variables in `data`")
})

test_that("No error if `dose_var` does not exist in `data` and `dosenorm'` == FALSE", {
  expect_no_error(plot_dvtime(data = data_sad, dv_var = "ODV", dose_var = "DOSEN", dosenorm = FALSE))
})

test_that("Error if argument `loq_method` is not one of 0, 1, 2", {
  expect_error(plot_dvtime(data = data_sad, dv_var = "ODV", loq_method = 3),
               regexp = "argument `loq_method` must be 0, 1, or 2")
})

test_that("Error if argument `loq` is not coercible to numeric and `loq_method` = 1", {
  expect_error(plot_dvtime(data = data_sad, dv_var = "ODV", loq_method = 1, loq = "$"),
               regexp = "If `loq_method` is 1 or 2, then a numeric variable `LLOQ` must be present in `data` \n or argument `loq` must specified and numeric or coercible to numeric")
})

test_that("Error if argument `loq` is not coercible to numeric and `loq_method` = 2", {
  expect_error(plot_dvtime(data = data_sad, dv_var = "ODV", loq_method = 2, loq = "$"),
               regexp = "If `loq_method` is 1 or 2, then a numeric variable `LLOQ` must be present in `data` \n or argument `loq` must specified and numeric or coercible to numeric")
})

test_that("No error if variable `LLOQ` exists in `data` and `loq` = NULL and `loq_method` != 0", {
  expect_no_error(plot_dvtime(data = data_sad, dv_var = "ODV", loq_method = 1))
})

test_that("Error if variable `LLOQ` does not exist in `data` and `loq` = NULL and `loq_method` = 1", {
  expect_error(plot_dvtime(data = dplyr::select(data_sad, -LLOQ), dv_var = "ODV", loq_method = 1),
               regexp = "If `loq_method` is 1 or 2, then a numeric variable `LLOQ` must be present in `data` \n or argument `loq` must specified and numeric or coercible to numeric")
})

test_that("Error if variable `LLOQ` does not exist in `data` and `loq` = NULL and `loq_method` = `", {
  expect_error(plot_dvtime(data = dplyr::select(data_sad, -LLOQ), dv_var = "ODV", loq_method = 2),
               regexp = "If `loq_method` is 1 or 2, then a numeric variable `LLOQ` must be present in `data` \n or argument `loq` must specified and numeric or coercible to numeric")
})

test_that("No error if variable `LLOQ` does not exist in `data` and `loq` = a numeric value and `loq_method` = 1", {
  expect_no_error(plot_dvtime(data = dplyr::select(data_sad, -LLOQ), dv_var = "ODV", loq = 1, loq_method = 1))
})

test_that("No error if variable `LLOQ` does not exist in `data` and `loq` = a numeric value and `loq_method` = 2", {
  expect_no_error(plot_dvtime(data = dplyr::select(data_sad, -LLOQ), dv_var = "ODV", loq = 1, loq_method = 2))
})
