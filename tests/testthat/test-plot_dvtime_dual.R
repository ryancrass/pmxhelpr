#####plot_dvtime_dual####

##Test Output
test_that("Output is a `ggplot` plot object", {
  data <- dplyr::mutate(data_sad, Dose = var_addn(DOSE, ID, sep = "mg"))
  expect_s3_class(plot_dvtime_dual(data, dv_var1 = "ODV", dv_var2 = "ODV", col_var = "Dose"),
               class = "ggplot")
})

test_that("Output contains two patchwork panels", {
  data <- dplyr::mutate(data_sad, Dose = var_addn(DOSE, ID, sep = "mg"))
  p <- plot_dvtime_dual(data, dv_var1 = "ODV", dv_var2 = "ODV", col_var = "Dose")
  expect_equal(length(p$patches$plots) + 1, 2)
})

test_that("Custom axis labels are applied via ggplot2 labs on dual panels", {
  data <- dplyr::mutate(data_sad, Dose = var_addn(DOSE, ID, sep = "mg"))
  p <- plot_dvtime_dual(data, dv_var1 = "ODV", dv_var2 = "ODV", col_var = "Dose")
  expect_s3_class(p, "ggplot")
})

test_that("onelegend = TRUE collects guides", {
  data <- dplyr::mutate(data_sad, Dose = var_addn(DOSE, ID, sep = "mg"))
  p <- plot_dvtime_dual(data, dv_var1 = "ODV", dv_var2 = "ODV", col_var = "Dose",
                        onelegend = TRUE)
  expect_equal(p$patches$layout$guides, "collect")
})

test_that("onelegend = FALSE does not collect guides", {
  data <- dplyr::mutate(data_sad, Dose = var_addn(DOSE, ID, sep = "mg"))
  p <- plot_dvtime_dual(data, dv_var1 = "ODV", dv_var2 = "ODV", col_var = "Dose",
                        onelegend = FALSE)
  expect_false(identical(p$patches$layout$guides, "collect"))
})

##Test Argument Handling
test_that("Error if incorrect class for argument `data`", {
  expect_error(plot_dvtime_dual(data = "not_a_df", dv_var1 = "ODV", dv_var2 = "ODV"))
})

test_that("Error if dvid_var does not exist in data", {
  expect_error(plot_dvtime_dual(data = data_sad, dv_var1 = "ODV", dv_var2 = "ODV",
                                dvid_var = "NONEXIST"))
})

##Test NSE Bare Names
test_that("plot_dvtime_dual accepts bare names", {
  data <- dplyr::mutate(data_sad, Dose = var_addn(DOSE, ID, sep = "mg"))
  expect_s3_class(plot_dvtime_dual(data, dv_var1 = ODV, dv_var2 = ODV, col_var = Dose),
                  "ggplot")
})
