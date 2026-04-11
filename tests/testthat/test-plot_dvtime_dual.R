#####plot_dvtime_dual####

##Test Output
test_that("Output is a `ggplot` plot object", {
  data <- df_addn(dplyr::mutate(data_sad_pd, Dose = DOSE), grp_var = Dose, sep = "mg")
  expect_s3_class(plot_dvtime_dual(data, dv_var1 = "ODV", dv_var2 = "ODV", col_var = "Dose"),
               class = "ggplot")
})

test_that("Output contains two patchwork panels", {
  data <- df_addn(dplyr::mutate(data_sad_pd, Dose = DOSE), grp_var = Dose, sep = "mg")
  p <- plot_dvtime_dual(data, dv_var1 = "ODV", dv_var2 = "ODV", col_var = "Dose")
  expect_equal(length(p$patches$plots) + 1, 2)
})

test_that("Custom ylab1 is applied to the first panel", {
  data <- df_addn(dplyr::mutate(data_sad_pd, Dose = DOSE), grp_var = Dose, sep = "mg")
  p <- plot_dvtime_dual(data, dv_var1 = "ODV", dv_var2 = "ODV", col_var = "Dose",
                        ylab1 = "PK", ylab2 = "PD")
  # patchwork: first plot goes to patches$plots, base plot is the second
  expect_equal(p$patches$plots[[1]]$labels$y, "PK")
})

test_that("Custom ylab2 is applied to the second panel", {
  data <- df_addn(dplyr::mutate(data_sad_pd, Dose = DOSE), grp_var = Dose, sep = "mg")
  p <- plot_dvtime_dual(data, dv_var1 = "ODV", dv_var2 = "ODV", col_var = "Dose",
                        ylab1 = "PK", ylab2 = "PD")
  # patchwork: base plot is the last added (dv2)
  expect_equal(p$labels$y, "PD")
})

test_that("onelegend = TRUE collects guides", {
  data <- df_addn(dplyr::mutate(data_sad_pd, Dose = DOSE), grp_var = Dose, sep = "mg")
  p <- plot_dvtime_dual(data, dv_var1 = "ODV", dv_var2 = "ODV", col_var = "Dose",
                        onelegend = TRUE)
  expect_equal(p$patches$layout$guides, "collect")
})

test_that("onelegend = FALSE does not collect guides", {
  data <- df_addn(dplyr::mutate(data_sad_pd, Dose = DOSE), grp_var = Dose, sep = "mg")
  p <- plot_dvtime_dual(data, dv_var1 = "ODV", dv_var2 = "ODV", col_var = "Dose",
                        onelegend = FALSE)
  expect_false(identical(p$patches$layout$guides, "collect"))
})

##Test Argument Handling
test_that("Error if incorrect class for argument `data`", {
  expect_error(plot_dvtime_dual(data = "not_a_df", dv_var1 = "ODV", dv_var2 = "ODV"))
})

test_that("Error if dvid_var does not exist in data", {
  expect_error(plot_dvtime_dual(data = data_sad_pd, dv_var1 = "ODV", dv_var2 = "ODV",
                                dvid_var = "NONEXIST"))
})
