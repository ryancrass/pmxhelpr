#####plot_dvconc####

##Test Output
test_that("Output is a `ggplot` plot object", {
  data <- dplyr::mutate(data_sad, Dose = var_addn(DOSE, ID, sep = "mg"))
  expect_s3_class(plot_dvconc(data, dv_var = "ODV", idv_var = "CONC", col_var = "Dose"),
               class = "ggplot")
})

test_that("Output plot maps variable IDV to the x aesthetic", {
  expect_equal(
    rlang::quo_name(plot_dvconc(data_sad, dv_var = "ODV", idv_var = "CONC")$mapping$x),
    "IDV"
  )
})

test_that("Output plot maps variable DV to the y aesthetic", {
  expect_equal(
    rlang::quo_name(plot_dvconc(data_sad, dv_var = "ODV", idv_var = "CONC")$mapping$y),
    "DV"
  )
})

test_that("Output plot contains a caption when argument `show_caption` = TRUE", {
  p <- plot_dvconc(data_sad, dv_var = "ODV", idv_var = "CONC")
  expect_true("caption" %in% names(p$labels))
})

test_that("Output plot does not contain a caption when `show_caption = FALSE`", {
  expect_no_match(
    names(plot_dvconc(data_sad, dv_var = "ODV", idv_var = "CONC", show_caption = FALSE)$labels),
    "caption"
  )
})

test_that("Custom axis labels are applied via ggplot2 labs", {
  p <- plot_dvconc(data_sad, dv_var = "ODV", idv_var = "CONC") +
    ggplot2::labs(y = "Custom Y", x = "Custom X")
  expect_equal(p$labels$y, "Custom Y")
  expect_equal(p$labels$x, "Custom X")
})

##Test col_trend
test_that("Color aesthetic is mapped when col_trend = TRUE", {
  data <- dplyr::mutate(data_sad, Dose = var_addn(DOSE, ID, sep = "mg"))
  p <- plot_dvconc(data, dv_var = "ODV", idv_var = "CONC",
                   col_var = "Dose", col_trend = TRUE)
  expect_true("colour" %in% names(p$mapping))
})

test_that("Color aesthetic is not mapped when col_trend = FALSE", {
  data <- dplyr::mutate(data_sad, Dose = var_addn(DOSE, ID, sep = "mg"))
  p <- plot_dvconc(data, dv_var = "ODV", idv_var = "CONC",
                   col_var = "Dose", col_trend = FALSE)
  expect_false("colour" %in% names(p$mapping))
})

##Test log transforms
test_that("Log y-axis is applied when log_y = TRUE", {
  p <- plot_dvconc(data_sad, dv_var = "ODV", idv_var = "CONC", log_y = TRUE)
  scale_classes <- vapply(p$scales$scales, function(s) class(s)[1], character(1))
  expect_true(any(grepl("ScaleContinuousPosition", scale_classes)))
})

test_that("Log x-axis is applied via ggplot2 scale", {
  p <- plot_dvconc(data_sad, dv_var = "ODV", idv_var = "CONC") +
    ggplot2::scale_x_log10()
  scale_classes <- vapply(p$scales$scales, function(s) class(s)[1], character(1))
  expect_true(any(grepl("ScaleContinuousPosition", scale_classes)))
})

##Test cfb
test_that("Reference line is added when cfb = TRUE", {
  p <- plot_dvconc(data_sad, dv_var = "ODV", idv_var = "CONC", cfb = TRUE)
  layer_types <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomHline" %in% layer_types)
})

test_that("No reference line when cfb = FALSE", {
  p <- plot_dvconc(data_sad, dv_var = "ODV", idv_var = "CONC", cfb = FALSE)
  layer_types <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_false("GeomHline" %in% layer_types)
})

##Test NSE Bare Names
test_that("plot_dvconc accepts bare names and produces ggplot", {
  data <- dplyr::mutate(data_sad, Dose = var_addn(DOSE, ID, sep = "mg"))
  expect_s3_class(plot_dvconc(data, dv_var = ODV, idv_var = CONC, col_var = Dose), "ggplot")
})

test_that("plot_dvconc bare names match string output", {
  data <- dplyr::mutate(data_sad, Dose = var_addn(DOSE, ID, sep = "mg"))
  p1 <- plot_dvconc(data, dv_var = ODV, idv_var = CONC)
  p2 <- plot_dvconc(data, dv_var = "ODV", idv_var = "CONC")
  expect_equal(p1$labels, p2$labels)
})
