#####plot_dvconc####

##Test Output
test_that("Output is a `ggplot` plot object", {
  data <- df_addn(dplyr::mutate(data_sad_pd, Dose = DOSE), grp_var = Dose, sep = "mg")
  expect_s3_class(plot_dvconc(data, dv_var = "ODV", idv_var = "CONC", col_var = "Dose"),
               class = "ggplot")
})

test_that("Output plot maps variable IDV to the x aesthetic", {
  expect_equal(
    rlang::quo_name(plot_dvconc(data_sad_pd, dv_var = "ODV", idv_var = "CONC")$mapping$x),
    "IDV"
  )
})

test_that("Output plot maps variable DV to the y aesthetic", {
  expect_equal(
    rlang::quo_name(plot_dvconc(data_sad_pd, dv_var = "ODV", idv_var = "CONC")$mapping$y),
    "DV"
  )
})

test_that("Output plot contains a caption when argument `show_caption` = TRUE", {
  p <- plot_dvconc(data_sad_pd, dv_var = "ODV", idv_var = "CONC")
  expect_true("caption" %in% names(p$labels))
})

test_that("Output plot does not contain a caption when `show_caption = FALSE`", {
  expect_no_match(
    names(plot_dvconc(data_sad_pd, dv_var = "ODV", idv_var = "CONC", show_caption = FALSE)$labels),
    "caption"
  )
})

test_that("Custom axis labels are applied", {
  p <- plot_dvconc(data_sad_pd, dv_var = "ODV", idv_var = "CONC",
                   ylab = "Custom Y", xlab = "Custom X")
  expect_equal(p$labels$y, "Custom Y")
  expect_equal(p$labels$x, "Custom X")
})

##Test col_trend
test_that("Color aesthetic is mapped when col_trend = TRUE", {
  data <- df_addn(dplyr::mutate(data_sad_pd, Dose = DOSE), grp_var = Dose, sep = "mg")
  p <- plot_dvconc(data, dv_var = "ODV", idv_var = "CONC",
                   col_var = "Dose", col_trend = TRUE)
  expect_true("colour" %in% names(p$mapping))
})

test_that("Color aesthetic is not mapped when col_trend = FALSE", {
  data <- df_addn(dplyr::mutate(data_sad_pd, Dose = DOSE), grp_var = Dose, sep = "mg")
  p <- plot_dvconc(data, dv_var = "ODV", idv_var = "CONC",
                   col_var = "Dose", col_trend = FALSE)
  expect_false("colour" %in% names(p$mapping))
})

##Test log transforms
test_that("Log y-axis is applied when log_y = TRUE", {
  p <- plot_dvconc(data_sad_pd, dv_var = "ODV", idv_var = "CONC", log_y = TRUE)
  scale_classes <- vapply(p$scales$scales, function(s) class(s)[1], character(1))
  expect_true(any(grepl("ScaleContinuousPosition", scale_classes)))
})

test_that("Log x-axis is applied when log_x = TRUE", {
  p <- plot_dvconc(data_sad_pd, dv_var = "ODV", idv_var = "CONC", log_x = TRUE)
  scale_classes <- vapply(p$scales$scales, function(s) class(s)[1], character(1))
  expect_true(any(grepl("ScaleContinuousPosition", scale_classes)))
})

##Test cfb
test_that("Reference line is added when cfb = TRUE", {
  p <- plot_dvconc(data_sad_pd, dv_var = "ODV", idv_var = "CONC", cfb = TRUE)
  layer_types <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomHline" %in% layer_types)
})

test_that("No reference line when cfb = FALSE", {
  p <- plot_dvconc(data_sad_pd, dv_var = "ODV", idv_var = "CONC", cfb = FALSE)
  layer_types <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_false("GeomHline" %in% layer_types)
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
  p <- plot_dvconc(data_sad_pd, dv_var = "ODV", idv_var = "CONC",
                   linear = TRUE, loess = FALSE)
  smooth_methods <- vapply(p$layers, function(l) {
    if (inherits(l$stat, "StatSmooth")) l$stat_params$method else NA_character_
  }, character(1))
  smooth_methods <- smooth_methods[!is.na(smooth_methods)]
  expect_true("lm" %in% smooth_methods)
  expect_false("loess" %in% smooth_methods)
})

test_that("linear = TRUE, loess = TRUE produces both smooth layers", {
  p <- plot_dvconc(data_sad_pd, dv_var = "ODV", idv_var = "CONC",
                   linear = TRUE, loess = TRUE)
  smooth_count <- sum(vapply(p$layers, function(l) inherits(l$stat, "StatSmooth"), logical(1)))
  expect_equal(smooth_count, 2)
})

test_that("col_var = NULL works without error and has no color aesthetic", {
  p <- plot_dvconc(data_sad_pd, dv_var = "ODV", idv_var = "CONC", col_var = NULL)
  expect_s3_class(p, "ggplot")
  expect_false("colour" %in% names(p$mapping))
})

##Test Argument Handling
test_that("Error if incorrect class for argument `data`", {
  expect_error(plot_dvconc(data = "data_sad_pd", dv_var = "ODV", idv_var = "CONC"),
               regexp = "must be a `data.frame`")
})

test_that("Error if dv_var does not exist in data", {
  expect_error(plot_dvconc(data = data_sad_pd, dv_var = "NONEXIST", idv_var = "CONC"),
               regexp = "must be variables in `data`")
})

test_that("Error if idv_var does not exist in data", {
  expect_error(plot_dvconc(data = data_sad_pd, dv_var = "ODV", idv_var = "NONEXIST"),
               regexp = "must be variables in `data`")
})

test_that("Error if col_var does not exist in data", {
  expect_error(plot_dvconc(data = data_sad_pd, dv_var = "ODV", idv_var = "CONC",
                           col_var = "NONEXIST"),
               regexp = "must be variables in `data`")
})

##Test NSE Bare Names
test_that("plot_dvconc accepts bare names and produces ggplot", {
  data <- df_addn(dplyr::mutate(data_sad_pd, Dose = DOSE), grp_var = Dose, sep = "mg")
  expect_s3_class(plot_dvconc(data, dv_var = ODV, idv_var = CONC, col_var = Dose), "ggplot")
})

test_that("plot_dvconc bare names match string output", {
  data <- df_addn(dplyr::mutate(data_sad_pd, Dose = DOSE), grp_var = Dose, sep = "mg")
  p1 <- plot_dvconc(data, dv_var = ODV, idv_var = CONC)
  p2 <- plot_dvconc(data, dv_var = "ODV", idv_var = "CONC")
  expect_equal(p1$labels, p2$labels)
})
