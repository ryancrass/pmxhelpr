#####plot_dvconc####

##Test Output
test_that("Output is a `ggplot` plot object", {
  data <- dplyr::mutate(data_sad, Dose = var_addn(DOSE, ID, sep = "mg"))
  expect_s3_class(suppressWarnings(plot_dvconc(data, dv_var = "ODV", idv_var = "CONC", col_var = "Dose")),
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
  p <- suppressWarnings(plot_dvconc(data, dv_var = "ODV", idv_var = "CONC",
                   col_var = "Dose", col_trend = FALSE))
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

##Test ref
test_that("Reference line is added when ref is specified", {
  p <- plot_dvconc(data_sad, dv_var = "ODV", idv_var = "CONC", ref = 0)
  layer_types <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomHline" %in% layer_types)
})

test_that("No reference line when ref is NULL", {
  p <- plot_dvconc(data_sad, dv_var = "ODV", idv_var = "CONC", ref = NULL)
  layer_types <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_false("GeomHline" %in% layer_types)
})

##Test NSE Bare Names
test_that("plot_dvconc accepts bare names and produces ggplot", {
  data <- dplyr::mutate(data_sad, Dose = var_addn(DOSE, ID, sep = "mg"))
  expect_s3_class(suppressWarnings(plot_dvconc(data, dv_var = ODV, idv_var = CONC, col_var = Dose)), "ggplot")
})

test_that("plot_dvconc bare names match string output", {
  data <- dplyr::mutate(data_sad, Dose = var_addn(DOSE, ID, sep = "mg"))
  p1 <- plot_dvconc(data, dv_var = ODV, idv_var = CONC)
  p2 <- plot_dvconc(data, dv_var = "ODV", idv_var = "CONC")
  expect_equal(p1$labels, p2$labels)
})


##Test ggstylekit fixed-aesthetic styling contract
# style_plot() must fill the per-series fixed aesthetics on series_layer-tagged,
# non-data-mapped role layers from the style_dvconc() preset maps. A ggstylekit
# regression (fixed in 0.2.x) silently reverted these to ggplot2 geom defaults;
# for the loess trend line that meant the line colour flipped to ggplot2's
# geom_smooth default (#3366FF). Expected values mirror R/style_presets.R.
styled_layer <- function(p, geom, stat = NULL) {
  b   <- suppressWarnings(ggplot2::ggplot_build(p))
  idx <- which(vapply(b$plot$layers, function(L)
    inherits(L$geom, geom) && (is.null(stat) || inherits(L$stat, stat)),
    logical(1)))[1]
  expect_false(is.na(idx))
  b$data[[idx]]
}

test_that("plot_dvconc fills role-keyed fixed aesthetics from style_dvconc()", {
  sd <- style_dvconc()
  p  <- suppressWarnings(plot_dvconc(data_sad, dv_var = "ODV", idv_var = "CONC"))

  obs <- styled_layer(p, "GeomPoint", "StatIdentity")
  expect_equal(unique(obs$shape), unname(sd$shapes["obs_point"]))
  expect_equal(unique(obs$size),  unname(sd$sizes["obs_point"]))
  expect_equal(unique(obs$alpha), unname(sd$alphas["obs_point"]))

  trend <- styled_layer(p, "GeomSmooth")            # loess (default)
  expect_equal(unique(trend$colour),    unname(sd$colors["loess"]))
  expect_equal(unique(trend$linewidth), unname(sd$linewidths["loess"]))
})


##Test col_var legend order contract with ggstylekit (>= 0.3.0)
test_that("plot_dvconc (col_trend) legend follows factor level order, not row order", {
  d <- dplyr::mutate(data_sad, Dose = var_addn(DOSE, ID, sep = "mg"))
  d <- d[order(-d$DOSE), ]   # reverse-sort rows: first-appearance != level order
  p <- suppressWarnings(plot_dvconc(d, dv_var = "ODV", idv_var = "CONC",
                                    col_var = "Dose", col_trend = TRUE))
  sc <- suppressWarnings(ggplot2::ggplot_build(p))$plot$scales$get_scales("colour")
  expect_equal(as.character(sc$get_breaks()), levels(d$Dose))
})
