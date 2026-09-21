#####plot_dvtime####

##Test Output
test_that("Output is a `ggplot` plot object", {
  expect_s3_class(plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = "ODV"),
               class = "ggplot")
})

test_that("plot_dvtime rejects invalid `cent`", {
  expect_error(plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", cent = "wat"),
               regexp = "should be one of")
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
    "Thick lines are the mean"
  )
})

test_that("Output plot does not contain a caption when `show_caption = FALSE`", {
  expect_no_match(
    names(plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", show_caption = FALSE)$labels),
    "caption"
  )
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
  data <- dplyr::mutate(dplyr::filter(data_sad, CMT != 3), Dose = var_addn(DOSE, ID, sep = "mg"))
  expect_s3_class(plot_dvtime(data, dv_var = ODV, col_var = Dose), "ggplot")
})

##Test cent branches
test_that("plot_dvtime cent = 'mean' adds no errorbar layer", {
  p <- plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", cent = "mean")
  has_errorbar <- any(vapply(p$layers,
                              function(l) inherits(l$geom, "GeomErrorbar"),
                              logical(1)))
  expect_false(has_errorbar)
})

test_that("plot_dvtime cent = 'mean_sdl' adds an errorbar layer", {
  p <- plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", cent = "mean_sdl")
  has_errorbar <- any(vapply(p$layers,
                              function(l) inherits(l$geom, "GeomErrorbar"),
                              logical(1)))
  expect_true(has_errorbar)
})

test_that("plot_dvtime cent = 'median_iqr' adds an errorbar layer", {
  p <- plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", cent = "median_iqr")
  has_errorbar <- any(vapply(p$layers,
                              function(l) inherits(l$geom, "GeomErrorbar"),
                              logical(1)))
  expect_true(has_errorbar)
})

test_that("plot_dvtime cent = 'none' produces fewer layers than cent = 'median'", {
  p_with <- plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", cent = "median")
  p_none <- plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", cent = "none")
  expect_lt(length(p_none$layers), length(p_with$layers))
})

##Test loq_method branches
test_that("plot_dvtime loq_method = 0 omits the LOQ reference line", {
  p <- plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", loq_method = 0)
  has_hline <- any(vapply(p$layers,
                           function(l) inherits(l$geom, "GeomHline"),
                           logical(1)))
  expect_false(has_hline)
})

test_that("plot_dvtime loq_method = 1 with explicit loq adds an LOQ reference hline", {
  p <- plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = "ODV",
                   loq = 0.5, loq_method = 1)
  has_hline <- any(vapply(p$layers,
                           function(l) inherits(l$geom, "GeomHline"),
                           logical(1)))
  expect_true(has_hline)
})

test_that("plot_dvtime loq_method = 2 with explicit loq adds an LOQ reference hline", {
  p <- plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = "ODV",
                   loq = 0.5, loq_method = 2)
  has_hline <- any(vapply(p$layers,
                           function(l) inherits(l$geom, "GeomHline"),
                           logical(1)))
  expect_true(has_hline)
})

test_that("plot_dvtime dosenorm = TRUE suppresses LOQ hline but keeps BLQ caption", {
  d <- dplyr::filter(data_sad, CMT != 3)
  suppressWarnings({
    p <- plot_dvtime(d, dv_var = "ODV", dose_var = "DOSE",
                     loq = 0.5, loq_method = 1, dosenorm = TRUE)
  })
  has_hline <- any(vapply(p$layers,
                           function(l) inherits(l$geom, "GeomHline"),
                           logical(1)))
  expect_false(has_hline)
  expect_match(p$labels$caption, "imputed to 1/2 LLOQ")
})

test_that("plot_dvtime loq_method character aliases add the LOQ hline and BLQ caption", {
  d <- dplyr::filter(data_sad, CMT != 3)
  for (alias in c("postdose", "all")) {
    p <- plot_dvtime(d, dv_var = "ODV", loq = 0.5, loq_method = alias)
    has_hline <- any(vapply(p$layers,
                             function(l) inherits(l$geom, "GeomHline"),
                             logical(1)))
    expect_true(has_hline)
    expect_match(p$labels$caption, "imputed to 1/2 LLOQ")
  }
})

##Test dosenorm branch
test_that("plot_dvtime dosenorm = TRUE divides DV by dose", {
  d <- dplyr::filter(data_sad, CMT != 3)
  suppressWarnings({
    p_raw  <- plot_dvtime(d, dv_var = "ODV", dose_var = "DOSE", dosenorm = FALSE)
    p_norm <- plot_dvtime(d, dv_var = "ODV", dose_var = "DOSE", dosenorm = TRUE)
    raw_y  <- ggplot2::layer_data(p_raw,  1)$y
    norm_y <- ggplot2::layer_data(p_norm, 1)$y
  })
  expect_false(identical(raw_y, norm_y))
})

##Test id_var (spaghetti lines)
test_that("plot_dvtime id_var adds a GeomLine layer", {
  d <- dplyr::filter(data_sad, CMT != 3)
  count_lines <- function(p) sum(vapply(p$layers,
                                          function(l) inherits(l$geom, "GeomLine"),
                                          logical(1)))
  p_no <- plot_dvtime(d, dv_var = "ODV")
  p_id <- plot_dvtime(d, dv_var = "ODV", id_var = "ID")
  expect_gt(count_lines(p_id), count_lines(p_no))
})

##Test mixed-CMT input warning
test_that("plot_dvtime warns when input has multiple CMT values after EVID filter", {
  expect_warning(plot_dvtime(data_sad, dv_var = "ODV"),
                 regexp = "Multiple unique values of `CMT`")
})


##Test ggstylekit fixed-aesthetic styling contract
# style_plot() must fill the per-series fixed aesthetics (shape/size/alpha/
# linewidth/...) on series_layer-tagged, non-data-mapped role layers, sourced
# from the style_*() preset maps keyed by role name. A ggstylekit regression
# (fixed in 0.2.x) silently reverted these to ggplot2 geom defaults; this guard
# fails loudly if that recurs. Expected values mirror R/style_presets.R.
styled_layer <- function(p, geom, stat = NULL) {
  b   <- suppressWarnings(ggplot2::ggplot_build(p))
  idx <- which(vapply(b$plot$layers, function(L)
    inherits(L$geom, geom) && (is.null(stat) || inherits(L$stat, stat)),
    logical(1)))[1]
  expect_false(is.na(idx))
  b$data[[idx]]
}

test_that("plot_dvtime fills role-keyed fixed aesthetics from style_dvtime()", {
  sd <- style_dvtime()
  p  <- plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = "ODV")

  obs <- styled_layer(p, "GeomPoint", "StatIdentity")
  expect_equal(unique(obs$shape), unname(sd$shapes["obs_point"]))
  expect_equal(unique(obs$size),  unname(sd$sizes["obs_point"]))
  expect_equal(unique(obs$alpha), unname(sd$alphas["obs_point"]))

  cent_pt <- styled_layer(p, "GeomPoint", "StatSummary")
  expect_equal(unique(cent_pt$shape), unname(sd$shapes["cent_point"]))
  expect_equal(unique(cent_pt$size),  unname(sd$sizes["cent_point"]))

  cent_ln <- styled_layer(p, "GeomLine", "StatSummary")
  expect_equal(unique(cent_ln$linewidth), unname(sd$linewidths["cent_line"]))
})


##Test col_var legend order / per-series value contract with ggstylekit (>= 0.3.0)
# Discrete legends and per-series values follow the factor's declared level
# order. `ggplot_build()` trains the scale; benign "Removed rows" warnings from
# BLQ/NA data are unrelated and suppressed.
colour_scale <- function(p) {
  suppressWarnings(ggplot2::ggplot_build(p))$plot$scales$get_scales("colour")
}

test_that("plot_dvtime legend follows factor level order regardless of row order", {
  d <- dplyr::filter(data_sad, CMT %in% c(1, 2))
  d <- dplyr::mutate(d, Dose = var_addn(DOSE, ID, sep = "mg"))
  d <- d[order(-d$DOSE), ]   # reverse-sort rows: first-appearance != level order
  sc <- colour_scale(plot_dvtime(d, dv_var = "ODV", cent = "median", col_var = "Dose"))
  expect_equal(as.character(sc$get_breaks()), levels(d$Dose))
})

test_that("plot_dvtime pins named series colors and leaves others on the palette", {
  d <- dplyr::filter(data_sad, CMT %in% c(1, 2))
  d <- dplyr::mutate(d, Dose = var_addn(DOSE, ID, sep = "mg"))
  lo <- levels(d$Dose)[1]
  hi <- levels(d$Dose)[length(levels(d$Dose))]
  sty <- style_dvtime(colors = stats::setNames(c("#111111", "#999999"), c(lo, hi)))
  sc  <- colour_scale(plot_dvtime(d, dv_var = "ODV", cent = "median",
                                  col_var = "Dose", style = sty))
  brk  <- as.character(sc$get_breaks())
  vals <- stats::setNames(sc$map(brk), brk)
  expect_equal(unname(vals[lo]), "#111111")            # pin lands on its series
  expect_equal(unname(vals[hi]), "#999999")
  others <- vals[setdiff(brk, c(lo, hi))]
  expect_false(any(others %in% c("#111111", "#999999")))  # pins don't shift others
  expect_equal(length(unique(others)), length(others))    # un-pinned stay distinct
})

test_that("plot_dvtime orders a numeric-labelled character col_var by value", {
  d <- dplyr::filter(data_sad, CMT %in% c(1, 2))
  d <- dplyr::mutate(d, DoseChr = paste(DOSE, "mg"))
  d <- d[order(-d$DOSE), ]   # rows reverse-sorted to isolate the ordering rule
  sc <- colour_scale(plot_dvtime(d, dv_var = "ODV", cent = "median", col_var = "DoseChr"))
  # numeric-aware, not lexicographic ("100 mg" would sort before "50 mg")
  expect_equal(as.character(sc$get_breaks()),
               c("10 mg", "50 mg", "100 mg", "200 mg", "400 mg"))
})

##Test style validation
test_that("plot_dvtime aborts early on a non-style_spec `style`", {
  expect_error(plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = "ODV",
                           style = list(alphas = c(obs_point = 0))),
               regexp = "argument `style` must be a `ggstylekit::style_spec\\(\\)` object")
})
