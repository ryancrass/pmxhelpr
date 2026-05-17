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
    "Solid circles and thick lines are the mean"
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
