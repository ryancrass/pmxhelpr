#####ggstylekit re-exports#####
# Smoke tests: the four verbs re-exported from ggstylekit work on
# pmxhelpr-styled plots under `library(pmxhelpr)` alone.

base_plot <- function() {
  plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", cent = "median")
}

# Built data for the first layer matching `geom` (and `stat`); benign
# "Removed rows" warnings from BLQ/NA data are unrelated and suppressed.
built_layer <- function(p, geom, stat = NULL) {
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  idx <- which(vapply(b$plot$layers, function(L)
    inherits(L$geom, geom) && (is.null(stat) || inherits(L$stat, stat)),
    logical(1)))[1]
  expect_false(is.na(idx))
  b$data[[idx]]
}

test_that("restyle_plot() recolors a role-tagged layer on a pmxhelpr plot", {
  p   <- restyle_plot(base_plot(), colors = c(obs_point = "#FF0000"))
  obs <- built_layer(p, "GeomPoint", "StatIdentity")
  expect_equal(unique(obs$colour), "#FF0000")
})

test_that("reveal(as = \"facet\") facets a pmxhelpr plot by an unmapped column", {
  p <- reveal(base_plot(), FOOD, as = "facet")
  expect_s3_class(p$facet, "FacetWrap")
})

test_that("reveal(as = \"colors\") maps an unmapped column onto colour", {
  p  <- reveal(base_plot(), FOOD, as = "colors")
  sc <- suppressWarnings(ggplot2::ggplot_build(p))$plot$scales$get_scales("colour")
  expect_false(is.null(sc))
  expect_gt(length(sc$get_breaks()), 1)
})

test_that("combine_styled_plots() arranges pmxhelpr plots into a patchwork", {
  skip_if_not_installed("patchwork")
  p <- combine_styled_plots(base_plot(), base_plot(), ncol = 2)
  expect_s3_class(p, "patchwork")
})

test_that("legend_spec() is accepted by the `legends` field of a preset", {
  s <- style_dvtime(legends = legend_spec(channel = "color", hide = TRUE))
  expect_s3_class(s, "ggstylekit_style_spec")
  expect_no_error(ggstylekit::validate_style_spec(s))
  d <- dplyr::mutate(dplyr::filter(data_sad, CMT != 3),
                     Dose = var_addn(DOSE, ID, sep = "mg"))
  expect_s3_class(plot_dvtime(d, dv_var = "ODV", cent = "median",
                              col_var = "Dose", style = s), "ggplot")
})
