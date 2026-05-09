##Test Output
test_that("Output is a `ggplot` object", {
  expect_s3_class(plot_vpc_legend(),
                  class = "ggplot")
})

test_that("Output with custom ci and pi is a `ggplot` object", {
  expect_s3_class(plot_vpc_legend(ci = 0.95, pi = c(0.025, 0.975)),
                  class = "ggplot")
})

##Test shown elements
test_that("plot_vpc_legend sim_median_line = TRUE adds a 'Sim Med' linetype label", {
  p <- plot_vpc_legend(shown = plot_vpc_shown(sim_median_line = TRUE,
                                                sim_median_ci   = FALSE))
  linetype_scale <- p$scales$scales[[which(vapply(p$scales$scales,
    function(s) "linetype" %in% s$aesthetics, logical(1)))]]
  expect_true("Sim Med" %in% linetype_scale$breaks)
})

test_that("plot_vpc_legend sim_pi_line = TRUE adds a line-geom layer when sim_pi_ci = FALSE", {
  count_lines <- function(p) sum(vapply(p$layers,
                                          function(l) inherits(l$geom, "GeomLine"),
                                          logical(1)))
  p_off <- plot_vpc_legend(shown = plot_vpc_shown(sim_pi_line = FALSE,
                                                    sim_pi_ci   = FALSE,
                                                    sim_median_ci = FALSE))
  p_on  <- plot_vpc_legend(shown = plot_vpc_shown(sim_pi_line = TRUE,
                                                    sim_pi_ci   = FALSE,
                                                    sim_median_ci = FALSE))
  expect_gt(count_lines(p_on), count_lines(p_off))
})

##Test theme update
test_that("Custom theme override via theme argument is reflected in the obs point layer", {
  p <- plot_vpc_legend(theme = plot_vpc_theme(obs_point = pmx_point(color = "#000000")))
  obs_layer <- p$layers[vapply(p$layers,
                                 function(l) inherits(l$geom, "GeomPoint"),
                                 logical(1))][[1]]
  expect_equal(obs_layer$aes_params$colour, "#000000")
})

##Test Legend Content
test_that("Default legend contains 'Obs Med' linetype label", {
  p <- plot_vpc_legend()
  linetype_scale <- p$scales$scales[[which(vapply(p$scales$scales,
    function(s) "linetype" %in% s$aesthetics, logical(1)))]]
  expect_true("Obs Med" %in% linetype_scale$breaks)
})

test_that("Custom pi = c(0.025, 0.975) produces label text with '2.5th' and '97.5th'", {
  p <- plot_vpc_legend(pi = c(0.025, 0.975))
  linetype_scale <- p$scales$scales[[which(vapply(p$scales$scales,
    function(s) "linetype" %in% s$aesthetics, logical(1)))]]
  pi_break <- linetype_scale$breaks[grepl("2.5th", linetype_scale$breaks)]
  expect_length(pi_break, 1)
  expect_match(pi_break, "97.5th")
})

##Test layer composition
test_that("plot_vpc_legend with lloq adds layers vs the no-lloq default", {
  p_no  <- plot_vpc_legend()
  p_loq <- plot_vpc_legend(lloq = c(0.1, 1.0))
  expect_gt(length(p_loq$layers), length(p_no$layers))
})

test_that("plot_vpc_legend obs_point = FALSE removes the obs point geom", {
  p_on  <- plot_vpc_legend(shown = plot_vpc_shown(obs_point = TRUE))
  p_off <- plot_vpc_legend(shown = plot_vpc_shown(obs_point = FALSE))
  has_point <- function(p) any(vapply(p$layers,
                                       function(l) inherits(l$geom, "GeomPoint"),
                                       logical(1)))
  expect_true(has_point(p_on))
  expect_false(has_point(p_off))
})

test_that("plot_vpc_legend sim_pi_area = TRUE adds a rect-geom layer for the area entry", {
  count_rects <- function(p) sum(vapply(p$layers,
                                          function(l) inherits(l$geom, "GeomRect"),
                                          logical(1)))
  p_off <- plot_vpc_legend(shown = plot_vpc_shown(sim_pi_area = FALSE,
                                                    sim_pi_ci   = FALSE,
                                                    sim_median_ci = FALSE))
  p_on  <- plot_vpc_legend(shown = plot_vpc_shown(sim_pi_area = TRUE,
                                                    sim_pi_ci   = FALSE,
                                                    sim_median_ci = FALSE))
  expect_gt(count_rects(p_on), count_rects(p_off))
})

##Test Arguments
