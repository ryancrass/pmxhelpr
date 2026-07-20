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

##Test style update
test_that("Custom style override via style argument is reflected in the obs point layer", {
  p <- plot_vpc_legend(style = style_vpc(colors = c(obs_point = "#000000")))
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

test_that("plot_vpc_legend rejects non-numeric lloq", {
  expect_error(plot_vpc_legend(lloq = "0.1"),
               regexp = "argument `lloq` must be class `numeric`")
  expect_error(plot_vpc_legend(lloq = c(0.1, NA)),
               regexp = "argument `lloq` must be class `numeric`")
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

##Test type dispatch (cont vs cens)

linetype_breaks <- function(p) {
  scale <- p$scales$scales[[which(vapply(p$scales$scales,
    function(s) "linetype" %in% s$aesthetics, logical(1)))]]
  scale$breaks
}
fill_breaks <- function(p) {
  scale <- p$scales$scales[[which(vapply(p$scales$scales,
    function(s) "fill" %in% s$aesthetics, logical(1)))]]
  scale$breaks
}

test_that("Default (type = 'cont') preserves existing 'Obs Med' / 'Sim Med' labels", {
  p <- plot_vpc_legend(shown = plot_vpc_shown(
    obs_point = TRUE, obs_pi_line = TRUE, obs_median_line = TRUE,
    sim_pi_line = TRUE, sim_pi_ci = TRUE, sim_pi_area = TRUE,
    sim_median_line = TRUE, sim_median_ci = TRUE))
  lt <- linetype_breaks(p)
  fl <- fill_breaks(p)
  expect_true("Obs Med" %in% lt)
  expect_true("Sim Med" %in% lt)
  expect_true("Obs 5th and 95th" %in% lt)
  expect_true("Sim 5th - 95th" %in% lt)
  expect_true("Sim 90% CI Med" %in% fl)
  expect_true("Sim 90% CI 5th and 95th" %in% fl)
  expect_true("Sim 5th - 95th" %in% fl)
})

test_that("type = 'cens' relabels all three 'Med' labels to 'Prop BLQ'", {
  p <- plot_vpc_legend(type = "cens",
                       shown = plot_vpc_shown(
                         obs_median_line = TRUE,
                         sim_median_line = TRUE,
                         sim_median_ci   = TRUE))
  lt <- linetype_breaks(p)
  fl <- fill_breaks(p)
  expect_true("Obs Prop BLQ" %in% lt)
  expect_true("Sim Prop BLQ" %in% lt)
  expect_true("Sim 90% CI Prop BLQ" %in% fl)
  ## Old labels should not appear
  expect_false("Obs Med" %in% lt)
  expect_false("Sim Med" %in% lt)
  expect_false("Sim 90% CI Med" %in% fl)
})

test_that("type = 'cens' suppresses pi labels even when shown enables them", {
  p <- plot_vpc_legend(type = "cens",
                       shown = plot_vpc_shown(
                         obs_pi_line = TRUE, sim_pi_line = TRUE,
                         sim_pi_ci = TRUE, sim_pi_area = TRUE))
  lt <- linetype_breaks(p)
  fl <- fill_breaks(p)
  ## No pi-related strings (defaults pi = c(0.05, 0.95), ci = 0.90)
  expect_false(any(grepl("5th and 95th", lt)))
  expect_false(any(grepl("5th - 95th",   lt)))
  expect_false(any(grepl("5th - 95th",   fl)))
  expect_false(any(grepl("90% CI 5th and 95th", fl)))
})

test_that("type = 'cens' still respects shown toggles for central-tendency layers", {
  p <- plot_vpc_legend(type = "cens",
                       shown = plot_vpc_shown(sim_median_line = FALSE))
  lt <- linetype_breaks(p)
  expect_false("Sim Prop BLQ" %in% lt)
  ## Obs proportion line should remain (default shown$obs_median_line = TRUE)
  expect_true("Obs Prop BLQ" %in% lt)
})

test_that("lloq labeling still works under type = 'cens'", {
  p <- plot_vpc_legend(type = "cens", lloq = c(1, 2))
  lt <- linetype_breaks(p)
  expect_true("LLOQ = 1" %in% lt)
  expect_true("LLOQ = 2" %in% lt)
})

test_that("type = 'cens' suppresses pi-related geom layers regardless of shown", {
  p <- plot_vpc_legend(type = "cens",
                       shown = plot_vpc_shown(
                         obs_pi_line = TRUE, sim_pi_line = TRUE,
                         sim_pi_ci = TRUE, sim_pi_area = TRUE))
  ## Count rect layers: only sim_median_ci should produce one (default ON)
  n_rect <- sum(vapply(p$layers, function(l) inherits(l$geom, "GeomRect"),
                       logical(1)))
  expect_equal(n_rect, 1L)
})

##Test Arguments
