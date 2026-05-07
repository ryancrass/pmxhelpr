##Test Output
test_that("Output is a `ggplot` object", {
  expect_s3_class(plot_vpc_legend(),
                  class = "ggplot")
})

test_that("Output with custom ci and pi is a `ggplot` object", {
  expect_s3_class(plot_vpc_legend(ci = 0.95, pi = c(0.025, 0.975)),
                  class = "ggplot")
})

test_that("Output with lloq specified is a `ggplot` object", {
  expect_s3_class(plot_vpc_legend(lloq = 1),
                  class = "ggplot")
})

##Test shown elements
test_that("Output with obs_point = FALSE is a `ggplot` object", {
  p <- plot_vpc_legend(shown = plot_vpc_shown(obs_point = FALSE))
  expect_s3_class(p, "ggplot")
})

test_that("Output with sim_median_line = TRUE is a `ggplot` object", {
  p <- plot_vpc_legend(shown = plot_vpc_shown(sim_median_line = TRUE, sim_median_ci = FALSE))
  expect_s3_class(p, "ggplot")
})

test_that("Output with sim_pi_line = TRUE and sim_pi_ci = FALSE is a `ggplot` object", {
  p <- plot_vpc_legend(shown = plot_vpc_shown(sim_pi_line = TRUE, sim_pi_ci = FALSE))
  expect_s3_class(p, "ggplot")
})

test_that("Output with sim_pi_area = TRUE is a `ggplot` object", {
  p <- plot_vpc_legend(shown = plot_vpc_shown(sim_pi_area = TRUE, sim_pi_ci = FALSE))
  expect_s3_class(p, "ggplot")
})

##Test theme update
test_that("Custom theme update via theme argument is accepted", {
  p <- plot_vpc_legend(theme = plot_vpc_theme(obs_point = pmx_point(color = "#000000")))
  expect_s3_class(p, "ggplot")
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

##Test Arguments
