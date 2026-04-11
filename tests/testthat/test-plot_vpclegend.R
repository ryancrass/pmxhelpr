##Test Output
test_that("Output is a `ggplot` object", {
  expect_s3_class(plot_vpclegend(),
                  class = "ggplot")
})

test_that("Output with custom ci and pi is a `ggplot` object", {
  expect_s3_class(plot_vpclegend(ci = c(0.025, 0.975), pi = c(0.025, 0.975)),
                  class = "ggplot")
})

test_that("Output with lloq specified is a `ggplot` object", {
  expect_s3_class(plot_vpclegend(lloq = 1),
                  class = "ggplot")
})

##Test shown elements
test_that("Output with obs_dv = FALSE is a `ggplot` object", {
  p <- plot_vpclegend(shown = list(obs_dv = FALSE))
  expect_s3_class(p, "ggplot")
})

test_that("Output with sim_median = TRUE is a `ggplot` object", {
  p <- plot_vpclegend(shown = list(sim_median = TRUE, sim_median_ci = FALSE))
  expect_s3_class(p, "ggplot")
})

test_that("Output with pi = TRUE and pi_ci = FALSE is a `ggplot` object", {
  p <- plot_vpclegend(shown = list(pi = TRUE, pi_ci = FALSE))
  expect_s3_class(p, "ggplot")
})

test_that("Output with pi_as_area = TRUE is a `ggplot` object", {
  p <- plot_vpclegend(shown = list(pi_as_area = TRUE, pi_ci = FALSE))
  expect_s3_class(p, "ggplot")
})

##Test theme update
test_that("Custom theme update via update argument is accepted", {
  p <- plot_vpclegend(update = list(obs_color = "#000000"))
  expect_s3_class(p, "ggplot")
})

##Test Arguments
