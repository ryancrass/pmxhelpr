#####theme functions####

#####plot_dvtime_theme####

test_that("plot_dvtime_theme returns a named list with defaults", {
  theme <- plot_dvtime_theme()
  expect_type(theme, "list")
  expect_true(length(theme) > 0)
  expect_true(all(nchar(names(theme)) > 0))
})

test_that("plot_dvtime_theme override merges correctly", {
  theme <- plot_dvtime_theme(ref = pmx_ref(linewidth = 99))
  expect_equal(theme$ref$linewidth, 99)
  # Other ref defaults preserved
  expect_equal(theme$ref$linetype, 2)
})

test_that("plot_dvtime_theme errors on invalid element field", {
  expect_error(plot_dvtime_theme(obs = pmx_obs(fake_key = 1)),
               regexp = "unused argument")
})

test_that("plot_dvtime_theme default cent shape is 16", {
  expect_equal(plot_dvtime_theme()$cent$shape, 16)
})

#####plot_dvconc_theme####

test_that("plot_dvconc_theme returns a named list with defaults", {
  theme <- plot_dvconc_theme()
  expect_type(theme, "list")
  expect_true("loess" %in% names(theme))
  expect_true("color" %in% names(theme$loess))
})

test_that("plot_dvconc_theme override merges correctly", {
  theme <- plot_dvconc_theme(loess = pmx_trend(color = "red"))
  expect_equal(theme$loess$color, "red")
})

test_that("plot_dvconc_theme errors on invalid element field", {
  expect_error(plot_dvconc_theme(loess = pmx_trend(fake_key = 1)),
               regexp = "unused argument")
})

test_that("plot_dvconc_theme default loess color is 'black'", {
  expect_equal(plot_dvconc_theme()$loess$color, "black")
})

#####plot_popgof_theme####

test_that("plot_popgof_theme returns a named list with defaults", {
  theme <- plot_popgof_theme()
  expect_type(theme, "list")
  expect_true("obs" %in% names(theme))
  expect_true("cent" %in% names(theme))
})

test_that("plot_popgof_theme override merges correctly", {
  theme <- plot_popgof_theme(cent = pmx_cent(linewidth = 5))
  expect_equal(theme$cent$linewidth, 5)
})

test_that("plot_popgof_theme errors on invalid element field", {
  expect_error(plot_popgof_theme(cent = pmx_cent(fake_key = 1)),
               regexp = "unused argument")
})

test_that("plot_popgof_theme default obs alpha is 0.5", {
  expect_equal(plot_popgof_theme()$obs$alpha, 0.5)
})

#####plot_vpc_theme####

test_that("plot_vpc_theme returns a named list with defaults", {
  theme <- plot_vpc_theme()
  expect_type(theme, "list")
  expect_true("obs" %in% names(theme))
  expect_true("sim_median" %in% names(theme))
})

test_that("plot_vpc_theme override merges correctly", {
  theme <- plot_vpc_theme(obs = pmx_vpc_point(color = "#000000"))
  expect_equal(theme$obs$color, "#000000")
})

test_that("plot_vpc_theme errors on invalid element field", {
  expect_error(plot_vpc_theme(obs = pmx_vpc_point(fake_key = 1)),
               regexp = "unused argument")
})

test_that("plot_vpc_theme default obs color is '#0000FF'", {
  expect_equal(plot_vpc_theme()$obs$color, "#0000FF")
})
