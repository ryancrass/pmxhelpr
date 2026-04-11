#####theme functions####

#####plot_dvtime_theme####

test_that("plot_dvtime_theme returns a named list with defaults", {
  theme <- plot_dvtime_theme()
  expect_type(theme, "list")
  expect_true(length(theme) > 0)
  expect_true(all(nchar(names(theme)) > 0))
})

test_that("plot_dvtime_theme update argument overrides a default", {
  theme <- plot_dvtime_theme(update = list(linewidth_ref = 99))
  expect_equal(theme$linewidth_ref, 99)
})

test_that("plot_dvtime_theme warns on invalid update key", {
  expect_warning(plot_dvtime_theme(update = list(fake_key = 1)),
                 regexp = "not a valid element")
})

test_that("plot_dvtime_theme default shape_point_cent is 16", {
  expect_equal(plot_dvtime_theme()$shape_point_cent, 16)
})

#####plot_dvconc_theme####

test_that("plot_dvconc_theme returns a named list with defaults", {
  theme <- plot_dvconc_theme()
  expect_type(theme, "list")
  expect_true("linewidth_loess" %in% names(theme))
  expect_true("color_loess" %in% names(theme))
})

test_that("plot_dvconc_theme update argument overrides a default", {
  theme <- plot_dvconc_theme(update = list(color_loess = "red"))
  expect_equal(theme$color_loess, "red")
})

test_that("plot_dvconc_theme warns on invalid update key", {
  expect_warning(plot_dvconc_theme(update = list(fake_key = 1)),
                 regexp = "not a valid element")
})

test_that("plot_dvconc_theme default color_loess is 'black'", {
  expect_equal(plot_dvconc_theme()$color_loess, "black")
})

#####plot_popgof_theme####

test_that("plot_popgof_theme returns a named list with defaults", {
  theme <- plot_popgof_theme()
  expect_type(theme, "list")
  expect_true("shape_point_obs" %in% names(theme))
  expect_true("linewidth_cent" %in% names(theme))
})

test_that("plot_popgof_theme update argument overrides a default", {
  theme <- plot_popgof_theme(update = list(linewidth_cent = 5))
  expect_equal(theme$linewidth_cent, 5)
})

test_that("plot_popgof_theme warns on invalid update key", {
  expect_warning(plot_popgof_theme(update = list(fake_key = 1)),
                 regexp = "not a valid element")
})

test_that("plot_popgof_theme default alpha_point_obs is 0.5", {
  expect_equal(plot_popgof_theme()$alpha_point_obs, 0.5)
})

#####pmxhelpr_vpc_theme####

test_that("pmxhelpr_vpc_theme returns a named list with defaults", {
  theme <- pmxhelpr_vpc_theme()
  expect_type(theme, "list")
  expect_true("obs_color" %in% names(theme))
  expect_true("sim_median_fill" %in% names(theme))
})

test_that("pmxhelpr_vpc_theme update argument overrides a default", {
  theme <- pmxhelpr_vpc_theme(update = list(obs_color = "#000000"))
  expect_equal(theme$obs_color, "#000000")
})

test_that("pmxhelpr_vpc_theme warns on invalid update key", {
  expect_warning(pmxhelpr_vpc_theme(update = list(fake_key = 1)),
                 regexp = "not a valid element")
})

test_that("pmxhelpr_vpc_theme default obs_color is '#0000FF'", {
  expect_equal(pmxhelpr_vpc_theme()$obs_color, "#0000FF")
})
