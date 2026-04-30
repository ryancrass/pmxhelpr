#####theme functions####

#####plot_dvtime_theme####

test_that("plot_dvtime_theme returns a named list with defaults", {
  theme <- plot_dvtime_theme()
  expect_type(theme, "list")
  expect_true(length(theme) > 0)
  expect_true(all(nchar(names(theme)) > 0))
})

test_that("plot_dvtime_theme override merges correctly", {
  theme <- plot_dvtime_theme(ref = pmx_line(linewidth = 99))
  expect_equal(theme$ref$linewidth, 99)
  # Other ref defaults preserved
  expect_equal(theme$ref$linetype, 2)
})

test_that("plot_dvtime_theme errors on invalid element field", {
  expect_error(plot_dvtime_theme(obs_point = pmx_point(fake_key = 1)),
               regexp = "unused argument")
})

test_that("plot_dvtime_theme default cent_point shape is 16", {
  expect_equal(plot_dvtime_theme()$cent_point$shape, 16)
})

test_that("plot_dvtime_theme pmx_style shortcut applies to point and line", {
  theme <- plot_dvtime_theme(obs = pmx_style(alpha = 0.3))
  expect_equal(theme$obs_point$alpha, 0.3)
  expect_equal(theme$obs_line$alpha, 0.3)
  # Other defaults preserved
  expect_equal(theme$obs_point$shape, 1)
  expect_equal(theme$obs_line$linewidth, 0.5)
})

test_that("plot_dvtime_theme returns all expected keys", {
  theme <- plot_dvtime_theme()
  expected <- c("obs_point", "obs_line", "cent_point", "cent_line", "errorbar", "ref", "loq")
  expect_setequal(names(theme), expected)
})

test_that("plot_dvtime_theme elements have correct classes", {
  theme <- plot_dvtime_theme()
  expect_s3_class(theme$obs_point, "pmx_point")
  expect_s3_class(theme$obs_line, "pmx_line")
  expect_s3_class(theme$cent_point, "pmx_point")
  expect_s3_class(theme$cent_line, "pmx_line")
  expect_s3_class(theme$errorbar, "pmx_errorbar")
  expect_s3_class(theme$ref, "pmx_line")
  expect_s3_class(theme$loq, "pmx_line")
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

test_that("plot_dvconc_theme returns all expected keys", {
  theme <- plot_dvconc_theme()
  expected <- c("obs", "ref", "loess", "linear")
  expect_setequal(names(theme), expected)
})

test_that("plot_dvconc_theme elements have correct classes", {
  theme <- plot_dvconc_theme()
  expect_s3_class(theme$obs, "pmx_point")
  expect_s3_class(theme$ref, "pmx_line")
  expect_s3_class(theme$loess, "pmx_trend")
  expect_s3_class(theme$linear, "pmx_trend")
})

#####plot_gof_theme####

test_that("plot_gof_theme returns a named list with defaults", {
  theme <- plot_gof_theme()
  expect_type(theme, "list")
  expect_true("obs_point" %in% names(theme))
  expect_true("dv_point" %in% names(theme))
  expect_true("pred_line" %in% names(theme))
  expect_true("ipred_point" %in% names(theme))
})

test_that("plot_gof_theme override merges correctly", {
  theme <- plot_gof_theme(dv_line = pmx_line(linewidth = 5))
  expect_equal(theme$dv_line$linewidth, 5)
})

test_that("plot_gof_theme errors on invalid element field", {
  expect_error(plot_gof_theme(dv_point = pmx_point(fake_key = 1)),
               regexp = "unused argument")
})

test_that("plot_gof_theme default obs_point alpha is 0.5", {
  expect_equal(plot_gof_theme()$obs_point$alpha, 0.5)
})

test_that("plot_gof_theme default colors match expected", {
  theme <- plot_gof_theme()
  expect_equal(theme$dv_point$color, "blue")
  expect_equal(theme$pred_point$color, "red")
  expect_equal(theme$ipred_point$color, "green")
  expect_equal(theme$obs_point$color, "darkgrey")
  # Line colors match point colors
  expect_equal(theme$dv_line$color, "blue")
  expect_equal(theme$pred_line$color, "red")
})

test_that("plot_gof_theme pmx_style shortcut applies color to point and line", {
  theme <- plot_gof_theme(pred = pmx_style(color = "purple"))
  expect_equal(theme$pred_point$color, "purple")
  expect_equal(theme$pred_line$color, "purple")
  # Other defaults preserved
  expect_equal(theme$pred_point$size, 1.25)
  expect_equal(theme$pred_line$linewidth, 0.75)
})

test_that("plot_gof_theme returns all expected keys", {
  theme <- plot_gof_theme()
  expected <- c("obs_point", "obs_line", "dv_point", "dv_line",
                "pred_point", "pred_line", "ipred_point", "ipred_line",
                "errorbar", "ref", "loq")
  expect_setequal(names(theme), expected)
})

test_that("plot_gof_theme elements have correct classes", {
  theme <- plot_gof_theme()
  expect_s3_class(theme$obs_point, "pmx_point")
  expect_s3_class(theme$obs_line, "pmx_line")
  expect_s3_class(theme$dv_point, "pmx_point")
  expect_s3_class(theme$dv_line, "pmx_line")
  expect_s3_class(theme$pred_point, "pmx_point")
  expect_s3_class(theme$pred_line, "pmx_line")
  expect_s3_class(theme$ipred_point, "pmx_point")
  expect_s3_class(theme$ipred_line, "pmx_line")
  expect_s3_class(theme$errorbar, "pmx_errorbar")
  expect_s3_class(theme$ref, "pmx_line")
  expect_s3_class(theme$loq, "pmx_line")
})

#####plot_vpc_theme####

test_that("plot_vpc_theme returns a named list with defaults", {
  theme <- plot_vpc_theme()
  expect_type(theme, "list")
  expect_true("obs" %in% names(theme))
  expect_true("sim_median" %in% names(theme))
})

test_that("plot_vpc_theme override merges correctly", {
  theme <- plot_vpc_theme(obs = pmx_point(color = "#000000"))
  expect_equal(theme$obs$color, "#000000")
})

test_that("plot_vpc_theme errors on invalid element field", {
  expect_error(plot_vpc_theme(obs = pmx_point(fake_key = 1)),
               regexp = "unused argument")
})

test_that("plot_vpc_theme default obs color is '#0000FF'", {
  expect_equal(plot_vpc_theme()$obs$color, "#0000FF")
})

test_that("plot_vpc_theme returns all expected keys", {
  theme <- plot_vpc_theme()
  expected <- c("obs", "obs_median", "obs_ci", "sim_pi", "sim_median", "loq", "bin_sep")
  expect_setequal(names(theme), expected)
})

test_that("plot_vpc_theme elements have correct classes", {
  theme <- plot_vpc_theme()
  expect_s3_class(theme$obs, "pmx_point")
  expect_s3_class(theme$obs_median, "pmx_line")
  expect_s3_class(theme$obs_ci, "pmx_line")
  expect_s3_class(theme$sim_pi, "pmx_ribbon")
  expect_s3_class(theme$sim_median, "pmx_ribbon")
  expect_s3_class(theme$loq, "pmx_line")
  expect_s3_class(theme$bin_sep, "pmx_line")
})
