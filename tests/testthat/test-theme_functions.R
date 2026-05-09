#####theme functions####

#####plot_dvtime_theme####

test_that("plot_dvtime_theme override merges correctly", {
  theme <- plot_dvtime_theme(ref_line = pmx_line(linewidth = 99))
  expect_equal(theme$ref_line$linewidth, 99)
  # Other ref_line defaults preserved
  expect_equal(theme$ref_line$linetype, 2)
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
  expected <- c("obs_point", "obs_line", "cent_point", "cent_line", "cent_errorbar", "ref_line", "loq_line")
  expect_setequal(names(theme), expected)
})

test_that("plot_dvtime_theme elements have correct classes", {
  theme <- plot_dvtime_theme()
  expect_s3_class(theme$obs_point, "pmx_point")
  expect_s3_class(theme$obs_line, "pmx_line")
  expect_s3_class(theme$cent_point, "pmx_point")
  expect_s3_class(theme$cent_line, "pmx_line")
  expect_s3_class(theme$cent_errorbar, "pmx_errorbar")
  expect_s3_class(theme$ref_line, "pmx_line")
  expect_s3_class(theme$loq_line, "pmx_line")
})

#####plot_dvconc_theme####

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
  expected <- c("obs_point", "ref_line", "loess", "linear")
  expect_setequal(names(theme), expected)
})

test_that("plot_dvconc_theme elements have correct classes", {
  theme <- plot_dvconc_theme()
  expect_s3_class(theme$obs_point, "pmx_point")
  expect_s3_class(theme$ref_line, "pmx_line")
  expect_s3_class(theme$loess, "pmx_trend")
  expect_s3_class(theme$linear, "pmx_trend")
})

#####plot_doseprop_theme####

test_that("plot_doseprop_theme override merges correctly", {
  theme <- plot_doseprop_theme(linear = pmx_trend(color = "red"))
  expect_equal(theme$linear$color, "red")
})

test_that("plot_doseprop_theme errors on invalid element field", {
  expect_error(plot_doseprop_theme(linear = pmx_trend(fake_key = 1)),
               regexp = "unused argument")
})

test_that("plot_doseprop_theme returns all expected keys", {
  theme <- plot_doseprop_theme()
  expected <- c("obs_point", "linear")
  expect_setequal(names(theme), expected)
})

test_that("plot_doseprop_theme elements have correct classes", {
  theme <- plot_doseprop_theme()
  expect_s3_class(theme$obs_point, "pmx_point")
  expect_s3_class(theme$linear, "pmx_trend")
})

test_that("plot_doseprop_theme obs shortcut applies to obs_point", {
  theme <- plot_doseprop_theme(obs = pmx_style(color = "red", alpha = 0.3))
  expect_equal(theme$obs_point$color, "red")
  expect_equal(theme$obs_point$alpha, 0.3)
})

#####plot_gof_theme####

test_that("plot_gof_theme override merges correctly", {
  theme <- plot_gof_theme(cent_line = pmx_line(linewidth = 5))
  expect_equal(theme$cent_line$linewidth, 5)
})

test_that("plot_gof_theme errors on invalid element field", {
  expect_error(plot_gof_theme(cent_point = pmx_point(fake_key = 1)),
               regexp = "unused argument")
})

test_that("plot_gof_theme default obs_point alpha is 0.5", {
  expect_equal(plot_gof_theme()$obs_point$alpha, 0.5)
})

test_that("plot_gof_theme default cent_color match expected", {
  theme <- plot_gof_theme()
  expect_equal(theme$cent_color$dv, "blue")
  expect_equal(theme$cent_color$pred, "red")
  expect_equal(theme$cent_color$ipred, "green")
  expect_equal(theme$obs_point$color, "darkgrey")
})

test_that("plot_gof_theme pmx_color override merges correctly", {
  theme <- plot_gof_theme(cent_color = pmx_color(pred = "purple"))
  expect_equal(theme$cent_color$pred, "purple")
  # Other colors preserved
  expect_equal(theme$cent_color$dv, "blue")
  expect_equal(theme$cent_color$ipred, "green")
})

test_that("plot_gof_theme returns all expected keys", {
  theme <- plot_gof_theme()
  expected <- c("obs_point", "obs_line", "cent_point", "cent_line",
                "cent_errorbar", "ref_line", "loq_line", "cent_color")
  expect_setequal(names(theme), expected)
})

test_that("plot_gof_theme elements have correct classes", {
  theme <- plot_gof_theme()
  expect_s3_class(theme$obs_point, "pmx_point")
  expect_s3_class(theme$obs_line, "pmx_line")
  expect_s3_class(theme$cent_point, "pmx_point")
  expect_s3_class(theme$cent_line, "pmx_line")
  expect_s3_class(theme$cent_errorbar, "pmx_errorbar")
  expect_s3_class(theme$ref_line, "pmx_line")
  expect_s3_class(theme$loq_line, "pmx_line")
  expect_s3_class(theme$cent_color, "pmx_color")
})

#####plot_vpc_theme####

test_that("plot_vpc_theme override merges correctly", {
  theme <- plot_vpc_theme(obs_point = pmx_point(color = "#000000"))
  expect_equal(theme$obs_point$color, "#000000")
})

test_that("plot_vpc_theme errors on invalid element field", {
  expect_error(plot_vpc_theme(obs_point = pmx_point(fake_key = 1)),
               regexp = "unused argument")
})

test_that("plot_vpc_theme default obs_point color is '#0000FF'", {
  expect_equal(plot_vpc_theme()$obs_point$color, "#0000FF")
})

test_that("plot_vpc_theme returns all expected keys", {
  theme <- plot_vpc_theme()
  expected <- c("obs_point", "obs_median_line", "obs_pi_line",
                "sim_pi_line", "sim_pi_ci", "sim_pi_area",
                "sim_median_line", "sim_median_ci", "loq_line")
  expect_setequal(names(theme), expected)
})

test_that("plot_vpc_theme elements have correct classes", {
  theme <- plot_vpc_theme()
  expect_s3_class(theme$obs_point, "pmx_point")
  expect_s3_class(theme$obs_median_line, "pmx_line")
  expect_s3_class(theme$obs_pi_line, "pmx_line")
  expect_s3_class(theme$sim_pi_line, "pmx_line")
  expect_s3_class(theme$sim_pi_ci, "pmx_ribbon")
  expect_s3_class(theme$sim_pi_area, "pmx_ribbon")
  expect_s3_class(theme$sim_median_line, "pmx_line")
  expect_s3_class(theme$sim_median_ci, "pmx_ribbon")
  expect_s3_class(theme$loq_line, "pmx_line")
})

#####plot_gof_shown####

test_that("plot_gof_shown returns all TRUE defaults", {
  s <- plot_gof_shown()
  expect_type(s, "list")
  expect_true(s$obs)
  expect_true(s$dv)
  expect_true(s$pred)
  expect_true(s$ipred)
})

test_that("plot_gof_shown partial override preserves other defaults", {
  s <- plot_gof_shown(pred = FALSE)
  expect_false(s$pred)
  expect_true(s$obs)
  expect_true(s$dv)
  expect_true(s$ipred)
})

test_that("plot_gof_shown returns expected keys", {
  s <- plot_gof_shown()
  expect_setequal(names(s), c("obs", "dv", "pred", "ipred"))
})

#####plot_gof_theme cent shortcut####

test_that("plot_gof_theme cent shortcut applies alpha to point and line", {
  theme <- plot_gof_theme(cent = pmx_style(alpha = 0.5))
  expect_equal(theme$cent_point$alpha, 0.5)
  expect_equal(theme$cent_line$alpha, 0.5)
})

#####plot_vpc_shown####

test_that("plot_vpc_shown returns the documented defaults", {
  s <- plot_vpc_shown()
  expect_type(s, "list")
  expect_true(s$obs_point)
  expect_true(s$obs_pi_line)
  expect_true(s$obs_median_line)
  expect_false(s$sim_pi_line)
  expect_true(s$sim_pi_ci)
  expect_false(s$sim_pi_area)
  expect_false(s$sim_median_line)
  expect_true(s$sim_median_ci)
})

test_that("plot_vpc_shown partial override preserves other defaults", {
  s <- plot_vpc_shown(sim_pi_area = TRUE, sim_pi_ci = FALSE)
  expect_true(s$sim_pi_area)
  expect_false(s$sim_pi_ci)
  expect_true(s$obs_point)
  expect_true(s$obs_pi_line)
  expect_true(s$sim_median_ci)
})

test_that("plot_vpc_shown returns expected keys", {
  s <- plot_vpc_shown()
  expect_setequal(
    names(s),
    c("obs_point", "obs_pi_line", "obs_median_line",
      "sim_pi_line", "sim_pi_ci", "sim_pi_area",
      "sim_median_line", "sim_median_ci")
  )
})
