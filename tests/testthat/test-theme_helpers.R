#####theme helpers####

#####merge_element####

test_that("merge_element returns default when user is NULL", {
  default <- pmx_point(shape = 1, size = 2, alpha = 0.5)
  result <- pmxhelpr:::merge_element(NULL, default)
  expect_equal(result, default)
})

test_that("merge_element overrides specified fields", {
  default <- pmx_point(shape = 1, size = 2, alpha = 0.5)
  result <- pmxhelpr:::merge_element(pmx_point(size = 5), default)
  expect_equal(result$size, 5)
  expect_equal(result$shape, 1)
  expect_equal(result$alpha, 0.5)
})

test_that("merge_element preserves class of default", {
  default <- pmx_line(linewidth = 1, linetype = 2, alpha = 1)
  result <- pmxhelpr:::merge_element(pmx_line(alpha = 0.5), default)
  expect_s3_class(result, "pmx_line")
})

test_that("merge_element warns on invalid field", {
  default <- pmx_point(shape = 1, size = 2, alpha = 0.5)
  expect_warning(
    pmxhelpr:::merge_element(list(bogus = 99), default),
    regexp = "bogus.*not a valid field"
  )
})

#####merge_theme####

test_that("merge_theme returns default when user is NULL", {
  defaults <- plot_dvtime_theme()
  result <- pmxhelpr:::merge_theme(NULL, defaults)
  expect_equal(result, defaults)
})

test_that("merge_theme merges valid group overrides", {
  defaults <- plot_dvtime_theme()
  user <- list(ref_line = pmx_line(linewidth = 99))
  result <- pmxhelpr:::merge_theme(user, defaults)
  expect_equal(result$ref_line$linewidth, 99)
  expect_equal(result$ref_line$linetype, 2)
})

test_that("merge_theme warns on invalid group name", {
  defaults <- plot_dvtime_theme()
  user <- list(nonexistent = pmx_line())
  expect_warning(
    pmxhelpr:::merge_theme(user, defaults),
    regexp = "nonexistent.*not a valid group"
  )
})

test_that("merge_theme routes pmx_style to apply_style", {
  defaults <- list(
    obs_point = pmx_point(shape = 1, size = 2, alpha = 0.5, color = "black"),
    obs_line  = pmx_line(linewidth = 1, linetype = 1, alpha = 0.5, color = "black")
  )
  user <- list(obs = pmx_style(color = "red"))
  result <- pmxhelpr:::merge_theme(user, defaults)
  expect_equal(result$obs_point$color, "red")
  expect_equal(result$obs_line$color, "red")
  # Non-color fields preserved
  expect_equal(result$obs_point$shape, 1)
  expect_equal(result$obs_line$linewidth, 1)
})

test_that("merge_theme explicit key beats pmx_style shortcut (shortcut last)", {
  defaults <- plot_dvtime_theme()
  user <- list(obs_point = pmx_point(color = "red"), obs = pmx_style(color = "blue"))
  result <- pmxhelpr:::merge_theme(user, defaults)
  expect_equal(result$obs_point$color, "red")
  expect_equal(result$obs_line$color, "blue")
})

test_that("merge_theme explicit key beats pmx_style shortcut (shortcut first)", {
  defaults <- plot_dvtime_theme()
  user <- list(obs = pmx_style(color = "blue"), obs_point = pmx_point(color = "red"))
  result <- pmxhelpr:::merge_theme(user, defaults)
  expect_equal(result$obs_point$color, "red")
  expect_equal(result$obs_line$color, "blue")
})

test_that("merge_theme errors on cross-family theme", {
  expect_error(
    pmxhelpr:::merge_theme(plot_vpc_theme(), plot_dvtime_theme()),
    regexp = "theme family mismatch"
  )
})

test_that("merge_theme allows base pmx_theme override with any family", {
  defaults <- plot_dvtime_theme()
  user <- pmx_theme(list(obs_point = pmx_point(size = 99)))
  result <- pmxhelpr:::merge_theme(user, defaults)
  expect_equal(result$obs_point$size, 99)
})

test_that("merge_theme applies pmx_color overrides in plot_gof_theme", {
  theme <- plot_gof_theme(cent_color = pmx_color(pred = "purple"))
  expect_equal(theme$cent_color$pred, "purple")
  # Other colors preserved
  expect_equal(theme$cent_color$dv, "blue")
  expect_equal(theme$cent_color$ipred, "green")
})

#####apply_style####

test_that("apply_style sets color on both _point and _line", {
  defaults <- list(
    obs_point = pmx_point(shape = 1, size = 2, alpha = 0.5, color = "grey"),
    obs_line  = pmx_line(linewidth = 1, linetype = 1, alpha = 0.5, color = "grey")
  )
  style <- pmx_style(color = "blue")
  result <- pmxhelpr:::apply_style(style, "obs", defaults)
  expect_equal(result$obs_point$color, "blue")
  expect_equal(result$obs_line$color, "blue")
})

test_that("apply_style sets alpha on both _point and _line", {
  defaults <- list(
    cent_point = pmx_point(shape = 16, size = 1.25, alpha = 1),
    cent_line  = pmx_line(linewidth = 0.75, linetype = 1, alpha = 1)
  )
  style <- pmx_style(alpha = 0.3)
  result <- pmxhelpr:::apply_style(style, "cent", defaults)
  expect_equal(result$cent_point$alpha, 0.3)
  expect_equal(result$cent_line$alpha, 0.3)
})

test_that("apply_style is no-op when prefix has no matching keys", {
  defaults <- list(
    obs = pmx_point(shape = 1, size = 2, alpha = 0.5),
    ref = pmx_line(linewidth = 0.5, linetype = 2, alpha = 1)
  )
  style <- pmx_style(color = "red")
  result <- pmxhelpr:::apply_style(style, "nonexistent", defaults)
  expect_equal(result, defaults)
})

test_that("apply_style preserves non-targeted elements", {
  defaults <- list(
    obs_point = pmx_point(shape = 1, size = 2, alpha = 0.5, color = "grey"),
    obs_line  = pmx_line(linewidth = 1, linetype = 1, alpha = 0.5, color = "grey"),
    ref_line  = pmx_line(linewidth = 0.5, linetype = 2, alpha = 1)
  )
  style <- pmx_style(color = "blue")
  result <- pmxhelpr:::apply_style(style, "obs", defaults)
  # ref_line unchanged
  expect_equal(result$ref_line$linewidth, 0.5)
  expect_equal(result$ref_line$linetype, 2)
})

#####errorbar_width####

test_that("errorbar_width returns user-supplied width when set", {
  theme <- list(cent_errorbar = list(width = 1.25))
  expect_equal(pmxhelpr:::errorbar_width(theme, data.frame(NTIME = c(0, 1))), 1.25)
})

test_that("errorbar_width derives 2.5% of max NTIME when width is NULL", {
  theme <- list(cent_errorbar = list(width = NULL))
  expect_equal(pmxhelpr:::errorbar_width(theme, data.frame(NTIME = c(0, 1, 8))), 0.2)
})

test_that("errorbar_width warns and returns NA on empty data with no override", {
  theme <- list(cent_errorbar = list(width = NULL))
  expect_warning(
    result <- pmxhelpr:::errorbar_width(theme, data.frame(NTIME = numeric(0))),
    regexp = "empty or all NA"
  )
  expect_equal(result, NA_real_)
})

test_that("errorbar_width warns and returns NA on all-NA NTIME with no override", {
  theme <- list(cent_errorbar = list(width = NULL))
  expect_warning(
    result <- pmxhelpr:::errorbar_width(theme, data.frame(NTIME = c(NA_real_, NA_real_))),
    regexp = "empty or all NA"
  )
  expect_equal(result, NA_real_)
})

test_that("errorbar_width warns and returns NA when NTIME column is missing", {
  theme <- list(cent_errorbar = list(width = NULL))
  expect_warning(
    result <- pmxhelpr:::errorbar_width(theme, data.frame(OTHER = 1)),
    regexp = "empty or all NA"
  )
  expect_equal(result, NA_real_)
})
