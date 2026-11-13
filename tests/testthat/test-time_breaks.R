##### Tests for breaks_time() and the time-axis default #####

x_breaks <- function(p) {
  b <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$breaks
  as.numeric(stats::na.omit(b))
}

test_that("breaks_time() picks the smallest unit step giving at most n breaks", {
  expect_equal(breaks_time()(c(0, 24)), seq(0, 24, by = 4))
  expect_equal(breaks_time()(c(0, 72)), seq(0, 72, by = 12))
  expect_equal(breaks_time()(c(0, 168.4)), seq(0, 168, by = 24))
  expect_equal(breaks_time()(c(0, 2)), seq(0, 2, by = 0.5))
  expect_equal(breaks_time("days")(c(0, 28)), seq(0, 28, by = 7))
  expect_equal(breaks_time("weeks")(c(0, 52)), seq(0, 48, by = 8))
  expect_equal(breaks_time("months")(c(0, 24)), seq(0, 24, by = 6))
  expect_equal(breaks_time(n = 5)(c(0, 24)), seq(0, 24, by = 6))
})

test_that("breaks_time() uses multiples of the largest step for long spans", {
  b <- breaks_time()(c(0, 8760))
  expect_lte(length(b), 8)
  expect_equal(unique(diff(b)), 2 * 672)
})

test_that("breaks_time() handles pre-dose, degenerate, and non-finite limits", {
  expect_equal(breaks_time()(c(-1, 24)), seq(0, 24, by = 4))
  expect_equal(breaks_time()(c(-24, 24)), seq(-24, 24, by = 8))
  expect_equal(breaks_time()(c(5, 5)), 5)
  expect_equal(breaks_time()(c(NA, 0, 24)), seq(0, 24, by = 4))
  expect_equal(breaks_time()(c(NA_real_, NA_real_)), numeric(0))
})

test_that("breaks_time() accepts short unit names and rejects others", {
  expect_equal(breaks_time("h")(c(0, 168)), breaks_time("hours")(c(0, 168)))
  expect_equal(breaks_time("d")(c(0, 28)), breaks_time("days")(c(0, 28)))
  expect_equal(breaks_time("wk")(c(0, 52)), breaks_time("weeks")(c(0, 52)))
  expect_equal(breaks_time("mo")(c(0, 24)), breaks_time("months")(c(0, 24)))
  expect_error(breaks_time("fortnights"), class = "pmxhelpr_error_bad_arg")
  expect_error(breaks_time(c("hours", "days")), class = "pmxhelpr_error_bad_arg")
  expect_error(breaks_time(n = 1), class = "pmxhelpr_error_bad_arg")
  expect_error(breaks_time(n = 2.5), class = "pmxhelpr_error_bad_arg")
})

test_that("plot_dvtime() places x breaks on the time unit, unless the style sets them", {
  pk <- dplyr::filter(data_sad, CMT == 2)
  suppressWarnings({
    expect_equal(x_breaks(plot_dvtime(pk, dv_var = ODV)), seq(0, 168, by = 24))
    expect_equal(x_breaks(plot_dvtime(pk, dv_var = ODV, time_unit = "days")),
                 seq(0, 168, by = 28))
    expect_equal(x_breaks(plot_dvtime(pk, dv_var = ODV,
                                      style = style_dvtime(xbreaks = c(0, 100)))),
                 c(0, 100))
    ## Style limits: the breaks follow the limits.
    expect_equal(x_breaks(plot_dvtime(pk, dv_var = ODV,
                                      style = style_dvtime(xlims = c(0, 48)))),
                 seq(0, 48, by = 8))
    ## Style tick labels: the breaks still follow the time unit.
    expect_equal(x_breaks(plot_dvtime(pk, dv_var = ODV,
                                      style = style_dvtime(xtick_labels = function(x) paste0(x, "h")))),
                 seq(0, 168, by = 24))
  })
  expect_error(plot_dvtime(pk, dv_var = ODV, time_unit = "fortnights"),
               class = "pmxhelpr_error_bad_arg")
})

test_that("a style log x-axis keeps its own breaks", {
  p <- suppressWarnings(plot_dvtime(dplyr::filter(data_sad, CMT == 2, TIME > 0),
                                    dv_var = ODV, style = style_dvtime(logx = TRUE)))
  expect_false(isTRUE(all.equal(suppressWarnings(x_breaks(p)), seq(0, 168, by = 24))))
})

test_that("plot_gof() and the VPC plots place x breaks on the time unit", {
  expect_equal(suppressWarnings(x_breaks(plot_gof(data_sad_pkfit, dv_var = ODV))),
               seq(0, 168, by = 24))
  testsim <- sim_pk(5)
  vs <- df_vpcstats(testsim, loq = 1)
  expect_equal(x_breaks(plot_vpc_cont(testsim)), seq(0, 168, by = 24))
  expect_equal(x_breaks(plot_vpc_cont(vs, time_unit = "days")), seq(0, 168, by = 28))
  expect_equal(x_breaks(plot_vpc_cens(vs, time_unit = "days")), seq(0, 168, by = 28))
  expect_equal(x_breaks(plot_build_vpc(vs, time_unit = "d")), seq(0, 168, by = 28))
  expect_equal(x_breaks(ggplot2::autoplot(vs, time_unit = "days")), seq(0, 168, by = 28))
})

test_that("an x scale added by the user takes over without a replacement message", {
  pk <- dplyr::filter(data_sad, CMT == 2)
  user_scale <- ggplot2::scale_x_continuous(breaks = c(0, 100))
  plots <- list(
    plot_dvtime(pk, dv_var = ODV),
    plot_gof(data_sad_pkfit, dv_var = ODV),
    plot_vpc_cont(sim_pk(5))
  )
  for (p in plots) {
    expect_no_message(p2 <- p + user_scale)
    expect_equal(suppressWarnings(x_breaks(p2)), c(0, 100))
  }
})
