#####shown - layer visibility objects and their resolution####

vs <- df_vpcstats(sim_pk())

## The function named in an error's call.
cnd_fn <- function(expr) {
  cnd <- rlang::catch_cnd(expr, classes = "error")
  as.character(cnd$call[[1]])
}

#####constructors####

test_that("plot_vpc_shown() and plot_gof_shown() return classed objects with the defaults", {
  v <- plot_vpc_shown()
  expect_s3_class(v, c("vpc_shown", "pmx_shown"), exact = TRUE)
  expect_identical(unclass(v), list(
    obs_point = TRUE, obs_pi_line = TRUE, obs_median_line = TRUE,
    sim_pi_line = FALSE, sim_pi_ci = TRUE, sim_pi_area = FALSE,
    sim_median_line = FALSE, sim_median_ci = TRUE, loq_line = TRUE))
  g <- plot_gof_shown()
  expect_s3_class(g, c("gof_shown", "pmx_shown"), exact = TRUE)
  expect_identical(unclass(g), list(obs = TRUE, dv = TRUE, pred = TRUE, ipred = TRUE))
  expect_false(plot_gof_shown(pred = FALSE)$pred)
  expect_true(plot_vpc_shown(sim_pi_line = TRUE)$sim_pi_line)
})

test_that("the constructors reject values that are not TRUE/FALSE", {
  expect_error(plot_vpc_shown(obs_point = "no"), "`obs_point` must be `TRUE` or `FALSE`",
               class = "pmxhelpr_error_bad_arg")
  expect_equal(cnd_fn(plot_vpc_shown(obs_point = "no")), "plot_vpc_shown")
  expect_error(plot_gof_shown(dv = NA), class = "pmxhelpr_error_bad_arg")
  expect_equal(cnd_fn(plot_gof_shown(dv = NA)), "plot_gof_shown")
})

test_that("print.pmx_shown lists each key and value", {
  expect_snapshot({
    plot_vpc_shown(sim_pi_line = TRUE)
    plot_gof_shown(pred = FALSE)
  })
  expect_invisible(print(plot_gof_shown()))
})

#####resolve_shown####

test_that("resolve_shown merges NULL, partial lists, and named logical vectors", {
  def <- plot_gof_shown()
  expect_identical(pmxhelpr:::resolve_shown(NULL, def), def)
  out <- pmxhelpr:::resolve_shown(list(obs = FALSE), def)
  expect_s3_class(out, "gof_shown")
  expect_false(out$obs)
  expect_true(out$dv)
  expect_identical(pmxhelpr:::resolve_shown(c(dv = FALSE), def), plot_gof_shown(dv = FALSE))
  expect_identical(pmxhelpr:::resolve_shown(plot_gof_shown(ipred = FALSE), def),
                   plot_gof_shown(ipred = FALSE))
})

test_that("unknown keys warn once, list the valid keys, and are ignored", {
  def <- plot_vpc_shown()
  expect_warning(out <- pmxhelpr:::resolve_shown(list(obs_pont = FALSE, x = 1, obs_point = FALSE), def),
                 "unknown keys \"obs_pont\" and \"x\"; valid keys: \"obs_point\"",
                 class = "pmxhelpr_warning_ignored_arg")
  expect_false(out$obs_point)
  expect_named(out, names(def))
  expect_length(testthat::capture_warnings(
    pmxhelpr:::resolve_shown(list(a = TRUE, b = TRUE), def)), 1)
})

test_that("values that are not TRUE/FALSE abort and name the shown argument", {
  expect_error(plot_vpc_cont(vs, shown = list(obs_point = "yes")),
               "`shown` must hold `TRUE` or `FALSE`; key \"obs_point\" does not",
               class = "pmxhelpr_error_bad_arg")
  expect_error(plot_vpc_legend(shown = list(loq_line = NA)), class = "pmxhelpr_error_bad_arg")
  expect_error(plot_gof(data_sad_pkfit, dv_var = ODV, shown = list(dv = c(TRUE, FALSE))),
               class = "pmxhelpr_error_bad_arg")
  expect_error(plot_vpc_cont(vs, shown = list(FALSE)), "must be a named list",
               class = "pmxhelpr_error_bad_arg")
  expect_error(plot_vpc_legend(shown = "a"), "must be a named list",
               class = "pmxhelpr_error_bad_arg")
  expect_equal(cnd_fn(plot_vpc_legend(shown = "a")), "plot_vpc_legend")
  expect_equal(cnd_fn(plot_gof(data_sad_pkfit, dv_var = ODV, shown = list(dv = 1))), "plot_gof")
})

test_that("the other family's shown object aborts and names the right constructor", {
  expect_error(plot_vpc_cont(vs, shown = plot_gof_shown()),
               "`shown` is a <gof_shown> object; use `plot_vpc_shown\\(\\)`",
               class = "pmxhelpr_error_bad_arg")
  expect_error(plot_gof(data_sad_pkfit, dv_var = ODV, shown = plot_vpc_shown()),
               "`shown` is a <vpc_shown> object; use `plot_gof_shown\\(\\)`",
               class = "pmxhelpr_error_bad_arg")
})

test_that("a plain list still controls the built layers", {
  p_list <- plot_vpc_cont(vs, shown = list(obs_point = FALSE))
  p_obj <- plot_vpc_cont(vs, shown = plot_vpc_shown(obs_point = FALSE))
  expect_false(has_geom(p_list, "GeomPoint"))
  expect_equal(capture.output(layer_summary(p_list)), capture.output(layer_summary(p_obj)))
})
