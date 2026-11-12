#####conditions - classed cli conditions and call attribution####

d1 <- dplyr::filter(data_sad, CMT != 3)

## The function named in an error's call, or "" for none.
cnd_fn <- function(expr) {
  cnd <- rlang::catch_cnd(expr, classes = "error")
  if (is.call(cnd$call)) as.character(cnd$call[[1]]) else ""
}

#####wrappers####

test_that("pmx_abort sets the kind and base classes and the caller's call", {
  f <- function(x) pmxhelpr:::pmx_abort("{.arg x} is {x}.", "bad_arg")
  cnd <- rlang::catch_cnd(f(3))
  expect_s3_class(cnd, c("pmxhelpr_error_bad_arg", "pmxhelpr_error", "error"))
  expect_equal(conditionMessage(cnd), "`x` is 3.")
  expect_equal(as.character(cnd$call[[1]]), "f")
})

test_that("pmx_abort reports a call passed down from a helper", {
  helper <- function(call = rlang::caller_env()) pmxhelpr:::pmx_abort("boom", "bad_arg", call = call)
  outer <- function() helper()
  expect_equal(cnd_fn(outer()), "outer")
})

test_that("pmx_warn and pmx_inform set the kind and base classes", {
  f <- function(n) pmxhelpr:::pmx_warn("{n} value{?s}", "data")
  expect_warning(f(2), "2 values", class = "pmxhelpr_warning_data")
  expect_warning(f(1), class = "pmxhelpr_warning")
  g <- function() pmxhelpr:::pmx_inform("hi", "loq_inherit")
  expect_message(g(), "hi", class = "pmxhelpr_message_loq_inherit")
  expect_message(g(), class = "pmxhelpr_message")
})

test_that("user values with braces are interpolated, not parsed", {
  f <- function(v) pmxhelpr:::pmx_abort("Not found: {.val {v}}.", "missing_var")
  expect_error(f("{oops}"), "\\{oops\\}", class = "pmxhelpr_error_missing_var")
})

#####one trigger per kind####

test_that("each error kind is raised by a representative bad call", {
  fs <- df_forest(data_sad_pkforest)
  expect_error(plot_dvtime("x", dv_var = "ODV"), class = "pmxhelpr_error_bad_arg")
  expect_error(plot_dvtime(d1, dv_var = "NOPE"), class = "pmxhelpr_error_missing_var")
  expect_error(plot_build_vpc(list()), class = "pmxhelpr_error_bad_stats")
  expect_error(plot_forest(fs, metric = "AUCRATIO", ci = 0.95),
               class = "pmxhelpr_error_pipeline_args")
  expect_error(model_mread_load("nope"), class = "pmxhelpr_error_model")
  expect_error(plot_dvtime(d1, dv_var = "ODV", style = list()),
               class = "pmxhelpr_error_bad_style")
  na_cov <- dplyr::mutate(data_sad_pkforest, cov_name = NA_character_)
  expect_error(df_forest(na_cov), class = "pmxhelpr_error_bad_data")
  ## every pmxhelpr error carries the base class
  expect_error(plot_dvtime(d1, dv_var = "NOPE"), class = "pmxhelpr_error")
})

test_that("each warning and message kind is raised by a representative call", {
  expect_warning(var_dosenorm(1, 0), class = "pmxhelpr_warning_data")
  expect_warning(plot_dvtime(d1, dv_var = "ODV", dose_var = "DOSE"),
                 class = "pmxhelpr_warning_ignored_arg")
  expect_warning(df_mrgsim_addpred(dplyr::mutate(d1, PRED = 1), model_mread_load("pkmodel")),
                 class = "pmxhelpr_warning_overwrite")
  withr::local_options(lifecycle_verbosity = "warning")
  expect_warning(plot_vpc_legend(lloq = 1), class = "lifecycle_warning_deprecated")
  expect_warning(plot_vpc_cont(sim_pk()) + ggplot2::facet_wrap(~FOOD),
                 class = "pmxhelpr_warning_vpc_facet")
  expect_message(df_vpcstats(sim_pk(carry_out = "LLOQ")),
                 class = "pmxhelpr_message_loq_inherit")
  expect_message(plot_dvtime(d1, dv_var = "ODV", loq = 1),
                 class = "pmxhelpr_message_ignored_arg")
})

#####call attribution####

test_that("errors raised in internal helpers name the exported function", {
  fs <- df_forest(data_sad_pkforest)
  vs <- df_vpcstats(sim_pk())
  expect_equal(cnd_fn(plot_dvtime(d1, dv_var = "ODV", dose_var = "nope", dosenorm = TRUE)),
               "plot_dvtime")
  expect_equal(cnd_fn(plot_gof(data_sad_pkfit, dv_var = "ODV", log_y = "a")), "plot_gof")
  expect_equal(cnd_fn(plot_vpc_cont(sim_pk(), pi = c(0.9, 0.1))), "df_vpcstats")
  expect_equal(cnd_fn(plot_vpc_cont(vs, pi = c(0.1, 0.9))), "plot_vpc_cont")
  expect_equal(cnd_fn(plot_build_vpc(list())), "plot_build_vpc")
  expect_equal(cnd_fn(df_forest(dplyr::rename(data_sad_pkforest, m = metric))), "df_forest")
  expect_equal(cnd_fn(plot_build_forest(fs, "AUCRATIO", ref_layout = "distributed")),
               "plot_build_forest")
  expect_equal(cnd_fn(plot_build_doseprop(data.frame(x = 1))), "plot_build_doseprop")
  expect_equal(cnd_fn(df_mrgsim_replicate(d1, model = list(), replicates = 2)),
               "df_mrgsim_replicate")
  expect_equal(cnd_fn(style_dvtime("red")), "style_dvtime")
})

test_that("error messages name the exported function and the user's argument", {
  fs <- df_forest(data_sad_pkforest)
  vs <- df_vpcstats(sim_pk())
  expect_snapshot(error = TRUE, {
    plot_dvtime(d1, dv_var = "ODV", dose_var = "nope", dosenorm = TRUE)
    plot_vpc_cont(sim_pk(), pi = c(0.9, 0.1))
    plot_vpc_cont(vs, pi = c(0.1, 0.9))
    plot_build_vpc(list())
    df_forest(dplyr::rename(data_sad_pkforest, m = metric))
    df_mrgsim_replicate(d1, model = list(), replicates = 2)
  })
})


#####deprecated arguments####

f_dep <- function(new = 1, old) {
  if (pmxhelpr:::use_deprecated_arg(!missing(old), !missing(new), "f_dep", "old", "new")) {
    new <- old
  }
  new
}

test_that("use_deprecated_arg warns through lifecycle and uses the old value", {
  withr::local_options(lifecycle_verbosity = "warning")
  expect_warning(out <- f_dep(old = 5), class = "lifecycle_warning_deprecated")
  expect_equal(out, 5)
  msg <- conditionMessage(rlang::catch_cnd(f_dep(old = 5), classes = "warning"))
  expect_match(msg, "The `old` argument of `f_dep()`", fixed = TRUE)
  expect_match(msg, "0.8.0", fixed = TRUE)
  expect_match(msg, "`new`", fixed = TRUE)
  expect_no_warning(expect_equal(f_dep(new = 2), 2))
  expect_no_warning(expect_equal(f_dep(), 1))
})

test_that("use_deprecated_arg aborts when both names are given", {
  expect_error(f_dep(new = 2, old = 5), "Pass `new` only; `old` is its deprecated name",
               class = "pmxhelpr_error_bad_arg")
  expect_equal(cnd_fn(f_dep(new = 2, old = 5)), "f_dep")
})

test_that("use_deprecated_arg leaves a bare-column argument unevaluated", {
  withr::local_options(lifecycle_verbosity = "warning")
  g <- function(data, new = "A", old) {
    q <- rlang::enquo(new)
    if (pmxhelpr:::use_deprecated_arg(!missing(old), !missing(new), "g", "old", "new")) {
      q <- rlang::enquo(old)
    }
    rlang::as_name(q)
  }
  expect_warning(out <- g(old = NOT_AN_OBJECT), class = "lifecycle_warning_deprecated")
  expect_equal(out, "NOT_AN_OBJECT")
})
