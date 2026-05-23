#####df_forest -- draws path#####

##Test Output
test_that("Output is a `forest_stats` / `pmx_stats` container (draws path)", {
  out <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  expect_s3_class(out, "forest_stats")
  expect_s3_class(out, "pmx_stats")
})

test_that("Output stats slot contains canonical columns", {
  out <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  expect_true(all(c("est", "lo", "hi", "ci_label", "y_label") %in% colnames(out$stats)))
  expect_true(all(c("metric", "cov_var", "cov_val") %in% colnames(out$stats)))
})

test_that("Output has one row per (metric x cov_name x cov_level) group", {
  out <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  n_metric <- length(unique(data_sad_pkforest$metric))
  n_cov    <- nrow(unique(data_sad_pkforest[, c("cov_var", "cov_val")]))
  expect_equal(nrow(out$stats), n_metric * n_cov)
})

test_that("Draws path quantiles match stats::quantile on the input draws", {
  out <- df_forest(data_sad_pkforest, replicate_var = "SIM", ci = 0.9)
  grp <- data_sad_pkforest[data_sad_pkforest$metric  == "AUCRATIO" &
                           data_sad_pkforest$cov_var == "WTBL" &
                           data_sad_pkforest$cov_val == "50 kg", ]
  expected_lo  <- unname(stats::quantile(grp$value, 0.05))
  expected_hi  <- unname(stats::quantile(grp$value, 0.95))
  expected_est <- stats::median(grp$value)
  row <- out$stats[out$stats$metric  == "AUCRATIO" &
                   out$stats$cov_var == "WTBL" &
                   out$stats$cov_val == "50 kg", ]
  expect_equal(row$est, expected_est)
  expect_equal(row$lo,  expected_lo)
  expect_equal(row$hi,  expected_hi)
})

test_that("`ci` argument controls quantile probs", {
  out_90 <- df_forest(data_sad_pkforest, replicate_var = "SIM", ci = 0.9)
  out_95 <- df_forest(data_sad_pkforest, replicate_var = "SIM", ci = 0.95)
  pick <- function(s) s[s$metric  == "AUCRATIO" &
                        s$cov_var == "WTBL" &
                        s$cov_val == "50 kg", ]
  row_90 <- pick(out_90$stats)
  row_95 <- pick(out_95$stats)
  expect_lt(row_95$lo, row_90$lo)
  expect_gt(row_95$hi, row_90$hi)
})

test_that("`statistic = 'mean'` produces the arithmetic mean as `est`", {
  out <- df_forest(data_sad_pkforest, replicate_var = "SIM", statistic = "mean")
  grp <- data_sad_pkforest[data_sad_pkforest$metric  == "AUCRATIO" &
                           data_sad_pkforest$cov_var == "WTBL" &
                           data_sad_pkforest$cov_val == "50 kg", ]
  row <- out$stats[out$stats$metric  == "AUCRATIO" &
                   out$stats$cov_var == "WTBL" &
                   out$stats$cov_val == "50 kg", ]
  expect_equal(row$est, mean(grp$value))
})

test_that("`statistic = 'geo_mean'` produces the geometric mean as `est`", {
  out <- df_forest(data_sad_pkforest, replicate_var = "SIM", statistic = "geo_mean")
  grp <- data_sad_pkforest[data_sad_pkforest$metric  == "AUCRATIO" &
                           data_sad_pkforest$cov_var == "WTBL" &
                           data_sad_pkforest$cov_val == "50 kg", ]
  row <- out$stats[out$stats$metric  == "AUCRATIO" &
                   out$stats$cov_var == "WTBL" &
                   out$stats$cov_val == "50 kg", ]
  expect_equal(row$est, exp(mean(log(grp$value))))
})


#####df_forest -- pre-summarized path#####

test_that("Pre-summarized path passes est/lo/hi through unchanged", {
  out <- df_forest(data_sad_pkforest_sum,
                   est_var = "P50", lo_var = "P05", hi_var = "P95")
  expect_s3_class(out, "forest_stats")
  expect_equal(out$stats$est, data_sad_pkforest_sum$P50)
  expect_equal(out$stats$lo,  data_sad_pkforest_sum$P05)
  expect_equal(out$stats$hi,  data_sad_pkforest_sum$P95)
})

test_that("Pre-summarized path config records est_var/lo_var/hi_var and NULL value_var", {
  out <- df_forest(data_sad_pkforest_sum,
                   est_var = "P50", lo_var = "P05", hi_var = "P95")
  expect_equal(out$config$est_var, "P50")
  expect_equal(out$config$lo_var,  "P05")
  expect_equal(out$config$hi_var,  "P95")
  expect_null(out$config$value_var)
  expect_null(out$config$replicate_var)
})

test_that("Draws path config records value_var/replicate_var and NULL est_var/lo_var/hi_var", {
  out <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  expect_equal(out$config$value_var,     "value")
  expect_equal(out$config$replicate_var, "SIM")
  expect_null(out$config$est_var)
  expect_null(out$config$lo_var)
  expect_null(out$config$hi_var)
})


#####ci_label / y_label formatting#####

test_that("`ci_label` is formatted as `est [lo, hi]` with `sigdigits`", {
  out <- df_forest(data_sad_pkforest_sum,
                   est_var = "P50", lo_var = "P05", hi_var = "P95",
                   sigdigits = 3)
  expect_match(out$stats$ci_label, "^.+ \\[.+, .+\\]$", all = TRUE)
})

test_that("`y_label` concatenates cov_name, cov_level, and ci_label", {
  out <- df_forest(data_sad_pkforest_sum,
                   est_var = "P50", lo_var = "P05", hi_var = "P95")
  row <- out$stats[out$stats$cov_var == "WTBL" &
                   out$stats$cov_val == "50 kg" &
                   out$stats$metric  == "AUCRATIO", ]
  expect_match(row$y_label, "^WTBL: 50 kg - ")
})

test_that("`sigdigits` controls numeric formatting of ci_label", {
  out_2 <- df_forest(data_sad_pkforest_sum,
                     est_var = "P50", lo_var = "P05", hi_var = "P95",
                     sigdigits = 2)
  out_4 <- df_forest(data_sad_pkforest_sum,
                     est_var = "P50", lo_var = "P05", hi_var = "P95",
                     sigdigits = 4)
  expect_false(identical(out_2$stats$ci_label, out_4$stats$ci_label))
})


#####df_forest -- argument handling#####

test_that("Error if `data` is not a data.frame", {
  expect_error(df_forest("not a data.frame", replicate_var = "SIM"),
               regexp = "argument `data` must be a `data.frame`")
})

test_that("Error if both draws-path and pre-summarized-path args are supplied", {
  expect_error(
    df_forest(data_sad_pkforest, replicate_var = "SIM",
              est_var = "P50", lo_var = "P05", hi_var = "P95"),
    regexp = "cannot be combined"
  )
})

test_that("Error if neither path is selected", {
  expect_error(df_forest(data_sad_pkforest),
               regexp = "must be supplied")
})

test_that("Error on pre-summarized path missing one of est_var/lo_var/hi_var", {
  expect_error(
    df_forest(data_sad_pkforest_sum, est_var = "P50", lo_var = "P05"),
    regexp = "Missing: hi_var"
  )
})

test_that("Error if `ci` is not numeric in (0, 1)", {
  expect_error(df_forest(data_sad_pkforest, replicate_var = "SIM", ci = 1.5),
               regexp = "argument `ci` must be a single numeric value between 0 and 1")
})

test_that("Error if `statistic` is not one of median/mean/geo_mean", {
  expect_error(df_forest(data_sad_pkforest, replicate_var = "SIM", statistic = "mode"),
               regexp = "argument `statistic` must be `median`, `mean`, or `geo_mean`")
})

test_that("Error if `sigdigits` is not coercible to an integer", {
  expect_error(df_forest(data_sad_pkforest, replicate_var = "SIM", sigdigits = "abc"),
               regexp = "argument `sigdigits` must be coercible to class `integer`")
})

test_that("Error if metric_var/cov_name_var/cov_level_var columns missing", {
  d <- data_sad_pkforest
  d$metric <- NULL
  expect_error(df_forest(d, replicate_var = "SIM"),
               regexp = "not found: 'metric'")
})

test_that("Error if value_var column missing on draws path", {
  d <- data_sad_pkforest
  d$value <- NULL
  expect_error(df_forest(d, replicate_var = "SIM"),
               regexp = "not found: 'value'")
})

test_that("Error if est_var/lo_var/hi_var columns missing on pre-summarized path", {
  d <- data_sad_pkforest_sum
  d$P50 <- NULL
  expect_error(df_forest(d, est_var = "P50", lo_var = "P05", hi_var = "P95"),
               regexp = "not found: 'P50'")
})


#####validate_forest_stats#####

test_that("validate_forest_stats passes a valid object invisibly", {
  out <- df_forest(data_sad_pkforest_sum,
                   est_var = "P50", lo_var = "P05", hi_var = "P95")
  expect_invisible(pmxhelpr:::validate_forest_stats(out))
})

test_that("validate_forest_stats errors on non-forest_stats input", {
  expect_error(pmxhelpr:::validate_forest_stats(list()),
               regexp = "must be a `forest_stats` object")
})

test_that("validate_forest_stats errors when canonical columns are missing", {
  out <- df_forest(data_sad_pkforest_sum,
                   est_var = "P50", lo_var = "P05", hi_var = "P95")
  out$stats$est <- NULL
  expect_error(pmxhelpr:::validate_forest_stats(out),
               regexp = "missing required columns")
})

test_that("validate_forest_stats errors when config keys are missing", {
  out <- df_forest(data_sad_pkforest_sum,
                   est_var = "P50", lo_var = "P05", hi_var = "P95")
  out$config$ci <- NULL
  expect_error(pmxhelpr:::validate_forest_stats(out),
               regexp = "missing required config keys")
})


#####is_forest_stats#####

test_that("is_forest_stats returns TRUE for a `forest_stats` and FALSE otherwise", {
  out <- df_forest(data_sad_pkforest_sum,
                   est_var = "P50", lo_var = "P05", hi_var = "P95")
  expect_true(is_forest_stats(out))
  expect_false(is_forest_stats(as.data.frame(out)))
  expect_false(is_forest_stats(list()))
})

test_that("is_forest_stats(strict = TRUE) runs validation and returns FALSE on a broken object", {
  out <- df_forest(data_sad_pkforest_sum,
                   est_var = "P50", lo_var = "P05", hi_var = "P95")
  out$stats$est <- NULL
  expect_true(is_forest_stats(out))                # class tag only
  expect_false(is_forest_stats(out, strict = TRUE)) # validation fails
})


#####print / summary methods#####

test_that("print.forest_stats produces output without erroring", {
  out <- df_forest(data_sad_pkforest_sum,
                   est_var = "P50", lo_var = "P05", hi_var = "P95")
  expect_output(print(out), "<forest_stats>")
})

test_that("summary.forest_stats produces output without erroring", {
  out <- df_forest(data_sad_pkforest_sum,
                   est_var = "P50", lo_var = "P05", hi_var = "P95")
  expect_output(summary(out), "<forest_stats>")
})


#####plot_forest_theme#####

test_that("plot_forest_theme() returns a pmx_theme with the expected elements", {
  th <- plot_forest_theme()
  expect_s3_class(th, "plot_forest_theme")
  expect_s3_class(th, "pmx_theme")
  expect_true(all(c("point", "errorbar", "ref_line", "ref_band") %in% names(th)))
  expect_s3_class(th$point,    "pmx_point")
  expect_s3_class(th$errorbar, "pmx_errorbar")
  expect_s3_class(th$ref_line, "pmx_line")
  expect_s3_class(th$ref_band, "pmx_ribbon")
})

test_that("plot_forest_theme() honors user element overrides", {
  th <- plot_forest_theme(point = pmx_point(shape = 18, size = 4))
  expect_equal(th$point$shape, 18)
  expect_equal(th$point$size,  4)
})


#####plot_build_forest -- output class#####

test_that("plot_build_forest() returns a ggplot object", {
  stats <- df_forest(data_sad_pkforest_sum,
                     est_var = "P50", lo_var = "P05", hi_var = "P95")
  expect_s3_class(plot_build_forest(stats), "ggplot")
})

test_that("plot_build_forest() rejects non-forest_stats input", {
  expect_error(plot_build_forest(data_sad_pkforest_sum),
               regexp = "must be a `forest_stats` object")
})

test_that("plot_build_forest() maps `est` to x and the y_label column to y", {
  stats <- df_forest(data_sad_pkforest_sum,
                     est_var = "P50", lo_var = "P05", hi_var = "P95")
  p <- plot_build_forest(stats)
  expect_equal(rlang::quo_name(p$mapping$x), "est")
  expect_equal(rlang::quo_name(p$mapping$y), "y_label")
})


#####plot_build_forest -- facets#####

test_that("plot_build_forest() facets by `metric_var`", {
  stats <- df_forest(data_sad_pkforest_sum,
                     est_var = "P50", lo_var = "P05", hi_var = "P95")
  p <- plot_build_forest(stats)
  facet_vars <- names(p$facet$params$facets)
  expect_equal(facet_vars, "metric")
})


#####plot_build_forest -- ref / ref_band#####

test_that("ref_band = NULL produces no rect layer; setting it adds one", {
  stats <- df_forest(data_sad_pkforest_sum,
                     est_var = "P50", lo_var = "P05", hi_var = "P95")
  has_rect <- function(p) {
    any(vapply(p$layers,
               function(L) inherits(L$geom, "GeomRect"),
               logical(1)))
  }
  p_off <- plot_build_forest(stats, ref_band = NULL)
  p_on  <- plot_build_forest(stats, ref_band = c(0.8, 1.25))
  expect_false(has_rect(p_off))
  expect_true(has_rect(p_on))
})

test_that("ref = NULL suppresses the vline; setting it adds one", {
  stats <- df_forest(data_sad_pkforest_sum,
                     est_var = "P50", lo_var = "P05", hi_var = "P95")
  has_vline <- function(p) {
    any(vapply(p$layers,
               function(L) inherits(L$geom, "GeomVline"),
               logical(1)))
  }
  expect_false(has_vline(plot_build_forest(stats, ref = NULL)))
  expect_true (has_vline(plot_build_forest(stats, ref = 1)))
})

test_that("Error if ref_band is not a length-2 numeric", {
  stats <- df_forest(data_sad_pkforest_sum,
                     est_var = "P50", lo_var = "P05", hi_var = "P95")
  expect_error(plot_build_forest(stats, ref_band = c(1, 2, 3)),
               regexp = "must be a length-2 numeric")
  expect_error(plot_build_forest(stats, ref_band = c(1.25, 0.8)),
               regexp = "must be ordered")
})

test_that("Error if ref is not numeric", {
  stats <- df_forest(data_sad_pkforest_sum,
                     est_var = "P50", lo_var = "P05", hi_var = "P95")
  expect_error(plot_build_forest(stats, ref = "one"),
               regexp = "argument `ref` must be class `numeric`")
})

test_that("Error if annotate_ci is not boolean", {
  stats <- df_forest(data_sad_pkforest_sum,
                     est_var = "P50", lo_var = "P05", hi_var = "P95")
  expect_error(plot_build_forest(stats, annotate_ci = "yes"),
               regexp = "argument `annotate_ci` must be `TRUE` or `FALSE`")
})


#####plot_build_forest -- annotate_ci toggle#####

test_that("annotate_ci = FALSE drops CI text from y-axis labels", {
  stats <- df_forest(data_sad_pkforest_sum,
                     est_var = "P50", lo_var = "P05", hi_var = "P95")
  p_on  <- plot_build_forest(stats, annotate_ci = TRUE)
  p_off <- plot_build_forest(stats, annotate_ci = FALSE)
  on_levels  <- levels(p_on$data$y_label)
  off_levels <- levels(p_off$data$y_plain)
  expect_true(any(grepl("\\[.*\\]", on_levels)))
  expect_false(any(grepl("\\[.*\\]", off_levels)))
  expect_true("ci_label" %in% colnames(stats$stats))
})


#####plot_build_forest -- y-axis ordering#####

test_that("Y axis factor levels are reversed input order (top row plots first)", {
  stats <- df_forest(data_sad_pkforest_sum,
                     est_var = "P50", lo_var = "P05", hi_var = "P95")
  p <- plot_build_forest(stats)
  expected <- rev(unique(stats$stats$y_label))
  expect_equal(levels(p$data$y_label), expected)
})


#####plot_forest -- dual-mode dispatch#####

test_that("plot_forest() raw-data path returns a ggplot", {
  expect_s3_class(plot_forest(data_sad_pkforest, replicate_var = "SIM"), "ggplot")
})

test_that("plot_forest() precomputed-stats path returns a ggplot", {
  stats <- df_forest(data_sad_pkforest_sum,
                     est_var = "P50", lo_var = "P05", hi_var = "P95")
  expect_s3_class(plot_forest(stats), "ggplot")
})

test_that("plot_forest() pre-summarized raw-data path returns a ggplot", {
  expect_s3_class(plot_forest(data_sad_pkforest_sum,
                              est_var = "P50", lo_var = "P05", hi_var = "P95"),
                  "ggplot")
})

test_that("plot_forest() aborts when pipeline args are passed on the precomputed path", {
  stats <- df_forest(data_sad_pkforest_sum,
                     est_var = "P50", lo_var = "P05", hi_var = "P95")
  expect_error(plot_forest(stats, ci = 0.95),
               regexp = "cannot accept pipeline arguments")
  expect_error(plot_forest(stats, replicate_var = "SIM"),
               regexp = "cannot accept pipeline arguments")
})

test_that("plot_forest() honors theme/ref/ref_band/annotate_ci on the precomputed path", {
  stats <- df_forest(data_sad_pkforest_sum,
                     est_var = "P50", lo_var = "P05", hi_var = "P95")
  p <- plot_forest(stats, theme = plot_forest_theme(point = pmx_point(shape = 18)),
                   ref = 0, ref_band = c(0.5, 2), annotate_ci = FALSE)
  expect_s3_class(p, "ggplot")
})
