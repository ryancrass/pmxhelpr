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


#####df_forest -- config round-trip#####

test_that("Config records metric_value_var and replicate_var", {
  out <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  expect_equal(out$config$metric_value_var, "value")
  expect_equal(out$config$replicate_var,    "SIM")
})


#####ci_label / y_label formatting#####

test_that("`ci_label` is formatted as `est [lo, hi]` with `sigdigits`", {
  out <- df_forest(data_sad_pkforest, replicate_var = "SIM", sigdigits = 3)
  expect_match(out$stats$ci_label, "^.+ \\[.+, .+\\]$", all = TRUE)
})

test_that("`y_label` mirrors cov_level (no CI concat; CI lives in ci_label only)", {
  out <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  row <- out$stats[out$stats$cov_var == "WTBL" &
                   out$stats$cov_val == "50 kg" &
                   out$stats$metric  == "AUCRATIO", ]
  expect_equal(row$y_label, "50 kg")
  expect_false(grepl("\\[", row$y_label))
})

test_that("`sigdigits` controls numeric formatting of ci_label", {
  out_2 <- df_forest(data_sad_pkforest, replicate_var = "SIM", sigdigits = 2)
  out_4 <- df_forest(data_sad_pkforest, replicate_var = "SIM", sigdigits = 4)
  expect_false(identical(out_2$stats$ci_label, out_4$stats$ci_label))
})


#####df_forest -- argument handling#####

test_that("Error if `data` is not a data.frame", {
  expect_error(df_forest("not a data.frame", replicate_var = "SIM"),
               regexp = "argument `data` must be a `data.frame`")
})

test_that("Error if `replicate_var` is not supplied", {
  expect_error(df_forest(data_sad_pkforest),
               regexp = "argument `replicate_var` is required")
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

test_that("Error if metric_name_var/cov_name_var/cov_level_var columns missing", {
  d <- data_sad_pkforest
  d$metric <- NULL
  expect_error(df_forest(d, replicate_var = "SIM"),
               regexp = "not found: 'metric'")
})

test_that("Error if metric_value_var column missing on draws path", {
  d <- data_sad_pkforest
  d$value <- NULL
  expect_error(df_forest(d, replicate_var = "SIM"),
               regexp = "not found: 'value'")
})

#####validate_forest_stats#####

test_that("validate_forest_stats passes a valid object invisibly", {
  out <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  expect_invisible(pmxhelpr:::validate_forest_stats(out))
})

test_that("validate_forest_stats errors on non-forest_stats input", {
  expect_error(pmxhelpr:::validate_forest_stats(list()),
               regexp = "must be a `forest_stats` object")
})

test_that("validate_forest_stats errors when canonical columns are missing", {
  out <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  out$stats$est <- NULL
  expect_error(pmxhelpr:::validate_forest_stats(out),
               regexp = "missing required columns")
})

test_that("validate_forest_stats errors when config keys are missing", {
  out <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  out$config$ci <- NULL
  expect_error(pmxhelpr:::validate_forest_stats(out),
               regexp = "missing required config keys")
})


#####is_forest_stats#####

test_that("is_forest_stats returns TRUE for a `forest_stats` and FALSE otherwise", {
  out <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  expect_true(is_forest_stats(out))
  expect_false(is_forest_stats(as.data.frame(out)))
  expect_false(is_forest_stats(list()))
})

test_that("is_forest_stats(strict = TRUE) runs validation and returns FALSE on a broken object", {
  out <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  out$stats$est <- NULL
  expect_true(is_forest_stats(out))                # class tag only
  expect_false(is_forest_stats(out, strict = TRUE)) # validation fails
})


#####print / summary methods#####

test_that("print.forest_stats produces output without erroring", {
  out <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  expect_output(print(out), "<forest_stats>")
})

test_that("summary.forest_stats produces output without erroring", {
  out <- df_forest(data_sad_pkforest, replicate_var = "SIM")
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
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  expect_s3_class(plot_build_forest(stats, metric = "AUCRATIO"), "ggplot")
})

test_that("plot_build_forest() rejects non-forest_stats input", {
  expect_error(plot_build_forest(data_sad_pkforest),
               regexp = "must be a `forest_stats` object")
})

test_that("plot_build_forest() maps `est` to x and the y_label column to y", {
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  p <- plot_build_forest(stats, metric = "AUCRATIO")
  expect_equal(rlang::quo_name(p$mapping$x), "est")
  expect_equal(rlang::quo_name(p$mapping$y), "y_label")
})


#####plot_build_forest -- facets#####

test_that("plot_build_forest() facet_grid uses cov_name rows only (no metric facet)", {
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  p <- plot_build_forest(stats, metric = "AUCRATIO")
  expect_s3_class(p$facet, "FacetGrid")
  row_vars <- vapply(p$facet$params$rows, rlang::as_name, character(1))
  expect_equal(unname(row_vars), "cov_var")
  expect_length(p$facet$params$cols, 0L)
  expect_true(isTRUE(p$facet$params$space_free$y))
})

test_that("plot_build_forest() sets the title to the rendered metric value", {
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  p <- plot_build_forest(stats, metric = "AUCRATIO")
  expect_equal(p$labels$title, "AUCRATIO")
})


#####plot_build_forest -- ref / ref_band#####

test_that("ref_band = NULL produces no rect layer; setting it adds one", {
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  has_rect <- function(p) {
    any(vapply(p$layers,
               function(L) inherits(L$geom, "GeomRect"),
               logical(1)))
  }
  p_off <- plot_build_forest(stats, metric = "AUCRATIO", ref_band = NULL)
  p_on  <- plot_build_forest(stats, metric = "AUCRATIO", ref_band = c(0.8, 1.25))
  expect_false(has_rect(p_off))
  expect_true(has_rect(p_on))
})

test_that("ref = NULL suppresses the vline; setting it adds one", {
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  has_vline <- function(p) {
    any(vapply(p$layers,
               function(L) inherits(L$geom, "GeomVline"),
               logical(1)))
  }
  expect_false(has_vline(plot_build_forest(stats, metric = "AUCRATIO", ref = NULL)))
  expect_true (has_vline(plot_build_forest(stats, metric = "AUCRATIO", ref = 1)))
})

test_that("Error if ref_band is not a length-2 numeric", {
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  expect_error(plot_build_forest(stats, metric = "AUCRATIO", ref_band = c(1, 2, 3)),
               regexp = "must be a length-2 numeric")
  expect_error(plot_build_forest(stats, metric = "AUCRATIO", ref_band = c(1.25, 0.8)),
               regexp = "must be ordered")
})

test_that("Error if ref is not numeric", {
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  expect_error(plot_build_forest(stats, metric = "AUCRATIO", ref = "one"),
               regexp = "argument `ref` must be class `numeric`")
})

#####plot_build_forest -- right-side ci_label annotation#####

test_that("ci_label is rendered as a right-side GeomText layer; y-axis carries cov_level only", {
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  p <- plot_build_forest(stats, metric = "AUCRATIO")
  has_text <- any(vapply(p$layers,
                         function(L) inherits(L$geom, "GeomText"),
                         logical(1)))
  expect_true(has_text)
  # primary y-axis only carries cov_level values (no CI text)
  expect_false(any(grepl("\\[", levels(p$data$y_label))))
  expect_true("ci_label" %in% colnames(stats$stats))
})

test_that("plot_build_forest() applies coord_cartesian(clip = 'off') and an extended right plot.margin", {
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  p <- plot_build_forest(stats, metric = "AUCRATIO")
  expect_equal(p$coordinates$clip, "off")
  expect_true(as.numeric(p$theme$plot.margin)[2] > 20)
})


#####plot_build_forest -- y-axis ordering#####

test_that("Y axis factor levels sort numerically within numeric panels, rev-data-order within categorical", {
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  p <- plot_build_forest(stats, metric = "AUCRATIO")
  # Expected per-panel ordering, concatenated in panel order:
  # REF panel  → ["REF"]            (non-numeric, single row)
  # FOOD panel → ["Fed"]            (non-numeric, single row)
  # WTBL panel → ["50 kg", "90 kg"] (numeric ascending; 90 ends up on top)
  expect_equal(levels(p$data$y_label), c("REF", "Fed", "50 kg", "90 kg"))
  expect_false(any(grepl("\\[", levels(p$data$y_label))))
})

test_that("Numeric covariates with dispersal: ref value sorts in numerical order with non-REF rows", {
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM",
                     cov_level_ref = c(FOOD = "Fasted", WTBL = "70 kg"))
  p <- plot_build_forest(stats, metric = "AUCRATIO")
  wtbl_levels <- levels(p$data$y_label)
  wtbl_idx <- which(wtbl_levels %in% c("50 kg", "70 kg", "90 kg"))
  # Within WTBL: factor levels low→high = 50, 70, 90 → on the discrete y axis
  # 90 lands at the top, 70 in the middle, 50 at the bottom
  expect_equal(wtbl_levels[wtbl_idx], c("50 kg", "70 kg", "90 kg"))
})


#####plot_forest -- dual-mode dispatch#####

test_that("plot_forest() raw-data path returns a ggplot", {
  expect_s3_class(plot_forest(data_sad_pkforest, replicate_var = "SIM",
                              metric = "AUCRATIO"),
                  "ggplot")
})

test_that("plot_forest() precomputed-stats path returns a ggplot", {
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  expect_s3_class(plot_forest(stats, metric = "AUCRATIO"), "ggplot")
})

test_that("plot_forest() aborts when pipeline args are passed on the precomputed path", {
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  expect_error(plot_forest(stats, ci = 0.95),
               regexp = "cannot accept pipeline arguments")
  expect_error(plot_forest(stats, replicate_var = "SIM"),
               regexp = "cannot accept pipeline arguments")
})

test_that("plot_forest() honors theme/ref/ref_band on the precomputed path", {
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  p <- plot_forest(stats,
                   metric = "AUCRATIO",
                   theme = plot_forest_theme(point = pmx_point(shape = 18)),
                   ref = 0, ref_band = c(0.5, 2))
  expect_s3_class(p, "ggplot")
})


#####plot_build_forest -- metric arg behavior#####

test_that("plot_build_forest() aborts when metric = NULL and stats has multiple metrics", {
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  expect_error(plot_build_forest(stats),
               regexp = "argument `metric` is required")
})

test_that("plot_build_forest() picks the single metric automatically when metric = NULL", {
  stats <- df_forest(dplyr::filter(data_sad_pkforest, metric == "AUCRATIO"),
                     replicate_var = "SIM")
  p <- plot_build_forest(stats)
  expect_equal(p$labels$title, "AUCRATIO")
  expect_true(all(as.character(p$data$metric) == "AUCRATIO"))
})

test_that("plot_build_forest() filters stats to the named metric", {
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  p <- plot_build_forest(stats, metric = "CMAXRATIO")
  expect_true(all(as.character(p$data$metric) == "CMAXRATIO"))
  expect_equal(p$labels$title, "CMAXRATIO")
})

test_that("plot_build_forest() aborts when metric is not in stats", {
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  expect_error(plot_build_forest(stats, metric = "NOPE"),
               regexp = "not found in `metric`")
})

test_that("plot_build_forest() aborts on malformed metric arg", {
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  expect_error(plot_build_forest(stats, metric = c("AUCRATIO", "CMAXRATIO")),
               regexp = "must be a single non-empty character string")
  expect_error(plot_build_forest(stats, metric = NA_character_),
               regexp = "must be a single non-empty character string")
  expect_error(plot_build_forest(stats, metric = ""),
               regexp = "must be a single non-empty character string")
})


#####plot_forest_theme -- refined defaults#####

test_that("plot_forest_theme() default sizes are bumped (point 2.5, errorbar 0.7)", {
  th <- plot_forest_theme()
  expect_equal(th$point$size, 2.5)
  expect_equal(th$errorbar$linewidth, 0.7)
})


#####plot_build_forest -- forest_panel theme variant#####

test_that("plot_build_forest() applies the forest_panel variant (panel.ontop unset, strip outside)", {
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  p <- plot_build_forest(stats, metric = "AUCRATIO")
  # facet_grid rows + strip-on-side now provide the cov_name grouping; panel.ontop is no longer needed
  expect_false(isTRUE(p$theme$panel.ontop))
  expect_equal(p$theme$strip.placement, "outside")
  expect_s3_class(p$theme$strip.background, "element_blank")
})


#####df_forest -- cov_name_ref#####

test_that("cov_name_ref defaults to 'REF' and round-trips through config", {
  out <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  expect_equal(out$config$cov_name_ref, "REF")
})

test_that("cov_name_ref can be overridden", {
  out <- df_forest(data_sad_pkforest, replicate_var = "SIM",
                   cov_name_ref = "Baseline")
  expect_equal(out$config$cov_name_ref, "Baseline")
})

test_that("cov_name_ref = NULL stores NULL in config", {
  out <- df_forest(data_sad_pkforest, replicate_var = "SIM",
                   cov_name_ref = NULL)
  expect_null(out$config$cov_name_ref)
})

test_that("check_forest_args() rejects malformed cov_name_ref", {
  expect_error(df_forest(data_sad_pkforest, replicate_var = "SIM",
                         cov_name_ref = c("REF", "Baseline")),
               regexp = "must be a single non-empty character string")
  expect_error(df_forest(data_sad_pkforest, replicate_var = "SIM",
                         cov_name_ref = NA_character_),
               regexp = "must be a single non-empty character string")
  expect_error(df_forest(data_sad_pkforest, replicate_var = "SIM",
                         cov_name_ref = ""),
               regexp = "must be a single non-empty character string")
  expect_error(df_forest(data_sad_pkforest, replicate_var = "SIM",
                         cov_name_ref = 1L),
               regexp = "must be a single non-empty character string")
})


#####plot_build_forest -- REF sorts to top#####

test_that("REF row sorts to the top via cov_name factor relevel", {
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  p <- plot_build_forest(stats, metric = "AUCRATIO")
  expect_equal(levels(p$data$cov_var)[1], "REF")
})

test_that("cov_name_ref with no matching row is silent and preserves data order", {
  # data_sad_pkforest has cov_var levels c("REF", "FOOD", "WTBL"); "NOMATCH" hits none
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM", cov_name_ref = "NOMATCH")
  expect_silent(p <- plot_build_forest(stats, metric = "AUCRATIO"))
  expect_false("NOMATCH" %in% levels(p$data$cov_var))
  expect_setequal(levels(p$data$cov_var),
                  unique(as.character(stats$stats$cov_var[stats$stats$metric == "AUCRATIO"])))
})

test_that("cov_name_ref = NULL preserves data order with no REF-first sort", {
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM", cov_name_ref = NULL)
  p <- plot_build_forest(stats, metric = "AUCRATIO")
  expect_equal(levels(p$data$cov_var),
               unique(as.character(stats$stats$cov_var[stats$stats$metric == "AUCRATIO"])))
})


#####plot_forest -- cov_name_ref on pipeline path / precomputed path#####

test_that("plot_forest() forwards cov_name_ref on the raw-data path", {
  p <- plot_forest(data_sad_pkforest, replicate_var = "SIM",
                   cov_name_ref = "REF", metric = "AUCRATIO")
  expect_equal(levels(p$data$cov_var)[1], "REF")
})

test_that("plot_forest() aborts when cov_name_ref is passed on the precomputed path", {
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  expect_error(plot_forest(stats, cov_name_ref = "REF"),
               regexp = "cannot accept pipeline arguments")
})


#####df_forest + plot_build_forest -- cov_level_ref dispersal#####

test_that("df_forest() builds canonical cov_ref column from cov_level_ref and round-trips through config", {
  out <- df_forest(data_sad_pkforest, replicate_var = "SIM",
                   cov_level_ref = c(FOOD = "Fasted", WTBL = "70 kg"))
  expect_true("cov_ref" %in% colnames(out$stats))
  expect_equal(out$config$cov_level_ref, c(FOOD = "Fasted", WTBL = "70 kg"))
  # FOOD rows get "Fasted", WTBL rows get "70 kg", REF rows get NA
  food_ref <- unique(out$stats$cov_ref[out$stats$cov_var == "FOOD"])
  wtbl_ref <- unique(out$stats$cov_ref[out$stats$cov_var == "WTBL"])
  ref_ref  <- unique(out$stats$cov_ref[out$stats$cov_var == "REF"])
  expect_equal(food_ref, "Fasted")
  expect_equal(wtbl_ref, "70 kg")
  expect_true(all(is.na(ref_ref)))
})

test_that("df_forest() leaves cov_ref out of stats when cov_level_ref is NULL (default)", {
  out <- df_forest(data_sad_pkforest, replicate_var = "SIM")
  expect_false("cov_ref" %in% colnames(out$stats))
  expect_null(out$config$cov_level_ref)
})

test_that("df_forest() rejects malformed cov_level_ref", {
  # unnamed vector
  expect_error(df_forest(data_sad_pkforest, replicate_var = "SIM",
                         cov_level_ref = c("Fasted", "70 kg")),
               regexp = "must be a non-empty named vector")
  # empty named entry
  expect_error(df_forest(data_sad_pkforest, replicate_var = "SIM",
                         cov_level_ref = stats::setNames(c("Fasted", "70 kg"),
                                                      c("FOOD", ""))),
               regexp = "must be a non-empty named vector")
  # duplicate names
  expect_error(df_forest(data_sad_pkforest, replicate_var = "SIM",
                         cov_level_ref = c(FOOD = "Fasted", FOOD = "Other")),
               regexp = "duplicate names")
  # list (not atomic)
  expect_error(df_forest(data_sad_pkforest, replicate_var = "SIM",
                         cov_level_ref = list(FOOD = "Fasted")),
               regexp = "named atomic vector")
})

test_that("cov_level_ref names absent from data are ignored (no error); existing names take effect", {
  out <- df_forest(data_sad_pkforest, replicate_var = "SIM",
                   cov_level_ref = c(FOOD = "Fasted", SEX = "Male"))
  # SEX is not in cov_var anywhere; FOOD rows still receive "Fasted"
  food_ref <- unique(out$stats$cov_ref[out$stats$cov_var == "FOOD"])
  wtbl_ref <- unique(out$stats$cov_ref[out$stats$cov_var == "WTBL"])
  expect_equal(food_ref, "Fasted")
  # WTBL not named in cov_level_ref → NA → no dispersed REF in that panel
  expect_true(all(is.na(wtbl_ref)))
})

test_that("plot_build_forest() disperses REF into each non-REF cov_name panel", {
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM",
                     cov_level_ref = c(FOOD = "Fasted", WTBL = "70 kg"))
  p <- plot_build_forest(stats, metric = "AUCRATIO")
  # REF panel is gone
  expect_false("REF" %in% as.character(unique(p$data$cov_var)))
  # FOOD panel carries the dispersed REF as cov_val == "Fasted"
  food_levels <- as.character(p$data$cov_val[p$data$cov_var == "FOOD"])
  expect_true("Fasted" %in% food_levels)
  # WTBL panel carries the dispersed REF as cov_val == "70 kg"
  wtbl_levels <- as.character(p$data$cov_val[p$data$cov_var == "WTBL"])
  expect_true("70 kg" %in% wtbl_levels)
})

test_that("dispersed REF row inherits est/lo/hi from the original REF row", {
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM",
                     cov_level_ref = c(FOOD = "Fasted", WTBL = "70 kg"))
  ref_stats <- stats$stats[stats$stats$cov_var == "REF" &
                           stats$stats$metric  == "AUCRATIO", ]
  p <- plot_build_forest(stats, metric = "AUCRATIO")
  row_fasted <- p$data[p$data$cov_var == "FOOD" & p$data$cov_val == "Fasted", ]
  expect_equal(row_fasted$est, ref_stats$est)
  expect_equal(row_fasted$lo,  ref_stats$lo)
  expect_equal(row_fasted$hi,  ref_stats$hi)
})

test_that("dispersed REF row sorts to top of its panel via y-axis factor order", {
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM",
                     cov_level_ref = c(FOOD = "Fasted", WTBL = "70 kg"))
  p <- plot_build_forest(stats, metric = "AUCRATIO")
  y_levels <- levels(p$data$y_label)
  expect_true("Fasted" %in% y_levels)
  expect_true("Fed"    %in% y_levels)
  expect_gt(which(y_levels == "Fasted"), which(y_levels == "Fed"))
})

test_that("plot_build_forest() aborts when cov_level_ref set but no matching REF row exists for the metric", {
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM",
                     cov_level_ref = c(FOOD = "Fasted", WTBL = "70 kg"),
                     cov_name_ref = "NOMATCH")
  expect_error(plot_build_forest(stats, metric = "AUCRATIO"),
               regexp = "no rows with")
})

test_that("plot_build_forest() aborts when cov_name_ref is NULL but cov_level_ref is set", {
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM",
                     cov_level_ref = c(FOOD = "Fasted", WTBL = "70 kg"),
                     cov_name_ref = NULL)
  expect_error(plot_build_forest(stats, metric = "AUCRATIO"),
               regexp = "cannot disperse REF row when `cov_name_ref` is NULL")
})

test_that("plot_forest() forwards cov_level_ref on the raw-data path", {
  p <- plot_forest(data_sad_pkforest, replicate_var = "SIM",
                   cov_level_ref = c(FOOD = "Fasted", WTBL = "70 kg"),
                   metric = "AUCRATIO")
  expect_false("REF" %in% as.character(unique(p$data$cov_var)))
})

test_that("plot_forest() aborts when cov_level_ref is passed on the precomputed path", {
  stats <- df_forest(data_sad_pkforest, replicate_var = "SIM",
                     cov_level_ref = c(FOOD = "Fasted", WTBL = "70 kg"))
  expect_error(plot_forest(stats, cov_level_ref = c(FOOD = "Fasted")),
               regexp = "cannot accept pipeline arguments")
})


#####df_forest -- cov_ref column input mode#####

# Helper: attach a per-row cov_ref column to data_sad_pkforest based on cov_var
attach_cov_ref <- function(d, mapping = c(FOOD = "Fasted", WTBL = "70 kg")) {
  d$cov_ref <- unname(mapping[as.character(d$cov_var)])
  d
}

test_that("df_forest() draws path reads cov_ref from a column on `data`", {
  d <- attach_cov_ref(data_sad_pkforest)
  out <- df_forest(d, replicate_var = "SIM")
  expect_true("cov_ref" %in% colnames(out$stats))
  expect_null(out$config$cov_level_ref)
  food_ref <- unique(out$stats$cov_ref[out$stats$cov_var == "FOOD"])
  wtbl_ref <- unique(out$stats$cov_ref[out$stats$cov_var == "WTBL"])
  ref_ref  <- unique(out$stats$cov_ref[out$stats$cov_var == "REF"])
  expect_equal(food_ref, "Fasted")
  expect_equal(wtbl_ref, "70 kg")
  expect_true(all(is.na(ref_ref)))
})

test_that("cov_ref column dispersal produces same dispersed structure as cov_level_ref arg", {
  d <- attach_cov_ref(data_sad_pkforest)
  stats_col <- df_forest(d, replicate_var = "SIM")
  stats_arg <- df_forest(data_sad_pkforest, replicate_var = "SIM",
                         cov_level_ref = c(FOOD = "Fasted", WTBL = "70 kg"))
  p_col <- plot_build_forest(stats_col, metric = "AUCRATIO")
  p_arg <- plot_build_forest(stats_arg, metric = "AUCRATIO")
  # Same dispersed y-axis factor levels
  expect_equal(levels(p_col$data$y_label), levels(p_arg$data$y_label))
  # REF panel dropped, Fasted in FOOD panel, 70 kg in WTBL panel
  expect_false("REF" %in% as.character(unique(p_col$data$cov_var)))
  expect_true ("Fasted" %in% as.character(p_col$data$cov_val[p_col$data$cov_var == "FOOD"]))
  expect_true ("70 kg"  %in% as.character(p_col$data$cov_val[p_col$data$cov_var == "WTBL"]))
})

test_that("df_forest() messages and uses the column when both cov_ref and cov_level_ref are supplied", {
  d <- attach_cov_ref(data_sad_pkforest,
                      mapping = c(FOOD = "Fasted", WTBL = "70 kg"))
  expect_message(
    out <- df_forest(d, replicate_var = "SIM",
                     cov_level_ref = c(FOOD = "OVERRIDDEN", WTBL = "OVERRIDDEN")),
    regexp = "Inheriting per-row `cov_ref` from `cov_ref` column"
  )
  # Column values win; config records NULL (the arg was discarded)
  expect_null(out$config$cov_level_ref)
  expect_equal(unique(out$stats$cov_ref[out$stats$cov_var == "FOOD"]), "Fasted")
  expect_equal(unique(out$stats$cov_ref[out$stats$cov_var == "WTBL"]), "70 kg")
})

test_that("plot_build_forest() aborts when cov_ref column is present but cov_name_ref is NULL", {
  d <- attach_cov_ref(data_sad_pkforest)
  stats <- df_forest(d, replicate_var = "SIM", cov_name_ref = NULL)
  expect_error(plot_build_forest(stats, metric = "AUCRATIO"),
               regexp = "cannot disperse REF row when `cov_name_ref` is NULL")
})

test_that("non-character cov_ref column is coerced to character", {
  # Edge case: numeric cov_ref values that should be carried through as character
  d <- data_sad_pkforest
  d$cov_ref <- ifelse(d$cov_var == "WTBL", 70, NA_real_)
  out <- df_forest(d, replicate_var = "SIM")
  expect_type(out$stats$cov_ref, "character")
  expect_equal(unique(out$stats$cov_ref[out$stats$cov_var == "WTBL"]), "70")
})


