ref_map <- c(FOOD = "Fasted", WTBL = "70 kg")

# Rows a forest plot draws (distributed reference applied at plot time)
drawn <- function(stats, ...) pmxhelpr:::forest_rows(stats, ...)

# Layer helpers
layer_of <- function(p, cls, which_one = 1L) {
  idx <- which(vapply(p$layers, function(L) inherits(L$geom, cls), logical(1)))
  p$layers[[idx[which_one]]]
}
has_geom <- function(p, cls) {
  any(vapply(p$layers, function(L) inherits(L$geom, cls), logical(1)))
}
n_geom <- function(p, cls) {
  sum(vapply(p$layers, function(L) inherits(L$geom, cls), logical(1)))
}
scale_aes_of <- function(p) {
  vapply(p$scales$scales, function(s) s$aesthetics[1], character(1))
}
sec_axis_of <- function(p) p$scales$get_scales("y")$secondary.axis
# Per-panel axis labels in bottom-to-top order
panel_labels <- function(p, axis = c("y", "y.sec")) {
  axis <- match.arg(axis)
  built <- ggplot2::ggplot_build(p)
  lapply(built$layout$panel_params, function(pp) pp[[axis]]$get_labels())
}


#####df_forest -- aggregation#####

test_that("Output is a `forest_stats` / `pmx_stats` container", {
  out <- df_forest(data_sad_pkforest)
  expect_s3_class(out, "forest_stats")
  expect_s3_class(out, "pmx_stats")
  expect_null(out$obs)
})

test_that("Output stats slot contains canonical columns, grouping columns as character", {
  out <- df_forest(data_sad_pkforest)
  expect_equal(colnames(out$stats),
               c("metric", "cov_var", "cov_val", "est", "lo", "hi",
                 "est_label", "lo_label", "hi_label", "ci_label", "is_ref", "layout"))
  expect_type(out$stats$metric, "character")
  expect_type(out$stats$cov_var, "character")
  expect_type(out$stats$cov_val, "character")
  expect_type(out$stats$is_ref, "logical")
  expect_false("cov_ref" %in% colnames(out$stats))
})

test_that("Output has one row per (metric x cov_name x cov_level) group", {
  out <- df_forest(data_sad_pkforest)
  n_metric <- length(unique(data_sad_pkforest$metric))
  n_cov    <- nrow(unique(data_sad_pkforest[, c("cov_var", "cov_val")]))
  expect_equal(nrow(out$stats), n_metric * n_cov)
})

test_that("Quantiles match stats::quantile on the input draws", {
  out <- df_forest(data_sad_pkforest, ci = 0.9)
  grp <- data_sad_pkforest[data_sad_pkforest$metric  == "AUCRATIO" &
                           data_sad_pkforest$cov_var == "WTBL" &
                           data_sad_pkforest$cov_val == "50 kg", ]
  row <- out$stats[out$stats$metric  == "AUCRATIO" &
                   out$stats$cov_var == "WTBL" &
                   out$stats$cov_val == "50 kg", ]
  expect_equal(row$est, stats::median(grp$value))
  expect_equal(row$lo,  unname(stats::quantile(grp$value, 0.05)))
  expect_equal(row$hi,  unname(stats::quantile(grp$value, 0.95)))
})

test_that("`ci` argument controls quantile probs", {
  pick <- function(s) s[s$metric == "AUCRATIO" & s$cov_var == "WTBL" & s$cov_val == "50 kg", ]
  row_90 <- pick(df_forest(data_sad_pkforest, ci = 0.9)$stats)
  row_95 <- pick(df_forest(data_sad_pkforest, ci = 0.95)$stats)
  expect_lt(row_95$lo, row_90$lo)
  expect_gt(row_95$hi, row_90$hi)
})

test_that("`est` is the median of the draws", {
  grp <- data_sad_pkforest[data_sad_pkforest$metric  == "AUCRATIO" &
                           data_sad_pkforest$cov_var == "WTBL" &
                           data_sad_pkforest$cov_val == "50 kg", ]
  pick <- function(s) s[s$metric == "AUCRATIO" & s$cov_var == "WTBL" & s$cov_val == "50 kg", ]
  expect_equal(pick(df_forest(data_sad_pkforest)$stats)$est, stats::median(grp$value))
  expect_error(df_forest(data_sad_pkforest, statistic = "mean"), "unused argument")
})

test_that("Aggregation needs no replicate column", {
  d <- data_sad_pkforest
  d$SIM <- NULL
  expect_s3_class(df_forest(d), "forest_stats")
  expect_error(df_forest(data_sad_pkforest, irep_name = "SIM"), "unused argument")
})

test_that("Extra columns on `data` are not consumed", {
  d <- data_sad_pkforest
  d$extra <- "x"
  expect_equal(df_forest(d)$stats, df_forest(data_sad_pkforest)$stats)
  expect_false("extra" %in% colnames(df_forest(d)$stats))
})

test_that("Config records column names and settings", {
  out <- df_forest(data_sad_pkforest, cov_level_ref = ref_map)
  expect_equal(out$config$metric_name_var, "metric")
  expect_equal(out$config$cov_name_var, "cov_var")
  expect_equal(out$config$cov_level_var, "cov_val")
  expect_equal(out$config$metric_value_var, "value")
  expect_equal(out$config$ci, 0.9)
  expect_equal(out$config$sigdigits, 3)
  expect_equal(out$config$cov_name_ref, "Reference")
  expect_equal(out$config$cov_level_ref, ref_map)
})

test_that("Column arguments accept bare names", {
  out <- df_forest(data_sad_pkforest, metric_name_var = metric, cov_name_var = cov_var,
                   cov_level_var = cov_val, metric_value_var = value)
  expect_equal(out$stats, df_forest(data_sad_pkforest)$stats)
})


#####label formatting#####

test_that("labels share one decimal count per metric, set by `sigdigits` of its median estimate", {
  s <- df_forest(data_sad_pkforest, sigdigits = 3)$stats
  ratio <- s[s$metric == "AUCRATIO", ]
  expect_equal(ratio$ci_label[ratio$cov_var == "Reference"], "1.00 (1.00, 1.00)")
  # values below 1 take the metric's 2 decimals, not 3 significant digits
  expect_match(ratio$ci_label[ratio$cov_val == "90 kg"],
               "^0\\.[0-9]{2} \\(0\\.[0-9]{2}, 0\\.[0-9]{2}\\)$")
  expect_match(c(ratio$est_label, ratio$lo_label, ratio$hi_label), "^[0-9]\\.[0-9]{2}$", all = TRUE)
  # an exposure near 12500 rounds every value to the hundreds
  auc <- s[s$metric == "AUC", ]
  expect_match(c(auc$est_label, auc$lo_label, auc$hi_label), "^[0-9]+00$", all = TRUE)
  expect_equal(s$ci_label, paste0(s$est_label, " (", s$lo_label, ", ", s$hi_label, ")"))
  expect_false(identical(df_forest(data_sad_pkforest, sigdigits = 2)$stats$ci_label,
                         df_forest(data_sad_pkforest, sigdigits = 4)$stats$ci_label))
})


#####df_forest -- argument handling#####

test_that("Error if `data` is not a data.frame", {
  expect_error(df_forest("not a data.frame"),
               regexp = "argument `data` must be a `data.frame`")
})

test_that("Error on malformed ci / sigdigits", {
  expect_error(df_forest(data_sad_pkforest, ci = 1.5),
               regexp = "argument `ci` must be a single numeric value in \\(0, 1\\)")
  expect_error(df_forest(data_sad_pkforest, sigdigits = "abc"),
               regexp = "argument `sigdigits` must be coercible to class `integer`")
})

test_that("Error if a named column is missing", {
  d <- data_sad_pkforest
  d$metric <- NULL
  expect_error(df_forest(d), regexp = "not found: 'metric'")
  d <- data_sad_pkforest
  d$value <- NULL
  expect_error(df_forest(d), regexp = "not found: 'value'")
})

test_that("df_forest() rejects malformed cov_name_ref", {
  expect_error(df_forest(data_sad_pkforest, cov_name_ref = c("Reference", "Baseline")),
               regexp = "must be a single non-empty character string")
  expect_error(df_forest(data_sad_pkforest, cov_name_ref = NA_character_),
               regexp = "must be a single non-empty character string")
  expect_error(df_forest(data_sad_pkforest, cov_name_ref = ""),
               regexp = "must be a single non-empty character string")
  expect_error(df_forest(data_sad_pkforest, cov_name_ref = 1L),
               regexp = "must be a single non-empty character string")
})

test_that("df_forest() rejects malformed cov_level_ref", {
  expect_error(df_forest(data_sad_pkforest, cov_level_ref = c("Fasted", "70 kg")),
               regexp = "must be a non-empty named vector")
  expect_error(df_forest(data_sad_pkforest,
                         cov_level_ref = stats::setNames(c("Fasted", "70 kg"), c("FOOD", ""))),
               regexp = "must be a non-empty named vector")
  expect_error(df_forest(data_sad_pkforest, cov_level_ref = c(FOOD = "Fasted", FOOD = "Other")),
               regexp = "duplicate names")
  expect_error(df_forest(data_sad_pkforest, cov_level_ref = list(FOOD = "Fasted")),
               regexp = "named atomic vector")
})

test_that("cov_level_ref requires a non-NULL cov_name_ref and a matching row", {
  expect_error(df_forest(data_sad_pkforest, cov_level_ref = ref_map, cov_name_ref = NULL),
               regexp = "requires a non-NULL `cov_name_ref`")
  expect_error(df_forest(data_sad_pkforest, cov_level_ref = ref_map, cov_name_ref = "NOMATCH"),
               regexp = "no rows with")
})


#####df_forest -- reference rows#####

test_that("Standalone reference: is_ref flags the cov_name_ref rows and they sort first", {
  s <- df_forest(data_sad_pkforest)$stats
  expect_equal(s$is_ref, s$cov_var == "Reference")
  expect_equal(unique(s$cov_var), c("Reference", "FOOD", "WTBL"))
  s_base <- df_forest(data_sad_pkforest, cov_name_ref = "Baseline")$stats
  expect_false(any(s_base$is_ref))
})

test_that("cov_name_ref = NULL flags no reference and keeps data order", {
  s <- df_forest(data_sad_pkforest, cov_name_ref = NULL)$stats
  expect_false(any(s$is_ref))
  expect_equal(unique(s$cov_var), unique(data_sad_pkforest$cov_var))
})

test_that("cov_level_ref distributes the reference row into each named covariate at plot time", {
  s <- drawn(df_forest(data_sad_pkforest, cov_level_ref = ref_map))
  expect_false("Reference" %in% s$cov_var)
  auc <- s[s$metric == "AUCRATIO", ]
  expect_equal(auc$cov_val[auc$is_ref & auc$cov_var == "FOOD"], "Fasted")
  expect_equal(auc$cov_val[auc$is_ref & auc$cov_var == "WTBL"], "70 kg")
  # one distributed row per metric per named covariate
  expect_equal(sum(s$is_ref), 2L * length(unique(s$metric)))
})

test_that("Distributed reference rows inherit est/lo/hi and labels from the reference row", {
  plain <- df_forest(data_sad_pkforest)$stats
  ref <- plain[plain$cov_var == "Reference" & plain$metric == "AUCRATIO", ]
  s <- drawn(df_forest(data_sad_pkforest, cov_level_ref = ref_map))
  fasted <- s[s$metric == "AUCRATIO" & s$cov_var == "FOOD" & s$cov_val == "Fasted", ]
  expect_equal(fasted$est, ref$est)
  expect_equal(fasted$lo,  ref$lo)
  expect_equal(fasted$hi,  ref$hi)
  expect_equal(fasted[c("est_label", "lo_label", "hi_label", "ci_label")],
               ref[c("est_label", "lo_label", "hi_label", "ci_label")],
               ignore_attr = TRUE)
})

test_that("cov_level_ref names absent from data are ignored with a warning; unnamed covariates get no reference row", {
  expect_warning(
    out <- df_forest(data_sad_pkforest, cov_level_ref = c(FOOD = "Fasted", SEX = "Male")),
    "`cov_level_ref` names covariates not found in `cov_var`: SEX"
  )
  s <- drawn(out)
  expect_true(any(s$is_ref & s$cov_var == "FOOD"))
  expect_false(any(s$is_ref & s$cov_var == "WTBL"))
  expect_false("SEX" %in% s$cov_var)
})

test_that("a misspelled cov_level_ref name warns instead of silently falling back to consolidated", {
  expect_warning(
    out <- df_forest(data_sad_pkforest, cov_level_ref = c(FOD = "Fasted")),
    "not found in `cov_var`: FOD"
  )
  expect_false(any(out$stats$layout == "distributed"))
})

test_that("cov_level_ref matching every covariate does not warn", {
  expect_no_warning(df_forest(data_sad_pkforest, cov_level_ref = ref_map))
})


#####reference layout (ref_layout)#####

test_that("cov_level_ref adds distributed reference copies; other rows are unchanged", {
  plain <- df_forest(data_sad_pkforest)
  with_map <- df_forest(data_sad_pkforest, cov_level_ref = ref_map)
  expect_equal(unique(plain$stats$layout), c("consolidated", "both"))
  kept <- with_map$stats[with_map$stats$layout != "distributed", ]
  expect_equal(kept, plain$stats, ignore_attr = TRUE)
  copies <- with_map$stats[with_map$stats$layout == "distributed", ]
  expect_true(all(copies$is_ref))
  expect_equal(nrow(copies), 2 * length(unique(data_sad_pkforest$metric)))
  # copies sit in their panel at their sorted position
  auc <- with_map$stats[with_map$stats$metric == "AUCRATIO" & with_map$stats$cov_var == "WTBL", ]
  expect_equal(auc$cov_val, c("50 kg", "60 kg", "70 kg", "80 kg", "90 kg"))
})

test_that("ref_layout = NULL distributes only when cov_level_ref was given", {
  plain <- df_forest(data_sad_pkforest)
  with_map <- df_forest(data_sad_pkforest, cov_level_ref = ref_map)
  expect_equal(drawn(plain), plain$stats)
  expect_equal(drawn(with_map), drawn(with_map, "distributed"))
  expect_equal(drawn(with_map, "consolidated"), plain$stats)
})

test_that("Both layouts share the covariate rows and labels; only reference rows move", {
  stats <- df_forest(data_sad_pkforest, cov_level_ref = ref_map)
  dist <- drawn(stats, "distributed")
  cons <- drawn(stats, "consolidated")
  expect_equal(dist[!dist$is_ref, ], cons[!cons$is_ref, ], ignore_attr = TRUE)
  ref <- cons[cons$is_ref, c("metric", "est_label", "ci_label")]
  copies <- dist[dist$is_ref, c("metric", "est_label", "ci_label")]
  expect_equal(unique(copies), ref, ignore_attr = TRUE)
})

test_that("Covariate rows keep the order the reference label set, in both layouts", {
  d <- dplyr::filter(data_sad_pkforest, metric == "AUCRATIO")
  d$cov_val <- factor(d$cov_val, levels = c("Reference", "Fed", "Fasted",
                                            "50 kg", "70 kg", "90 kg", "60 kg", "80 kg"))
  stats <- df_forest(d, cov_level_ref = ref_map)
  cons <- drawn(stats, "consolidated")
  expect_equal(cons$cov_val[cons$cov_var == "WTBL"], c("50 kg", "90 kg", "60 kg", "80 kg"))
  dist <- drawn(stats, "distributed")
  expect_equal(dist$cov_val[dist$cov_var == "WTBL"], c("50 kg", "70 kg", "90 kg", "60 kg", "80 kg"))
  expect_equal(dist$cov_val[dist$cov_var == "FOOD"], c("Fed", "Fasted"))
})

test_that("ref_layout errors: distributed without distributed rows; bad values", {
  expect_error(plot_build_forest(df_forest(data_sad_pkforest), metric = "AUC",
                                 ref_layout = "distributed"),
               "pass `cov_level_ref` to `df_forest\\(\\)`")
  stats <- df_forest(data_sad_pkforest, cov_level_ref = ref_map)
  stats$stats <- stats$stats[stats$stats$layout != "distributed", ]
  expect_error(plot_build_forest(stats, metric = "AUC", ref_layout = "distributed"),
               "per-covariate reference rows")
  expect_error(plot_build_forest(stats, metric = "AUC", ref_layout = "split"), "ref_layout")
  expect_error(plot_build_forest(stats, metric = "AUC", ref_layout = NA_character_), "ref_layout")
  expect_error(plot_build_forest(stats, metric = "AUC", ref_layout = c("distributed", "consolidated")),
               "ref_layout")
})

test_that("ref_layout switches the drawn panels of one container", {
  stats <- df_forest(data_sad_pkforest, cov_level_ref = ref_map)
  p_dist <- plot_build_forest(stats, metric = "AUCRATIO")
  p_cons <- plot_build_forest(stats, metric = "AUCRATIO", ref_layout = "consolidated")
  expect_equal(levels(p_dist$data$cov_var), c("FOOD", "WTBL"))
  expect_equal(levels(p_cons$data$cov_var), c("Reference", "FOOD", "WTBL"))
  # the reference line resolves to the same estimate in both layouts
  vline <- function(p) layer_of(p, "GeomVline")$data$xintercept
  expect_equal(vline(p_dist), vline(p_cons))
})


#####df_forest -- row order#####

test_that("Numeric-leading labels sort ascending; the distributed reference lands in sequence", {
  s <- drawn(df_forest(data_sad_pkforest, cov_level_ref = ref_map))
  expect_equal(s$cov_val[s$metric == "AUCRATIO" & s$cov_var == "WTBL"],
               c("50 kg", "60 kg", "70 kg", "80 kg", "90 kg"))
  s0 <- df_forest(data_sad_pkforest)$stats
  expect_equal(s0$cov_val[s0$metric == "AUCRATIO"],
               c("Reference", "Fed", "50 kg", "60 kg", "80 kg", "90 kg"))
})

test_that("Negative-valued levels sort by signed value through df_forest()", {
  set.seed(1)
  d <- expand.grid(metric = "DELTA", cov_var = "EGFRCHG",
                   cov_val = c("10%", "-20%", "-5%", "0%"),
                   SIM = 1:20, stringsAsFactors = FALSE)
  d$value <- stats::rlnorm(nrow(d), 0, 0.2)
  expect_equal(df_forest(d, cov_name_ref = NULL)$stats$cov_val,
               c("-20%", "-5%", "0%", "10%"))
})

test_that("Categorical labels keep first-appearance order from `data`, reference label on top", {
  set.seed(1)
  d <- expand.grid(metric = "AUCRATIO", cov_var = "RENAL",
                   cov_val = c("Normal", "Mild", "Moderate", "Severe"),
                   SIM = 1:20, stringsAsFactors = FALSE)
  d$value <- stats::rlnorm(nrow(d), 0, 0.2)
  expect_equal(df_forest(d, cov_name_ref = NULL)$stats$cov_val,
               c("Normal", "Mild", "Moderate", "Severe"))
  s <- drawn(df_forest(data_sad_pkforest, cov_level_ref = ref_map))
  expect_equal(s$cov_val[s$metric == "AUCRATIO" & s$cov_var == "FOOD"], c("Fasted", "Fed"))
})

test_that("Labels sharing a leading number fall back to data order", {
  set.seed(1)
  d <- expand.grid(metric = "AUCRATIO", cov_var = "CYP2D6",
                   cov_val = c("CYP2D6 PM", "CYP2D6 IM", "CYP2D6 UM"),
                   SIM = 1:20, stringsAsFactors = FALSE)
  d$value <- stats::rlnorm(nrow(d), 0, 0.2)
  expect_equal(df_forest(d, cov_name_ref = NULL)$stats$cov_val,
               c("CYP2D6 PM", "CYP2D6 IM", "CYP2D6 UM"))
})

test_that("Range labels sharing a number keep data order, reference label on top", {
  set.seed(1)
  lab <- c("Mild (60-89)", "Moderate (30-59)", "Severe (<30)")
  d <- expand.grid(metric = "AUCRATIO", cov_var = c("Reference", "RENAL"),
                   cov_val = lab, SIM = 1:20, stringsAsFactors = FALSE)
  d$cov_val[d$cov_var == "Reference"] <- "Reference"
  d$value <- stats::rlnorm(nrow(d), 0, 0.2)
  s <- drawn(df_forest(d, cov_level_ref = c(RENAL = "Normal (>=90)")))
  expect_equal(s$cov_val[s$cov_var == "RENAL"], c("Normal (>=90)", lab))
})

test_that("Factor levels on cov_level_var control the row order, including the reference label", {
  d <- dplyr::filter(data_sad_pkforest, metric == "AUCRATIO")
  d$cov_val <- factor(d$cov_val, levels = c("Reference", "Fed", "Fasted",
                                            "50 kg", "70 kg", "90 kg", "60 kg", "80 kg"))
  s <- drawn(df_forest(d, cov_level_ref = ref_map))
  expect_equal(s$cov_val[s$cov_var == "FOOD"], c("Fed", "Fasted"))
  expect_equal(s$cov_val[s$cov_var == "WTBL"], c("50 kg", "70 kg", "90 kg", "60 kg", "80 kg"))
  # a label outside the factor levels falls back to the heuristics
  s2 <- drawn(df_forest(d, cov_level_ref = c(FOOD = "No food", WTBL = "70 kg")))
  expect_equal(s2$cov_val[s2$cov_var == "FOOD"], c("No food", "Fed"))
})

test_that("Factor levels on cov_name_var and metric_name_var control panel and metric order", {
  d <- data_sad_pkforest
  d$cov_var <- factor(d$cov_var, levels = c("WTBL", "FOOD", "Reference"))
  d$metric <- factor(d$metric, levels = rev(sort(unique(d$metric))))
  s <- df_forest(d, cov_name_ref = NULL)$stats
  expect_equal(unique(s$cov_var), c("WTBL", "FOOD", "Reference"))
  expect_equal(unique(s$metric), levels(d$metric))
  # the standalone reference still moves first
  s_ref <- df_forest(d)$stats
  expect_equal(unique(s_ref$cov_var), c("Reference", "WTBL", "FOOD"))
})


#####validate_forest_stats / is_forest_stats#####

test_that("validate_forest_stats passes a valid object invisibly", {
  expect_invisible(pmxhelpr:::validate_forest_stats(df_forest(data_sad_pkforest)))
})

test_that("validate_forest_stats errors on non-forest_stats input", {
  expect_error(pmxhelpr:::validate_forest_stats(list()),
               regexp = "must be a `forest_stats` object")
})

test_that("validate_forest_stats errors when canonical columns or config keys are missing", {
  out <- df_forest(data_sad_pkforest)
  out$stats$est <- NULL
  expect_error(pmxhelpr:::validate_forest_stats(out), regexp = "missing required columns")
  out <- df_forest(data_sad_pkforest)
  out$stats$is_ref <- NULL
  expect_error(pmxhelpr:::validate_forest_stats(out), regexp = "missing required columns")
  out <- df_forest(data_sad_pkforest)
  out$stats$layout <- NULL
  expect_error(pmxhelpr:::validate_forest_stats(out), regexp = "missing required columns: layout")
  out <- df_forest(data_sad_pkforest)
  out$config$ci <- NULL
  expect_error(pmxhelpr:::validate_forest_stats(out), regexp = "missing required keys")
})

test_that("validate_forest_stats does not require the provenance config keys", {
  out <- df_forest(data_sad_pkforest)
  out$config$metric_value_var <- NULL
  out$config$cov_name_ref <- NULL
  out$config$cov_level_ref <- NULL
  expect_invisible(pmxhelpr:::validate_forest_stats(out))
})

test_that("is_forest_stats returns TRUE for a `forest_stats` and FALSE otherwise", {
  out <- df_forest(data_sad_pkforest)
  expect_true(is_forest_stats(out))
  expect_false(is_forest_stats(as.data.frame(out)))
  expect_false(is_forest_stats(list()))
  out$stats$est <- NULL
  expect_true(is_forest_stats(out))
  expect_false(is_forest_stats(out, strict = TRUE))
})


#####print / summary methods#####

test_that("print and summary produce the forest_stats header", {
  out <- df_forest(data_sad_pkforest)
  expect_output(print(out), "<forest_stats>")
  expect_output(summary(out), "<forest_stats>")
  expect_output(summary(out), "per-row")
})

test_that("summary.forest_stats output is stable (snapshot)", {
  out <- df_forest(dplyr::filter(data_sad_pkforest, metric == "AUCRATIO"),
                   cov_level_ref = c(FOOD = "Fasted", WTBL = "70 kg"))
  expect_snapshot(summary(out))
})

test_that("forest header prints NULL config values and omits the obs line", {
  out <- df_forest(data_sad_pkforest, cov_name_ref = NULL)
  expect_output(print(out), "cov_name_ref = NULL, cov_level_ref = NULL")
  expect_false(any(grepl("obs:", capture.output(print(out)))))
})


#####style_forest#####

test_that("style_forest() returns a style_spec with the forest roles", {
  st <- style_forest()
  expect_s3_class(st, "ggstylekit_style_spec")
  expect_equal(names(st$colors), c("point", "errorbar", "ref_point", "ref_line"))
  expect_equal(names(st$fill), c("ref_point", "ref_band"))
  expect_true(all(c("point", "errorbar", "ref_point", "ref_line", "ref_band") %in%
                    names(st$alphas)))
  expect_equal(unname(st$linetypes["ref_line"]), "dashed")
  expect_equal(unname(st$shapes["ref_point"]), 21)
  expect_equal(unname(st$fill["ref_point"]), "white")
  expect_equal(unname(st$sizes["point"]), 2.2)
  expect_equal(unname(st$linewidths["errorbar"]), 0.6)
  expect_equal(st$legend.position, "none")
})

test_that("style_forest() merges per-series overrides entry-wise", {
  st <- style_forest(shapes = c(point = 18), sizes = c(point = 4))
  expect_equal(unname(st$shapes["point"]), 18)
  expect_equal(unname(st$sizes["point"]), 4)
  expect_equal(unname(st$colors["point"]), "black")
})

test_that("style_forest() colors carry no covariate entries by default; adding them keeps the roles", {
  expect_false(any(c("FOOD", "WTBL", "Reference") %in% names(style_forest()$colors)))
  st <- style_forest(colors = c(FOOD = "firebrick", WTBL = "steelblue", Reference = "grey20"))
  expect_equal(unname(st$colors["FOOD"]), "firebrick")
  expect_equal(unname(st$colors["ref_line"]), "grey30")
})

test_that("style_forest() theme uses the forest panel variant", {
  th <- style_forest()$theme
  expect_equal(th$strip.placement, "outside")
  expect_s3_class(th$strip.background, "element_blank")
  expect_s3_class(th$panel.grid.major.y, "element_blank")
  expect_s3_class(th$panel.grid.major.x, "element_line")
  # panels stack into one frame: no vertical spacing, grey60 shared borders
  expect_equal(as.numeric(th$panel.spacing.y), 0)
  expect_equal(th$panel.background$colour, "grey60")
  # estimate column text and header at full axis size; black y-axis text
  expect_null(th$axis.text.y.right$size)
  expect_equal(th$axis.text.y.right$colour, "black")
  expect_equal(as.numeric(th$axis.text.y.right$margin)[4], 8)
  expect_equal(th$axis.text.y.left$colour, "black")
  # covariate strips head their rows: top-left aligned
  expect_equal(th$strip.text.y.left$hjust, 0)
  expect_equal(th$strip.text.y.left$vjust, 1)
  expect_equal(unclass(th$plot.subtitle$size), 1)
})


#####plot_build_forest -- structure#####

test_that("plot_build_forest() returns a ggplot; rejects non-forest_stats input and bad style", {
  stats <- df_forest(data_sad_pkforest)
  expect_s3_class(plot_build_forest(stats, metric = "AUCRATIO"), "ggplot")
  expect_error(plot_build_forest(data_sad_pkforest), regexp = "must be a `forest_stats` object")
  expect_error(plot_build_forest(stats, metric = "AUCRATIO", style = list(point = 1)),
               regexp = "must be a `ggstylekit::style_spec\\(\\)` object")
})

test_that("plot_build_forest() maps `est` to x and the per-row key to y", {
  p <- plot_build_forest(df_forest(data_sad_pkforest), metric = "AUCRATIO")
  expect_equal(rlang::quo_name(p$mapping$x), "est")
  expect_equal(rlang::quo_name(p$mapping$y), ".row")
  expect_s3_class(p$layers[[1]]$geom, "GeomBlank")
})

test_that("plot_build_forest() facet_grid uses cov_name rows only; the x-axis label carries the metric", {
  p <- plot_build_forest(df_forest(data_sad_pkforest), metric = "AUCRATIO")
  expect_s3_class(p$facet, "FacetGrid")
  row_vars <- vapply(p$facet$params$rows, rlang::as_name, character(1))
  expect_equal(unname(row_vars), "cov_var")
  expect_length(p$facet$params$cols, 0L)
  expect_true(isTRUE(p$facet$params$space_free$y))
  expect_equal(p$labels$x, "AUCRATIO")
  expect_null(p$labels$y)
  expect_null(p$labels$title)
})

test_that("the style's xlabel and title fields override the metric x label and empty title", {
  p <- plot_build_forest(df_forest(data_sad_pkforest), metric = "AUCRATIO",
                         style = style_forest(xlabel = "AUC ratio", title = "Food and weight"))
  expect_equal(p$labels$x, "AUC ratio")
  expect_equal(p$labels$title, "Food and weight")
})

test_that("Rows draw top to bottom in stats order; panels follow row order", {
  stats <- df_forest(data_sad_pkforest, cov_level_ref = ref_map)
  p <- plot_build_forest(stats, metric = "AUCRATIO")
  s <- drawn(stats, metric = "AUCRATIO")
  expect_equal(p$data$cov_val, s$cov_val)
  expect_equal(levels(p$data$cov_var), c("FOOD", "WTBL"))
  # bottom-to-top per panel is the reverse of the stats order
  expect_equal(panel_labels(p, "y"),
               list(rev(s$cov_val[s$cov_var == "FOOD"]), rev(s$cov_val[s$cov_var == "WTBL"])))
  # reordering stats$stats reorders the plot
  stats$stats <- stats$stats[rev(seq_len(nrow(stats$stats))), ]
  p2 <- plot_build_forest(stats, metric = "AUCRATIO")
  expect_equal(levels(p2$data$cov_var), c("WTBL", "FOOD"))
  # the distributed reference row moves with the rows around it
  expect_equal(panel_labels(p2, "y")[[1]], c("50 kg", "60 kg", "70 kg", "80 kg", "90 kg"))
})

test_that("Covariate level labels may repeat across panels", {
  set.seed(1)
  draws <- expand.grid(METRIC = "AUC", COV = c("WTBL", "AGE"), COVLVL = c("Low", "High"),
                       REP = 1:50, stringsAsFactors = FALSE)
  draws$VALUE <- stats::rlnorm(nrow(draws), 0, 0.2)
  stats <- df_forest(draws, metric_name_var = "METRIC", cov_name_var = "COV",
                     cov_level_var = "COVLVL", metric_value_var = "VALUE", cov_name_ref = NULL)
  p <- plot_build_forest(stats, ref = 1)
  expect_equal(panel_labels(p, "y"), list(c("High", "Low"), c("High", "Low")))
  s <- stats$stats
  col <- paste0(s$est_label, " (", s$lo_label, ", ", s$hi_label, ")")
  expect_equal(panel_labels(p, "y.sec"),
               list(rev(col[s$COV == "WTBL"]), rev(col[s$COV == "AGE"])))
})


#####plot_build_forest -- estimate column (secondary y-axis)#####

test_that("the label pieces are the secondary y-axis text; reference rows show the estimate alone", {
  stats <- df_forest(data_sad_pkforest, cov_level_ref = ref_map)
  p <- plot_build_forest(stats, metric = "AUCRATIO")
  expect_false(has_geom(p, "GeomText"))
  expect_s3_class(sec_axis_of(p), "AxisSecondary")
  s <- drawn(stats, metric = "AUCRATIO")
  expected <- ifelse(s$is_ref, s$est_label,
                     paste0(s$est_label, " (", s$lo_label, ", ", s$hi_label, ")"))
  expect_equal(unlist(panel_labels(p, "y.sec")),
               c(rev(expected[s$cov_var == "FOOD"]), rev(expected[s$cov_var == "WTBL"])))
  expect_equal(expected[s$cov_val %in% c("Fasted", "70 kg")], c("1.00", "1.00"))
  expect_equal(expected[s$cov_val == "Fed"], "2.32 (2.00, 2.65)")
})

test_that("the estimate column header is the plot subtitle built from ci", {
  p <- plot_build_forest(df_forest(data_sad_pkforest), metric = "AUCRATIO")
  expect_equal(p$labels$subtitle, "Estimate (90% CI)")
  expect_null(p$labels$tag)
  expect_null(sec_axis_of(p)$name)
  stats_95 <- df_forest(data_sad_pkforest, ci = 0.95)
  expect_equal(plot_build_forest(stats_95, metric = "AUCRATIO")$labels$subtitle, "Estimate (95% CI)")
})


#####plot_build_forest -- point roles#####

test_that("reference rows draw through the ref_point role (open circle) and test rows through point", {
  stats <- df_forest(data_sad_pkforest, cov_level_ref = ref_map)
  p <- plot_build_forest(stats, metric = "AUCRATIO")
  expect_equal(n_geom(p, "GeomPoint"), 2L)
  pt <- layer_of(p, "GeomPoint", 1L)
  ref_pt <- layer_of(p, "GeomPoint", 2L)
  expect_equal(pt$aes_params$shape, 16)
  expect_equal(pt$aes_params$size, 2.2)
  expect_equal(pt$aes_params$colour, "black")
  expect_equal(ref_pt$aes_params$shape, 21)
  expect_equal(ref_pt$aes_params$fill, "white")
  expect_equal(ref_pt$aes_params$colour, "black")
  expect_true(all(ref_pt$data$is_ref))
  expect_setequal(ref_pt$data$cov_val, c("Fasted", "70 kg"))
  expect_false(any(pt$data$is_ref))
  expect_equal(layer_of(p, "GeomErrorbar")$aes_params$linewidth, 0.6)
})

test_that("a standalone Reference panel draws through ref_point; no ref_point layer without a reference", {
  p <- plot_build_forest(df_forest(data_sad_pkforest), metric = "AUCRATIO")
  expect_equal(n_geom(p, "GeomPoint"), 2L)
  expect_equal(as.character(layer_of(p, "GeomPoint", 2L)$data$cov_var), "Reference")
  expect_equal(panel_labels(p, "y.sec")[[1]], "1.00")
  # the tick label is blank where the level repeats the covariate name
  expect_equal(panel_labels(p, "y")[[1]], "")
  expect_equal(panel_labels(p, "y")[[2]], "Fed")
  p2 <- plot_build_forest(df_forest(data_sad_pkforest, cov_name_ref = NULL),
                          metric = "AUCRATIO", ref = 1)
  expect_equal(n_geom(p2, "GeomPoint"), 1L)
})


#####plot_build_forest -- ref / ref_band#####

test_that("ref_band = NULL produces no rect layer; setting it adds one styled from ref_band", {
  stats <- df_forest(data_sad_pkforest)
  expect_false(has_geom(plot_build_forest(stats, metric = "AUCRATIO", ref_band = NULL), "GeomRect"))
  p <- plot_build_forest(stats, metric = "AUCRATIO", ref_band = c(0.8, 1.25))
  expect_true(has_geom(p, "GeomRect"))
  rect <- layer_of(p, "GeomRect")
  expect_equal(rect$aes_params$alpha, 0.3)
  expect_equal(unique(ggplot2::ggplot_build(p)$data[[2]]$fill), "grey80")
  p2 <- plot_build_forest(stats, metric = "AUCRATIO", ref_band = c(0.8, 1.25),
                          style = style_forest(fill = c(ref_band = "skyblue"),
                                               alphas = c(ref_band = 0.2)))
  expect_equal(layer_of(p2, "GeomRect")$aes_params$alpha, 0.2)
  expect_equal(unique(ggplot2::ggplot_build(p2)$data[[2]]$fill), "skyblue")
})

test_that("ref = NULL suppresses the vline; a numeric ref places it; the default resolves to the reference row", {
  stats <- df_forest(data_sad_pkforest)
  expect_false(has_geom(plot_build_forest(stats, metric = "AUCRATIO", ref = NULL), "GeomVline"))
  p <- plot_build_forest(stats, metric = "AUCRATIO", ref = 0.5)
  expect_true(has_geom(p, "GeomVline"))
  expect_equal(layer_of(p, "GeomVline")$aes_params$linetype, "dashed")
  expect_equal(layer_of(p, "GeomVline")$data$xintercept, 0.5)
  # default `ref = "reference"`: 1 for the ratio, the reference exposure for AUC
  p_ratio <- plot_build_forest(stats, metric = "AUCRATIO")
  expect_equal(layer_of(p_ratio, "GeomVline")$data$xintercept, 1)
  ref_auc <- stats$stats$est[stats$stats$metric == "AUC" & stats$stats$is_ref]
  p_auc <- plot_build_forest(stats, metric = "AUC")
  expect_equal(layer_of(p_auc, "GeomVline")$data$xintercept, ref_auc)
  # distributed reference rows share one estimate and resolve the same way
  stats_d <- df_forest(data_sad_pkforest, cov_level_ref = ref_map)
  p_d <- plot_build_forest(stats_d, metric = "AUC")
  expect_equal(layer_of(p_d, "GeomVline")$data$xintercept, ref_auc)
})

test_that("ref = \"reference\" with no reference row warns and draws no line", {
  stats <- df_forest(data_sad_pkforest, cov_name_ref = NULL)
  expect_warning(p <- plot_build_forest(stats, metric = "AUCRATIO"),
                 regexp = "has no reference row")
  expect_false(has_geom(p, "GeomVline"))
  expect_error(
    suppressWarnings(plot_build_forest(stats, metric = "AUCRATIO", ref_band = c(0.8, 1.25))),
    regexp = "needs a reference value"
  )
  expect_silent(p2 <- plot_build_forest(stats, metric = "AUCRATIO", ref = NULL))
  expect_false(has_geom(p2, "GeomVline"))
})

test_that("ref_band_type places the band around the resolved ref", {
  stats <- df_forest(data_sad_pkforest)
  ref_auc <- stats$stats$est[stats$stats$metric == "AUC" & stats$stats$is_ref]
  band_of <- function(p) unlist(layer_of(p, "GeomRect")$data[, c("xmin", "xmax")], use.names = FALSE)
  # multiplicative (default): ratio scale is unchanged, raw scale scales with ref
  expect_equal(band_of(plot_build_forest(stats, metric = "AUCRATIO", ref_band = c(0.8, 1.25))),
               c(0.8, 1.25))
  expect_equal(band_of(plot_build_forest(stats, metric = "AUC", ref_band = c(0.8, 1.25))),
               ref_auc * c(0.8, 1.25))
  # additive
  expect_equal(band_of(plot_build_forest(stats, metric = "AUC", ref_band = c(-2000, 2000),
                                         ref_band_type = "additive")),
               ref_auc + c(-2000, 2000))
  # absolute, with and without a reference line
  expect_equal(band_of(plot_build_forest(stats, metric = "AUC", ref_band = c(5000, 15000),
                                         ref_band_type = "absolute")),
               c(5000, 15000))
  expect_equal(band_of(plot_build_forest(stats, metric = "AUC", ref = NULL, ref_band = c(5000, 15000),
                                         ref_band_type = "absolute")),
               c(5000, 15000))
  # a numeric ref anchors the band too
  expect_equal(band_of(plot_build_forest(stats, metric = "AUC", ref = 10000, ref_band = c(0.5, 2))),
               c(5000, 20000))
  expect_error(plot_build_forest(stats, metric = "AUC", ref = NULL, ref_band = c(0.8, 1.25)),
               regexp = "needs a reference value")
  expect_error(plot_build_forest(stats, metric = "AUC", ref_band = c(0.8, 1.25), ref_band_type = "nope"),
               regexp = "should be one of")
})

test_that("Error on malformed ref_band / ref", {
  stats <- df_forest(data_sad_pkforest)
  expect_error(plot_build_forest(stats, metric = "AUCRATIO", ref_band = c(1, 2, 3)),
               regexp = "must be a length-2 numeric")
  expect_error(plot_build_forest(stats, metric = "AUCRATIO", ref_band = c(1.25, 0.8)),
               regexp = "must be ordered")
  expect_error(plot_build_forest(stats, metric = "AUCRATIO", ref = "one"),
               regexp = "argument `ref` must be a single numeric value")
  expect_error(plot_build_forest(stats, metric = "AUCRATIO", ref = c(1, 2)),
               regexp = "argument `ref` must be a single numeric value")
  expect_error(plot_build_forest(stats, metric = "AUCRATIO", ref_band = c(-0.2, 1.25)),
               regexp = "must be positive when `ref_band_type = \"multiplicative\"`")
})

test_that("the reference line and band are unkeyed: no fill or linetype scale", {
  p <- plot_build_forest(df_forest(data_sad_pkforest), metric = "AUCRATIO",
                         ref_band = c(0.8, 1.25),
                         style = style_forest(fill = c(ref_band = "skyblue"),
                                              linetypes = c(ref_line = "dotted")))
  scale_aes <- scale_aes_of(p)
  expect_false(any(c("fill", "linetype") %in% scale_aes))
  expect_false("fill" %in% names(layer_of(p, "GeomRect")$mapping))
  expect_false("linetype" %in% names(layer_of(p, "GeomVline")$mapping))
  built <- ggplot2::ggplot_build(p)
  is_rect <- vapply(p$layers, function(L) inherits(L$geom, "GeomRect"), logical(1))
  is_vl <- vapply(p$layers, function(L) inherits(L$geom, "GeomVline"), logical(1))
  expect_equal(unique(built$data[[which(is_rect)]]$fill), "skyblue")
  expect_equal(unique(built$data[[which(is_vl)]]$linetype), "dotted")
  expect_error(plot_build_forest(df_forest(data_sad_pkforest), metric = "AUCRATIO",
                                 show_legend = TRUE), "unused argument")
})


#####plot_build_forest -- metric arg#####

test_that("metric picks the single value automatically, filters, and validates", {
  stats <- df_forest(data_sad_pkforest)
  expect_error(plot_build_forest(stats), regexp = "argument `metric` is required")
  p <- plot_build_forest(df_forest(dplyr::filter(data_sad_pkforest, metric == "AUCRATIO")))
  expect_true(all(p$data$metric == "AUCRATIO"))
  p <- plot_build_forest(stats, metric = "CMAXRATIO")
  expect_true(all(p$data$metric == "CMAXRATIO"))
  expect_error(plot_build_forest(stats, metric = "NOPE"), regexp = "not found in `metric`")
  expect_error(plot_build_forest(stats, metric = c("AUCRATIO", "CMAXRATIO")),
               regexp = "must be a single non-empty character string")
  expect_error(plot_build_forest(stats, metric = NA_character_),
               regexp = "must be a single non-empty character string")
  expect_error(plot_build_forest(stats, metric = ""),
               regexp = "must be a single non-empty character string")
})


#####plot_build_forest -- per-covariate coloring via style colors#####

test_that("no colour scale when the style names no covariate; fixed point colour is kept", {
  stats <- df_forest(data_sad_pkforest)
  p <- plot_build_forest(stats, metric = "AUCRATIO")
  expect_false("colour" %in% scale_aes_of(p))
  expect_null(p$guides$guides$colour)
  p2 <- plot_build_forest(stats, metric = "AUCRATIO", style = style_forest(colors = c(point = "navy")))
  pt <- layer_of(p2, "GeomPoint")
  expect_equal(pt$aes_params$colour, "navy")
  expect_false("colour" %in% names(pt$mapping))
})

test_that("plot_build_forest() maps color by covariate when the style names covariates", {
  stats <- df_forest(data_sad_pkforest, cov_level_ref = ref_map)
  p <- plot_build_forest(
    stats, metric = "AUCRATIO",
    style = style_forest(colors = c(Reference = "grey20", FOOD = "firebrick", WTBL = "steelblue"))
  )
  scale_aes <- scale_aes_of(p)
  expect_true("colour" %in% scale_aes)
  color_scale <- p$scales$scales[[which(scale_aes == "colour")]]
  expect_equal(color_scale$palette(2)[c("FOOD", "WTBL")],
               c(FOOD = "firebrick", WTBL = "steelblue"))
  expect_equal(p$guides$guides$colour, "none")
  pt <- layer_of(p, "GeomPoint", 1L)
  ref_pt <- layer_of(p, "GeomPoint", 2L)
  expect_true("colour" %in% names(pt$mapping))
  expect_null(pt$aes_params$colour)
  expect_true("colour" %in% names(ref_pt$mapping))
  expect_true("colour" %in% names(layer_of(p, "GeomErrorbar")$mapping))
  built <- ggplot2::ggplot_build(p)
  is_pt <- vapply(p$layers, function(L) inherits(L$geom, "GeomPoint"), logical(1))
  expect_setequal(unique(built$data[[which(is_pt)[1]]]$colour), c("firebrick", "steelblue"))
  expect_setequal(unique(built$data[[which(is_pt)[2]]]$colour), c("firebrick", "steelblue"))
})

test_that("plot_build_forest() colors a standalone Reference panel from the style", {
  p <- plot_build_forest(
    df_forest(data_sad_pkforest), metric = "AUCRATIO",
    style = style_forest(colors = c(Reference = "grey20", FOOD = "firebrick", WTBL = "steelblue"))
  )
  built <- ggplot2::ggplot_build(p)
  is_pt <- vapply(p$layers, function(L) inherits(L$geom, "GeomPoint"), logical(1))
  expect_setequal(unique(built$data[[which(is_pt)[1]]]$colour), c("firebrick", "steelblue"))
  expect_equal(unique(built$data[[which(is_pt)[2]]]$colour), "grey20")
})

test_that("Covariates without a style colour take the house palette, silently", {
  expect_no_warning(
    p <- plot_build_forest(df_forest(data_sad_pkforest), metric = "AUCRATIO",
                           style = style_forest(colors = c(FOOD = "red")))
  )
  built <- ggplot2::ggplot_build(p)
  is_pt <- vapply(p$layers, function(L) inherits(L$geom, "GeomPoint"), logical(1))
  test_cols <- built$data[[which(is_pt)[1]]]
  food <- p$layers[[which(is_pt)[1]]]$data$cov_var == "FOOD"
  expect_equal(unique(test_cols$colour[food]), "red")
  others <- unique(c(test_cols$colour[!food], built$data[[which(is_pt)[2]]]$colour))
  expect_length(others, 2L)
  expect_false(any(others %in% c("red", "grey50", NA)))
})


#####plot_build_forest -- forest panel theme variant#####

test_that("plot_build_forest() applies the forest_panel variant", {
  p <- plot_build_forest(df_forest(data_sad_pkforest), metric = "AUCRATIO")
  expect_false(isTRUE(p$theme$panel.ontop))
  expect_equal(p$theme$strip.placement, "outside")
  expect_s3_class(p$theme$strip.background, "element_blank")
  expect_equal(p$theme$axis.text.y.right$hjust, 0)
  expect_s3_class(p$theme$axis.ticks.y.right, "element_blank")
  # the header subtitle spans the plot, right-aligned over the column
  expect_equal(p$theme$plot.title.position, "plot")
  expect_equal(p$theme$plot.subtitle$hjust, 1)
  expect_equal(p$theme$plot.subtitle$face, "bold")
})


#####plot_forest -- dual-mode dispatch#####

test_that("plot_forest() raw-data path returns a ggplot and forwards the pipeline args", {
  p <- plot_forest(data_sad_pkforest, metric = "AUCRATIO", cov_level_ref = ref_map,
                   cov_name_var = cov_var)
  expect_s3_class(p, "ggplot")
  expect_false("Reference" %in% p$data$cov_var)
  expect_setequal(p$data$cov_val[p$data$is_ref], c("Fasted", "70 kg"))
  p2 <- plot_forest(data_sad_pkforest, metric = "AUCRATIO", cov_name_ref = NULL, ref = 1)
  expect_false(any(p2$data$is_ref))
})

test_that("plot_forest() precomputed-stats path returns a ggplot and honors plot args", {
  stats <- df_forest(data_sad_pkforest)
  expect_s3_class(plot_forest(stats, metric = "AUCRATIO"), "ggplot")
  p <- plot_forest(stats, metric = "AUCRATIO", style = style_forest(shapes = c(point = 18)),
                   ref = 0, ref_band = c(0.5, 2))
  expect_equal(layer_of(p, "GeomPoint")$aes_params$shape, 18)
  expect_true(has_geom(p, "GeomRect"))
})

test_that("plot_forest() forwards ref_layout on both paths", {
  stats <- df_forest(data_sad_pkforest, cov_level_ref = ref_map)
  p_pre <- plot_forest(stats, metric = "AUCRATIO", ref_layout = "consolidated")
  p_raw <- plot_forest(data_sad_pkforest, metric = "AUCRATIO", cov_level_ref = ref_map,
                       ref_layout = "consolidated")
  expect_equal(levels(p_pre$data$cov_var), c("Reference", "FOOD", "WTBL"))
  expect_equal(p_raw$data, p_pre$data)
  expect_equal(levels(plot_forest(stats, metric = "AUCRATIO")$data$cov_var), c("FOOD", "WTBL"))
})

test_that("plot_forest() forwards ref / ref_band_type on both paths", {
  stats <- df_forest(data_sad_pkforest)
  ref_auc <- stats$stats$est[stats$stats$metric == "AUC" & stats$stats$is_ref]
  p1 <- plot_forest(stats, metric = "AUC", ref_band = c(-1000, 1000), ref_band_type = "additive")
  p2 <- plot_forest(data_sad_pkforest, metric = "AUC", ref_band = c(-1000, 1000),
                    ref_band_type = "additive")
  for (p in list(p1, p2)) {
    expect_equal(layer_of(p, "GeomVline")$data$xintercept, ref_auc)
    expect_equal(unlist(layer_of(p, "GeomRect")$data[, c("xmin", "xmax")], use.names = FALSE),
                 ref_auc + c(-1000, 1000))
  }
})

test_that("plot_forest() aborts when pipeline args are passed on the precomputed path", {
  stats <- df_forest(data_sad_pkforest)
  expect_error(plot_forest(stats, ci = 0.95), regexp = "cannot accept pipeline arguments")
  expect_error(plot_forest(stats, cov_name_ref = "Reference"),
               regexp = "cannot accept pipeline arguments")
  expect_error(plot_forest(stats, cov_level_ref = ref_map),
               regexp = "cannot accept pipeline arguments")
})

#####df_forest -- custom reference name#####

renamed_ref <- function(cov_name = "Typical subject") {
  d <- data_sad_pkforest[data_sad_pkforest$metric == "AUCRATIO", ]
  is_ref <- d$cov_var == "Reference"
  d$cov_var[is_ref] <- cov_name
  d$cov_val[is_ref] <- cov_name
  d
}

test_that("`cov_name_ref` identifies a renamed reference, which sorts first", {
  out <- df_forest(renamed_ref(), cov_name_ref = "Typical subject")$stats
  expect_equal(out$cov_var[out$is_ref], "Typical subject")
  expect_equal(out$cov_var[1], "Typical subject")
  expect_false(any(df_forest(renamed_ref())$stats$is_ref))
})

test_that("`cov_level_ref` distributes a renamed reference", {
  s <- drawn(df_forest(renamed_ref(), cov_name_ref = "Typical subject",
                       cov_level_ref = ref_map))
  expect_equal(s$cov_val[s$cov_var == "FOOD"], c("Fasted", "Fed"))
  expect_equal(s$cov_val[s$cov_var == "WTBL"],
               c("50 kg", "60 kg", "70 kg", "80 kg", "90 kg"))
  expect_true(all(s$is_ref[s$cov_val %in% c("Fasted", "70 kg")]))
  expect_false("Typical subject" %in% s$cov_var)
})


#####missing values#####

test_that("df_forest() aborts on NA metric or covariate names", {
  d <- dplyr::filter(data_sad_pkforest, metric == "AUC")
  d$cov_var[1:5] <- NA
  expect_error(df_forest(d), "columns `cov_var` of `data` must not contain `NA`")
  d <- dplyr::filter(data_sad_pkforest, metric == "AUC")
  d$metric[1] <- NA
  expect_error(df_forest(d), "columns `metric` of `data` must not contain `NA`")
})

test_that("an all-NA group does not change the metric's decimal places", {
  d <- dplyr::filter(data_sad_pkforest, metric == "AUC")
  base <- df_forest(d)$stats
  d$value[d$cov_val == "90 kg"] <- NA
  out <- suppressWarnings(df_forest(d))$stats
  keep <- out$cov_val != "90 kg"
  expect_equal(out$est_label[keep], base$est_label[base$cov_val != "90 kg"])
  expect_true(is.na(out$est_label[!keep]))
  expect_true(is.na(out$ci_label[!keep]))
})

test_that("ref = \"reference\" aborts on distinct reference-row estimates", {
  stats <- df_forest(dplyr::filter(data_sad_pkforest, metric == "AUCRATIO"),
                     cov_level_ref = ref_map)
  copy <- which(stats$stats$layout == "distributed")[1]
  stats$stats$est[copy] <- 2
  expect_error(plot_build_forest(stats), "multiple distinct reference-row estimates")
  expect_s3_class(plot_build_forest(stats, ref = 1), "ggplot")
})

test_that("cov_level_ref naming the reference row itself warns and is ignored", {
  expect_warning(
    out <- df_forest(data_sad_pkforest, cov_level_ref = c(Reference = "x", FOOD = "Fasted")),
    "names the reference row itself \\(`Reference`\\)"
  )
  expect_false("x" %in% out$stats$cov_val)
})

test_that("cov_level_ref label matching an existing level warns", {
  expect_warning(
    out <- df_forest(data_sad_pkforest, cov_level_ref = c(WTBL = "50 kg")),
    "WTBL = \"50 kg\""
  )
  wt50 <- out$stats[out$stats$cov_var == "WTBL" & out$stats$cov_val == "50 kg" &
                      out$stats$metric == "AUC", ]
  expect_setequal(wt50$layout, c("both", "distributed"))
})

test_that("df_forest() column arguments follow the metric-first order", {
  expect_identical(names(formals(df_forest))[2:5],
                   c("metric_name_var", "metric_value_var", "cov_name_var", "cov_level_var"))
})


#####style_pmforest#####

test_that("style_pmforest() returns the pmforest look as a style_spec", {
  st <- style_pmforest()
  expect_s3_class(st, "ggstylekit_style_spec")
  expect_true(is.function(st$colors))
  expect_equal(st$colors(3), grDevices::hcl(h = c(15, 135, 255), c = 100, l = 65))
  expect_equal(unname(st$shapes[c("point", "ref_point")]), c(23, 23))
  expect_equal(unname(st$sizes["point"]), 3.5)
  expect_equal(unname(st$linetypes["ref_line"]), "dotdash")
  expect_equal(unname(st$fill[c("ref_point", "ref_band")]), c("white", "grey35"))
  expect_equal(unname(st$alphas["ref_band"]), 0.2)
  expect_equal(st$legend.position, "none")
  th <- st$theme
  expect_s3_class(ggplot2::calc_element("panel.grid.minor.x", th), "element_blank")
  expect_equal(th$panel.grid.major.x$colour, "grey")
  expect_equal(th$strip.placement, "outside")
  expect_equal(th$axis.text.y.right$size, 2.8 / 0.352777778)
})

test_that("style_pmforest() overrides: named colors replace the palette; maps merge entry-wise", {
  st <- style_pmforest(colors = c(FOOD = "red"), sizes = c(point = 3))
  expect_equal(st$colors, c(FOOD = "red"))
  expect_equal(unname(st$sizes[c("point", "ref_point")]), c(3, 3.5))
})

test_that("plot_build_forest() with style_pmforest() fills diamonds per covariate with black outlines", {
  stats <- df_forest(data_sad_pkforest)
  p <- plot_build_forest(stats, metric = "AUCRATIO", ref_band = c(0.8, 1.25),
                         style = style_pmforest())
  pt <- layer_of(p, "GeomPoint", 1L)
  expect_true("fill" %in% names(pt$mapping))
  expect_false("colour" %in% names(pt$mapping))
  expect_equal(pt$aes_params$colour, "black")
  expect_equal(p$guides$guides$fill, "none")
  built <- ggplot2::ggplot_build(p)
  is_pt <- vapply(p$layers, function(L) inherits(L$geom, "GeomPoint"), logical(1))
  is_eb <- vapply(p$layers, function(L) inherits(L$geom, "GeomErrorbar"), logical(1))
  pts <- built$data[[which(is_pt)[1]]]
  ebs <- built$data[[which(is_eb)]]
  expect_equal(unique(pts$shape), 23)
  expect_equal(pts$fill, ebs$colour)
  expect_equal(length(unique(pts$fill)), length(unique(p$layers[[which(is_pt)[1]]]$data$cov_var)))
  rect <- layer_of(p, "GeomRect")
  expect_equal(rect$aes_params$fill, "grey35")
  expect_equal(rect$aes_params$alpha, 0.2)
  vline <- built$data[[which(vapply(p$layers, function(L) inherits(L$geom, "GeomVline"), logical(1)))]]
  expect_equal(unique(vline$linetype), "dotdash")
})

test_that("a palette function in style_forest() colors each covariate", {
  p <- plot_build_forest(df_forest(data_sad_pkforest), metric = "AUCRATIO",
                         style = style_forest(colors = function(n) rep("red", n)))
  pt <- layer_of(p, "GeomPoint", 1L)
  expect_true("colour" %in% names(pt$mapping))
  built <- ggplot2::ggplot_build(p)
  is_pt <- vapply(p$layers, function(L) inherits(L$geom, "GeomPoint"), logical(1))
  expect_equal(unique(built$data[[which(is_pt)[1]]]$colour), "red")
})

test_that("covariate colors match across ref_layout; the Reference panel keeps the ref_point colour", {
  stats <- df_forest(data_sad_pkforest, cov_level_ref = ref_map)
  pt_cols <- function(style, layout) {
    p <- plot_build_forest(stats, metric = "AUCRATIO", style = style, ref_layout = layout)
    built <- ggplot2::ggplot_build(p)
    is_pt <- which(vapply(p$layers, function(L) inherits(L$geom, "GeomPoint"), logical(1)))
    lapply(is_pt, function(i) {
      cov <- as.character(p$layers[[i]]$data$cov_var)
      tapply(built$data[[i]]$colour, cov, unique)
    })
  }
  for (st in list(style_pumas(), style_forest(colors = c(FOOD = "red", WTBL = "blue")))) {
    dist <- pt_cols(st, "distributed")
    cons <- pt_cols(st, "consolidated")
    expect_equal(cons[[1]][c("FOOD", "WTBL")], dist[[1]][c("FOOD", "WTBL")])
    ref_col <- if (is.function(st$colors)) "black" else st$colors[["ref_point"]]
    expect_equal(unname(cons[[2]]["Reference"]), ref_col)
  }
  expect_equal(as.vector(pt_cols(style_pumas(), "distributed")[[1]][c("FOOD", "WTBL")]),
               style_pumas()$colors(2))
})

test_that("a palette function whose names match no covariate warns", {
  expect_warning(
    plot_build_forest(df_forest(data_sad_pkforest), metric = "AUCRATIO",
                      style = style_forest(colors = function(n) c(a = "red"))),
    "matching no covariate"
  )
})

test_that("a palette function returning fewer than n colors is recycled", {
  p <- plot_build_forest(df_forest(data_sad_pkforest), metric = "AUCRATIO",
                         style = style_forest(colors = function(n) "red"))
  built <- ggplot2::ggplot_build(p)
  is_pt <- vapply(p$layers, function(L) inherits(L$geom, "GeomPoint"), logical(1))
  cols <- unlist(lapply(built$data[is_pt], `[[`, "colour"))
  expect_false(anyNA(cols))
  expect_equal(unique(built$data[[which(is_pt)[1]]]$colour), "red")
})


#####style_coveffectsplot#####

test_that("style_coveffectsplot() returns the coveffectsplot look as a style_spec", {
  st <- style_coveffectsplot()
  expect_s3_class(st, "ggstylekit_style_spec")
  expect_equal(unname(st$shapes[c("point", "ref_point")]), c(16, 16))
  expect_equal(unname(st$sizes[c("point", "ref_point")]), c(4, 4))
  expect_equal(unname(st$colors[c("point", "errorbar", "ref_point", "ref_line")]),
               c("blue", "blue", "blue", "black"))
  expect_equal(unname(st$linetypes["ref_line"]), "dashed")
  expect_equal(unname(st$linewidths[c("errorbar", "ref_line")]), c(1, 1))
  expect_equal(unname(st$fill["ref_band"]), "grey")
  expect_equal(unname(st$alphas["ref_band"]), 80 / 255)
  expect_equal(st$legend.position, "none")
  th <- st$theme
  expect_equal(th$strip.placement, "inside")
  expect_equal(th$strip.background$fill, "#E5E5E5")
  expect_equal(th$strip.text.y.left$size, 8)
  expect_s3_class(th$panel.grid.major.y, "element_line")
  expect_s3_class(ggplot2::calc_element("panel.grid.minor.x", th), "element_blank")
  expect_equal(as.numeric(th$panel.spacing.y), 5.5)
  expect_equal(th$axis.text.y.right$hjust, 0.5)
})

test_that("plot_build_forest() with style_coveffectsplot() draws every row blue, unmapped", {
  p <- plot_build_forest(df_forest(data_sad_pkforest), metric = "AUCRATIO",
                         ref_band = c(0.8, 1.25), style = style_coveffectsplot())
  expect_false("colour" %in% scale_aes_of(p))
  built <- ggplot2::ggplot_build(p)
  is_pt <- vapply(p$layers, function(L) inherits(L$geom, "GeomPoint"), logical(1))
  expect_equal(unique(unlist(lapply(built$data[is_pt], `[[`, "colour"))), "blue")
  expect_equal(layer_of(p, "GeomRect")$aes_params$fill, "grey")
})


#####style_pmxforest#####

test_that("style_pmxforest() returns the PMXForest look as a style_spec", {
  st <- style_pmxforest()
  expect_s3_class(st, "ggstylekit_style_spec")
  expect_equal(unname(st$shapes[c("point", "ref_point")]), c(16, 16))
  expect_equal(unname(st$sizes[c("point", "ref_point")]), c(2.5, 2.5))
  expect_equal(unname(st$colors[c("point", "errorbar", "ref_point", "ref_line")]),
               rep("black", 4))
  expect_equal(unname(st$linetypes[c("errorbar", "ref_line")]), c("solid", "dotted"))
  expect_equal(unname(st$linewidths[c("errorbar", "ref_line")]), c(0.7, 1))
  expect_equal(unname(st$fill["ref_band"]), "lightgrey")
  expect_equal(unname(st$alphas["ref_band"]), 0.7)
  expect_equal(st$legend.position, "none")
  th <- st$theme
  expect_equal(th$panel.background$fill, "grey92")
  expect_s3_class(th$panel.border, "element_blank")
  expect_equal(th$panel.grid.major.y$colour, "white")
  expect_s3_class(ggplot2::calc_element("panel.grid.minor.x", th), "element_blank")
  expect_equal(th$strip.background$fill, "grey85")
  expect_equal(th$strip.placement, "inside")
  expect_equal(th$strip.text.y.left$face, "plain")
})

test_that("plot_build_forest() with style_pmxforest() draws black, dotted, unmapped", {
  p <- plot_build_forest(df_forest(data_sad_pkforest), metric = "AUCRATIO",
                         ref_band = c(0.8, 1.25), style = style_pmxforest())
  expect_false("colour" %in% scale_aes_of(p))
  built <- ggplot2::ggplot_build(p)
  is_pt <- vapply(p$layers, function(L) inherits(L$geom, "GeomPoint"), logical(1))
  expect_equal(unique(unlist(lapply(built$data[is_pt], `[[`, "colour"))), "black")
  is_vl <- vapply(p$layers, function(L) inherits(L$geom, "GeomVline"), logical(1))
  expect_equal(unique(built$data[[which(is_vl)]]$linetype), "dotted")
  expect_equal(layer_of(p, "GeomRect")$aes_params$fill, "lightgrey")
})


#####plot_build_forest -- capped error bars#####

test_that("intervals are zero-cap error bars unless the style sets errorbar_width", {
  stats <- df_forest(data_sad_pkforest)
  p <- plot_build_forest(stats, metric = "AUCRATIO")
  expect_equal(layer_of(p, "GeomErrorbar")$aes_params$width, 0)
  expect_false(any(vapply(p$layers, function(L) inherits(L$geom, "GeomLinerange"), logical(1))))

  p2 <- plot_build_forest(stats, metric = "AUCRATIO",
                          style = style_forest(errorbar_width = 0.3))
  eb <- layer_of(p2, "GeomErrorbar")
  expect_equal(eb$geom_params$orientation, "y")
  expect_equal(eb$aes_params$width, 0.3)
  expect_false(any(vapply(p2$layers, function(L) inherits(L$geom, "GeomLinerange"), logical(1))))
  built <- ggplot2::ggplot_build(p2)$data[[which(vapply(p2$layers, function(L)
    inherits(L$geom, "GeomErrorbar"), logical(1)))]]
  expect_equal(unique(round(built$ymax - built$ymin, 8)), 0.3)
  expect_equal(unique(built$colour), "black")
})

test_that("restyle_plot() caps the error bars of a built forest plot", {
  p <- plot_build_forest(df_forest(data_sad_pkforest), metric = "AUCRATIO")
  r <- restyle_plot(p, errorbar_width = 0.3)
  is_eb <- vapply(r$layers, function(L) inherits(L$geom, "GeomErrorbar"), logical(1))
  built <- ggplot2::ggplot_build(r)$data[[which(is_eb)]]
  expect_equal(unique(round(built$ymax - built$ymin, 8)), 0.3)
})

test_that("capped error bars follow per-covariate colors", {
  p <- plot_build_forest(df_forest(data_sad_pkforest), metric = "AUCRATIO",
                         style = style_forest(errorbar_width = 0.3,
                                              colors = c(FOOD = "red", WTBL = "blue")))
  eb <- layer_of(p, "GeomErrorbar")
  expect_true("colour" %in% names(eb$mapping))
  built <- ggplot2::ggplot_build(p)$data[[which(vapply(p$layers, function(L)
    inherits(L$geom, "GeomErrorbar"), logical(1)))]]
  expect_setequal(unique(built$colour), c("red", "blue"))
})


#####style_pumas#####

test_that("style_pumas() returns the Pumas look as a style_spec", {
  st <- style_pumas()
  expect_s3_class(st, "ggstylekit_style_spec")
  expect_true(is.function(st$colors))
  expect_equal(st$colors(3), c("#0072B2", "#E69F00", "#009E73"))
  expect_equal(unname(st$shapes[c("point", "ref_point")]), c(18, 18))
  expect_equal(unname(st$linetypes["ref_line"]), "dashed")
  expect_equal(unname(st$fill["ref_band"]), "grey50")
  expect_equal(unname(st$alphas["ref_band"]), 0.2)
  expect_equal(st$errorbar_width, 0.3)
  th <- st$theme
  expect_s3_class(th$panel.grid.major.x, "element_blank")
  expect_s3_class(th$panel.border, "element_blank")
  expect_s3_class(th$axis.line.x, "element_line")
  expect_equal(as.numeric(th$panel.spacing.y), 0)
})

test_that("plot_build_forest() with style_pumas() colors capped bars and diamonds per covariate", {
  p <- plot_build_forest(df_forest(data_sad_pkforest), metric = "AUCRATIO",
                         ref_band = c(0.8, 1.25), style = style_pumas())
  eb <- layer_of(p, "GeomErrorbar")
  expect_true("colour" %in% names(eb$mapping))
  pt <- layer_of(p, "GeomPoint", 1L)
  expect_true("colour" %in% names(pt$mapping))
  expect_false("fill" %in% names(pt$mapping))
  built <- ggplot2::ggplot_build(p)
  is_pt <- vapply(p$layers, function(L) inherits(L$geom, "GeomPoint"), logical(1))
  is_eb <- vapply(p$layers, function(L) inherits(L$geom, "GeomErrorbar"), logical(1))
  expect_equal(built$data[[which(is_pt)[1]]]$colour, built$data[[which(is_eb)]]$colour)
  expect_true(all(built$data[[which(is_pt)[1]]]$colour %in% style_pumas()$colors(7)))
})
