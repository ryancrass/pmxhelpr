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
                 "est_label", "lo_label", "hi_label", "ci_label", "is_ref"))
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
  expect_error(df_forest(data_sad_pkforest, replicate_var = "SIM"), "unused argument")
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


#####order_panel_labels (internal)#####

# Convenience: data-order `given`, non-factor, no reference row.
ord <- function(labels, is_ref = rep(FALSE, length(labels)),
                given = labels, from_factor = FALSE) {
  pmxhelpr:::order_panel_labels(labels, is_ref, given, from_factor)
}

test_that("Numeric labels sort ascending by value, not by input order", {
  expect_equal(ord(c("90 kg", "50 kg", "70 kg")),
               c("50 kg", "70 kg", "90 kg"))
  expect_equal(ord(c("10 mg", "2.5 mg", "0.5 mg")),
               c("0.5 mg", "2.5 mg", "10 mg"))
})

test_that("Negative labels sort by signed value, not by magnitude", {
  # Regression: the sign used to be swallowed as a non-digit prefix, so these
  # sorted 5, 10, 20 -- i.e. "-5%", "10%", "-20%".
  expect_equal(ord(c("-5%", "10%", "-20%")),
               c("-20%", "-5%", "10%"))
  expect_equal(ord(c("-0.5", "0.5", "-1.5")),
               c("-1.5", "-0.5", "0.5"))
})

test_that("A hyphen inside a label is not read as a minus sign", {
  # Compound word: the number is 50, not -50, so it sorts below "10 kg".
  expect_equal(ord(c("Weight-50 kg", "Weight-10 kg")),
               c("Weight-10 kg", "Weight-50 kg"))
  # Range separator: ordered by the lower bound.
  expect_equal(ord(c("50-60 kg", "20-30 kg", "70-80 kg")),
               c("20-30 kg", "50-60 kg", "70-80 kg"))
  # A real negative still beats both.
  expect_equal(ord(c("50-60 kg", "-5 kg")), c("-5 kg", "50-60 kg"))
})

test_that("A number anywhere in the label is used, not just a leading one", {
  expect_equal(ord(c("Age 65 y", "Age 18 y", "Age 40 y")),
               c("Age 18 y", "Age 40 y", "Age 65 y"))
  expect_equal(ord(c("WT90", "WT50")), c("WT50", "WT90"))
})

test_that("Factor levels win over numeric order when they cover every label", {
  expect_equal(
    ord(c("90 kg", "50 kg"), given = c("90 kg", "50 kg"), from_factor = TRUE),
    c("90 kg", "50 kg")
  )
})

test_that("Non-numeric labels keep the reference on top, then data order", {
  expect_equal(
    ord(c("Fed", "Fasted"), is_ref = c(FALSE, TRUE), given = c("Fed", "Fasted")),
    c("Fasted", "Fed")
  )
  # A set mixing numeric and non-numeric labels takes the same fallback.
  expect_equal(
    ord(c("Fed", "70 kg"), is_ref = c(TRUE, FALSE), given = c("Fed", "70 kg")),
    c("Fed", "70 kg")
  )
})

test_that("A numeric reference label lands in sequence, not forced to the top", {
  expect_equal(
    ord(c("90 kg", "70 kg", "50 kg"), is_ref = c(FALSE, TRUE, FALSE)),
    c("50 kg", "70 kg", "90 kg")
  )
})



#####df_forest -- argument handling#####

test_that("Error if `data` is not a data.frame", {
  expect_error(df_forest("not a data.frame"),
               regexp = "argument `data` must be a `data.frame`")
})

test_that("Error on malformed ci / sigdigits", {
  expect_error(df_forest(data_sad_pkforest, ci = 1.5),
               regexp = "argument `ci` must be a single numeric value between 0 and 1")
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

test_that("check_forest_args() rejects malformed cov_name_ref", {
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

test_that("cov_level_ref names absent from data are ignored; unnamed covariates get no reference row", {
  s <- drawn(df_forest(data_sad_pkforest, cov_level_ref = c(FOOD = "Fasted", SEX = "Male")))
  expect_true(any(s$is_ref & s$cov_var == "FOOD"))
  expect_false(any(s$is_ref & s$cov_var == "WTBL"))
  expect_false("SEX" %in% s$cov_var)
})


#####reference layout (ref_layout)#####

test_that("stats$stats keeps the standalone reference regardless of cov_level_ref", {
  plain <- df_forest(data_sad_pkforest)
  with_map <- df_forest(data_sad_pkforest, cov_level_ref = ref_map)
  expect_equal(with_map$stats, plain$stats)
  expect_equal(with_map$config$ref_after, c(FOOD = NA_character_, WTBL = "60 kg"))
  expect_null(plain$config$ref_after)
})

test_that("ref_layout = NULL distributes only when cov_level_ref was given", {
  plain <- df_forest(data_sad_pkforest)
  with_map <- df_forest(data_sad_pkforest, cov_level_ref = ref_map)
  expect_equal(drawn(plain), plain$stats)
  expect_equal(drawn(with_map), drawn(with_map, "distributed"))
  expect_equal(drawn(with_map, "consolidated"), with_map$stats)
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

test_that("A missing `ref_after` label puts the distributed row at the top of its panel", {
  stats <- df_forest(data_sad_pkforest, cov_level_ref = ref_map)
  stats$config$ref_after <- NULL
  s <- drawn(stats, metric = "AUCRATIO")
  expect_equal(s$cov_val[s$cov_var == "WTBL"], c("70 kg", "50 kg", "60 kg", "80 kg", "90 kg"))
  stats$config$ref_after[["WTBL"]] <- "not a label"
  s <- drawn(stats, metric = "AUCRATIO")
  expect_equal(s$cov_val[s$cov_var == "WTBL"][1], "70 kg")
})

test_that("Covariate rows keep the order the reference label set, in both layouts", {
  d <- dplyr::filter(data_sad_pkforest, metric == "AUCRATIO")
  d$cov_val <- factor(d$cov_val, levels = c("Reference", "Fed", "Fasted",
                                            "50 kg", "70 kg", "90 kg", "60 kg", "80 kg"))
  stats <- df_forest(d, cov_level_ref = ref_map)
  cons <- drawn(stats, "consolidated")
  expect_equal(cons$cov_val[cons$cov_var == "WTBL"], c("50 kg", "90 kg", "60 kg", "80 kg"))
  expect_equal(stats$config$ref_after, c(FOOD = "Fed", WTBL = "50 kg"))
})

test_that("ref_layout errors: distributed without cov_level_ref or a reference panel; bad values", {
  expect_error(plot_build_forest(df_forest(data_sad_pkforest), metric = "AUC",
                                 ref_layout = "distributed"),
               "pass `cov_level_ref` to `df_forest\\(\\)`")
  stats <- df_forest(data_sad_pkforest, cov_level_ref = ref_map)
  stats$stats$is_ref <- FALSE
  expect_error(plot_build_forest(stats, metric = "AUC", ref = 1), "no standalone reference panel")
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
  out$config$ci <- NULL
  expect_error(pmxhelpr:::validate_forest_stats(out), regexp = "missing required config keys")
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
  expect_equal(st$legend.position, "bottom")
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
  # the distributed reference row follows its `ref_after` label (60 kg)
  expect_equal(panel_labels(p2, "y")[[1]], c("50 kg", "70 kg", "60 kg", "80 kg", "90 kg"))
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
  col <- forest_col_labels(s$est_label, s$lo_label, s$hi_label, s$is_ref)
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
                     paste0(s$est_label, "  (", s$lo_label, ", ", s$hi_label, ")"))
  expect_equal(unlist(panel_labels(p, "y.sec")),
               c(rev(expected[s$cov_var == "FOOD"]), rev(expected[s$cov_var == "WTBL"])))
  expect_equal(expected[s$cov_val %in% c("Fasted", "70 kg")], c("1.00", "1.00"))
  expect_equal(expected[s$cov_val == "Fed"], "2.32  (2.00, 2.65)")
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
  expect_equal(layer_of(p, "GeomLinerange")$aes_params$linewidth, 0.6)
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
  expect_error(plot_build_forest(stats, metric = "AUC", ref_band = c(0.8, 1.25), ref_band_type = "nope"))
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
  expect_equal(unname(color_scale$palette(2)), c("firebrick", "steelblue"))
  expect_equal(color_scale$na.value, "grey50")
  expect_equal(color_scale$guide, "none")
  pt <- layer_of(p, "GeomPoint", 1L)
  ref_pt <- layer_of(p, "GeomPoint", 2L)
  expect_true("colour" %in% names(pt$mapping))
  expect_null(pt$aes_params$colour)
  expect_true("colour" %in% names(ref_pt$mapping))
  expect_true("colour" %in% names(layer_of(p, "GeomLinerange")$mapping))
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

test_that("plot_build_forest() warns when style colors are missing entries for present covariates", {
  expect_warning(
    plot_build_forest(df_forest(data_sad_pkforest), metric = "AUCRATIO",
                      style = style_forest(colors = c(FOOD = "red"))),
    regexp = "colors.*missing entries"
  )
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
