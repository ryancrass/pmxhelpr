ref_map <- c(FOOD = "Fasted", WTBL = "70 kg")

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
               c("metric", "cov_var", "cov_val", "est", "lo", "hi", "ci_label", "is_ref"))
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

test_that("`statistic` selects mean / geometric mean for `est`", {
  grp <- data_sad_pkforest[data_sad_pkforest$metric  == "AUCRATIO" &
                           data_sad_pkforest$cov_var == "WTBL" &
                           data_sad_pkforest$cov_val == "50 kg", ]
  pick <- function(s) s[s$metric == "AUCRATIO" & s$cov_var == "WTBL" & s$cov_val == "50 kg", ]
  expect_equal(pick(df_forest(data_sad_pkforest, statistic = "mean")$stats)$est,
               mean(grp$value))
  expect_equal(pick(df_forest(data_sad_pkforest, statistic = "geo_mean")$stats)$est,
               exp(mean(log(grp$value))))
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
  expect_equal(out$config$statistic, "median")
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


#####ci_label formatting#####

test_that("ci_label pads every value to `sigdigits` significant digits", {
  s <- df_forest(data_sad_pkforest, sigdigits = 3)$stats
  expect_equal(s$ci_label[s$cov_var == "Reference" & s$metric == "AUCRATIO"],
               "1.00 [1.00, 1.00]")
  expect_match(s$ci_label[s$cov_val == "90 kg" & s$metric == "AUCRATIO"],
               "^0\\.[0-9]{3} \\[0\\.[0-9]{3}, 0\\.[0-9]{3}\\]$")
  expect_match(s$ci_label, "^[0-9.]+ \\[[0-9.]+, [0-9.]+\\]$", all = TRUE)
  expect_false(identical(df_forest(data_sad_pkforest, sigdigits = 2)$stats$ci_label,
                         df_forest(data_sad_pkforest, sigdigits = 4)$stats$ci_label))
})

test_that("fmt_sig() keeps trailing zeros, rounds to sigdigits, and passes NA through", {
  expect_equal(fmt_sig(c(1, 0.8, 23312, 9969, 2.334, NA), 3),
               c("1.00", "0.800", "23300", "9970", "2.33", NA))
  expect_equal(fmt_sig(1.5, 2), "1.5")
  expect_equal(fmt_sig(0.0001234, 3), "0.000123")
  expect_equal(fmt_sig(c(0, -17.24), 3), c("0.00", "-17.2"))
  expect_equal(fmt_sig(0, 1), "0")
})


#####df_forest -- argument handling#####

test_that("Error if `data` is not a data.frame", {
  expect_error(df_forest("not a data.frame"),
               regexp = "argument `data` must be a `data.frame`")
})

test_that("Error on malformed ci / statistic / sigdigits", {
  expect_error(df_forest(data_sad_pkforest, ci = 1.5),
               regexp = "argument `ci` must be a single numeric value between 0 and 1")
  expect_error(df_forest(data_sad_pkforest, statistic = "mode"),
               regexp = "argument `statistic` must be `median`, `mean`, or `geo_mean`")
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
  expect_error(df_forest(data_sad_pkforest, cov_level_ref = ref_map, cov_name_ref = NULL, is_ref_var = NULL),
               regexp = "requires a non-NULL `cov_name_ref`")
  expect_error(df_forest(data_sad_pkforest, cov_level_ref = ref_map, cov_name_ref = "NOMATCH", is_ref_var = NULL),
               regexp = "no rows with")
})


#####df_forest -- reference rows#####

test_that("Standalone reference: is_ref flags the cov_name_ref rows and they sort first", {
  s <- df_forest(data_sad_pkforest)$stats
  expect_equal(s$is_ref, s$cov_var == "Reference")
  expect_equal(unique(s$cov_var), c("Reference", "FOOD", "WTBL"))
  s_base <- df_forest(data_sad_pkforest, cov_name_ref = "Baseline", is_ref_var = NULL)$stats
  expect_false(any(s_base$is_ref))
})

test_that("cov_name_ref = NULL, is_ref_var = NULL flags no reference and keeps data order", {
  s <- df_forest(data_sad_pkforest, cov_name_ref = NULL, is_ref_var = NULL)$stats
  expect_false(any(s$is_ref))
  expect_equal(unique(s$cov_var), unique(data_sad_pkforest$cov_var))
})

test_that("cov_level_ref disperses the reference row into each named covariate", {
  s <- df_forest(data_sad_pkforest, cov_level_ref = ref_map)$stats
  expect_false("Reference" %in% s$cov_var)
  auc <- s[s$metric == "AUCRATIO", ]
  expect_equal(auc$cov_val[auc$is_ref & auc$cov_var == "FOOD"], "Fasted")
  expect_equal(auc$cov_val[auc$is_ref & auc$cov_var == "WTBL"], "70 kg")
  # one dispersed row per metric per named covariate
  expect_equal(sum(s$is_ref), 2L * length(unique(s$metric)))
})

test_that("Dispersed reference rows inherit est/lo/hi/ci_label from the reference row", {
  plain <- df_forest(data_sad_pkforest)$stats
  ref <- plain[plain$cov_var == "Reference" & plain$metric == "AUCRATIO", ]
  s <- df_forest(data_sad_pkforest, cov_level_ref = ref_map)$stats
  fasted <- s[s$metric == "AUCRATIO" & s$cov_var == "FOOD" & s$cov_val == "Fasted", ]
  expect_equal(fasted$est, ref$est)
  expect_equal(fasted$lo,  ref$lo)
  expect_equal(fasted$hi,  ref$hi)
  expect_equal(fasted$ci_label, ref$ci_label)
})

test_that("cov_level_ref names absent from data are ignored; unnamed covariates get no reference row", {
  s <- df_forest(data_sad_pkforest, cov_level_ref = c(FOOD = "Fasted", SEX = "Male"))$stats
  expect_true(any(s$is_ref & s$cov_var == "FOOD"))
  expect_false(any(s$is_ref & s$cov_var == "WTBL"))
  expect_false("SEX" %in% s$cov_var)
})


#####df_forest -- row order#####

test_that("Numeric-leading labels sort ascending; the dispersed reference lands in sequence", {
  s <- df_forest(data_sad_pkforest, cov_level_ref = ref_map)$stats
  expect_equal(s$cov_val[s$metric == "AUCRATIO" & s$cov_var == "WTBL"],
               c("50 kg", "60 kg", "70 kg", "80 kg", "90 kg"))
  s0 <- df_forest(data_sad_pkforest)$stats
  expect_equal(s0$cov_val[s0$metric == "AUCRATIO"],
               c("Reference", "Fed", "50 kg", "60 kg", "80 kg", "90 kg"))
})

test_that("Categorical labels keep first-appearance order from `data`, reference label on top", {
  set.seed(1)
  d <- expand.grid(metric = "AUCRATIO", cov_var = "RENAL",
                   cov_val = c("Normal", "Mild", "Moderate", "Severe"),
                   SIM = 1:20, stringsAsFactors = FALSE)
  d$value <- stats::rlnorm(nrow(d), 0, 0.2)
  expect_equal(df_forest(d, cov_name_ref = NULL, is_ref_var = NULL)$stats$cov_val,
               c("Normal", "Mild", "Moderate", "Severe"))
  s <- df_forest(data_sad_pkforest, cov_level_ref = ref_map)$stats
  expect_equal(s$cov_val[s$metric == "AUCRATIO" & s$cov_var == "FOOD"], c("Fasted", "Fed"))
})

test_that("Labels sharing a leading number fall back to data order", {
  set.seed(1)
  d <- expand.grid(metric = "AUCRATIO", cov_var = "CYP2D6",
                   cov_val = c("CYP2D6 PM", "CYP2D6 IM", "CYP2D6 UM"),
                   SIM = 1:20, stringsAsFactors = FALSE)
  d$value <- stats::rlnorm(nrow(d), 0, 0.2)
  expect_equal(df_forest(d, cov_name_ref = NULL, is_ref_var = NULL)$stats$cov_val,
               c("CYP2D6 PM", "CYP2D6 IM", "CYP2D6 UM"))
})

test_that("Factor levels on cov_level_var control the row order, including the reference label", {
  d <- dplyr::filter(data_sad_pkforest, metric == "AUCRATIO")
  d$cov_val <- factor(d$cov_val, levels = c("Reference", "Fed", "Fasted",
                                            "50 kg", "70 kg", "90 kg", "60 kg", "80 kg"))
  s <- df_forest(d, cov_level_ref = ref_map)$stats
  expect_equal(s$cov_val[s$cov_var == "FOOD"], c("Fed", "Fasted"))
  expect_equal(s$cov_val[s$cov_var == "WTBL"], c("50 kg", "70 kg", "90 kg", "60 kg", "80 kg"))
  # a label outside the factor levels falls back to the heuristics
  s2 <- df_forest(d, cov_level_ref = c(FOOD = "No food", WTBL = "70 kg"))$stats
  expect_equal(s2$cov_val[s2$cov_var == "FOOD"], c("No food", "Fed"))
})

test_that("Factor levels on cov_name_var and metric_name_var control panel and metric order", {
  d <- data_sad_pkforest
  d$cov_var <- factor(d$cov_var, levels = c("WTBL", "FOOD", "Reference"))
  d$metric <- factor(d$metric, levels = rev(sort(unique(d$metric))))
  s <- df_forest(d, cov_name_ref = NULL, is_ref_var = NULL)$stats
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
  # estimate column text and header at full axis size
  expect_null(th$axis.text.y.right$size)
  expect_equal(unclass(th$plot.tag$size), 1)
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

test_that("plot_build_forest() facet_grid uses cov_name rows only; title carries the metric", {
  p <- plot_build_forest(df_forest(data_sad_pkforest), metric = "AUCRATIO")
  expect_s3_class(p$facet, "FacetGrid")
  row_vars <- vapply(p$facet$params$rows, rlang::as_name, character(1))
  expect_equal(unname(row_vars), "cov_var")
  expect_length(p$facet$params$cols, 0L)
  expect_true(isTRUE(p$facet$params$space_free$y))
  expect_null(p$labels$x)
  expect_null(p$labels$y)
  expect_equal(p$labels$title, "AUCRATIO")
})

test_that("Rows draw top to bottom in stats order; panels follow row order", {
  stats <- df_forest(data_sad_pkforest, cov_level_ref = ref_map)
  p <- plot_build_forest(stats, metric = "AUCRATIO")
  s <- stats$stats[stats$stats$metric == "AUCRATIO", ]
  expect_equal(p$data$cov_val, s$cov_val)
  expect_equal(levels(p$data$cov_var), c("FOOD", "WTBL"))
  # bottom-to-top per panel is the reverse of the stats order
  expect_equal(panel_labels(p, "y"),
               list(rev(s$cov_val[s$cov_var == "FOOD"]), rev(s$cov_val[s$cov_var == "WTBL"])))
  # reordering stats$stats reorders the plot
  stats$stats <- stats$stats[rev(seq_len(nrow(stats$stats))), ]
  p2 <- plot_build_forest(stats, metric = "AUCRATIO")
  expect_equal(levels(p2$data$cov_var), c("WTBL", "FOOD"))
  expect_equal(panel_labels(p2, "y")[[1]], s$cov_val[s$cov_var == "WTBL"])
})

test_that("Covariate level labels may repeat across panels", {
  set.seed(1)
  draws <- expand.grid(METRIC = "AUC", COV = c("WTBL", "AGE"), COVLVL = c("Low", "High"),
                       REP = 1:50, stringsAsFactors = FALSE)
  draws$VALUE <- stats::rlnorm(nrow(draws), 0, 0.2)
  stats <- df_forest(draws, metric_name_var = "METRIC", cov_name_var = "COV",
                     cov_level_var = "COVLVL", metric_value_var = "VALUE", cov_name_ref = NULL, is_ref_var = NULL)
  p <- plot_build_forest(stats, ref = 1)
  expect_equal(panel_labels(p, "y"), list(c("High", "Low"), c("High", "Low")))
  s <- stats$stats
  expect_equal(panel_labels(p, "y.sec"),
               list(rev(s$ci_label[s$COV == "WTBL"]), rev(s$ci_label[s$COV == "AGE"])))
})


#####plot_build_forest -- estimate column (secondary y-axis)#####

test_that("ci_label is the secondary y-axis text; reference rows show the estimate alone", {
  stats <- df_forest(data_sad_pkforest, cov_level_ref = ref_map)
  p <- plot_build_forest(stats, metric = "AUCRATIO")
  expect_false(has_geom(p, "GeomText"))
  expect_s3_class(sec_axis_of(p), "AxisSecondary")
  s <- stats$stats[stats$stats$metric == "AUCRATIO", ]
  expected <- ifelse(s$is_ref, fmt_sig(s$est, 3), s$ci_label)
  expect_equal(unlist(panel_labels(p, "y.sec")),
               c(rev(expected[s$cov_var == "FOOD"]), rev(expected[s$cov_var == "WTBL"])))
  expect_equal(expected[s$cov_val %in% c("Fasted", "70 kg")], c("1.00", "1.00"))
  expect_match(expected[s$cov_val == "Fed"], "^2\\.[0-9]{2} \\[")
})

test_that("the estimate column header is the plot tag built from statistic and ci", {
  p <- plot_build_forest(df_forest(data_sad_pkforest), metric = "AUCRATIO")
  expect_equal(p$labels$tag, "Median [90% CI]")
  expect_null(sec_axis_of(p)$name)
  stats_mean <- df_forest(data_sad_pkforest, statistic = "mean", ci = 0.95)
  expect_equal(plot_build_forest(stats_mean, metric = "AUCRATIO")$labels$tag, "Mean [95% CI]")
  stats_geo <- df_forest(data_sad_pkforest, statistic = "geo_mean", ci = 0.8)
  expect_equal(plot_build_forest(stats_geo, metric = "AUCRATIO")$labels$tag,
               "Geometric mean [80% CI]")
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
  p2 <- plot_build_forest(df_forest(data_sad_pkforest, cov_name_ref = NULL, is_ref_var = NULL),
                          metric = "AUCRATIO", ref = 1)
  expect_equal(n_geom(p2, "GeomPoint"), 1L)
})


#####plot_build_forest -- ref / ref_band / show_legend#####

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
  # dispersed reference rows share one estimate and resolve the same way
  stats_d <- df_forest(data_sad_pkforest, cov_level_ref = ref_map)
  p_d <- plot_build_forest(stats_d, metric = "AUC")
  expect_equal(layer_of(p_d, "GeomVline")$data$xintercept, ref_auc)
})

test_that("ref = \"reference\" with no reference row warns and draws no line", {
  stats <- df_forest(data_sad_pkforest, cov_name_ref = NULL, is_ref_var = NULL)
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

test_that("show_legend = FALSE (default) hides the band guide and adds no linetype scale", {
  p <- plot_build_forest(df_forest(data_sad_pkforest), metric = "AUCRATIO", ref_band = c(0.8, 1.25))
  scale_aes <- scale_aes_of(p)
  expect_false("linetype" %in% scale_aes)
  fill_scale <- p$scales$scales[[which(scale_aes == "fill")]]
  expect_equal(fill_scale$guide, "none")
  expect_false("linetype" %in% names(layer_of(p, "GeomVline")$mapping))
})

test_that("show_legend = TRUE maps the band and line to manual scales keyed for the legend", {
  p <- plot_build_forest(df_forest(data_sad_pkforest), metric = "AUCRATIO",
                         ref_band = c(0.8, 1.25), show_legend = TRUE,
                         style = style_forest(fill = c(ref_band = "skyblue"),
                                              linetypes = c(ref_line = "dotted")))
  scale_aes <- scale_aes_of(p)
  expect_true(all(c("fill", "linetype") %in% scale_aes))
  expect_equal(p$scales$scales[[which(scale_aes == "fill")]]$guide, "legend")
  rect <- layer_of(p, "GeomRect")
  vl <- layer_of(p, "GeomVline")
  expect_true("fill" %in% names(rect$mapping))
  expect_true("linetype" %in% names(vl$mapping))
  expect_equal(vl$aes_params$colour, "grey30")
  built <- ggplot2::ggplot_build(p)
  is_rect <- vapply(p$layers, function(L) inherits(L$geom, "GeomRect"), logical(1))
  is_vl <- vapply(p$layers, function(L) inherits(L$geom, "GeomVline"), logical(1))
  expect_equal(unique(built$data[[which(is_rect)]]$fill), "skyblue")
  expect_equal(unique(built$data[[which(is_vl)]]$linetype), "dotted")
  expect_equal(p$theme$legend.position, "bottom")
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
  expect_equal(p$theme$plot.tag.position, "topright")
  expect_equal(p$theme$plot.tag.location, "plot")
  expect_equal(p$theme$plot.tag$face, "bold")
})


#####plot_forest -- dual-mode dispatch#####

test_that("plot_forest() raw-data path returns a ggplot and forwards the pipeline args", {
  p <- plot_forest(data_sad_pkforest, metric = "AUCRATIO", cov_level_ref = ref_map,
                   cov_name_var = cov_var)
  expect_s3_class(p, "ggplot")
  expect_false("Reference" %in% p$data$cov_var)
  expect_setequal(p$data$cov_val[p$data$is_ref], c("Fasted", "70 kg"))
  p2 <- plot_forest(data_sad_pkforest, metric = "AUCRATIO", cov_name_ref = NULL, is_ref_var = NULL, ref = 1)
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

test_that("plot_forest() passes show_legend through on both paths", {
  p1 <- plot_forest(df_forest(data_sad_pkforest), metric = "AUCRATIO",
                    ref_band = c(0.8, 1.25), show_legend = TRUE)
  p2 <- plot_forest(dplyr::filter(data_sad_pkforest, metric == "AUCRATIO"),
                    ref_band = c(0.8, 1.25), show_legend = TRUE)
  for (p in list(p1, p2)) {
    expect_true(all(c("fill", "linetype") %in% scale_aes_of(p)))
  }
})


#####df_forest -- is_ref flag column#####

flag_data <- function(cov_name = "Reference") {
  d <- data_sad_pkforest[data_sad_pkforest$metric == "AUCRATIO", ]
  d$cov_var[d$is_ref] <- cov_name
  d$cov_val[d$is_ref] <- cov_name
  d
}

test_that("An `is_ref` column identifies the reference rows", {
  d <- flag_data()
  out <- df_forest(d)
  expect_equal(out$config$is_ref_var, "is_ref")
  expect_true(all(out$stats$is_ref[out$stats$cov_var == "Reference"]))
  expect_false(any(out$stats$is_ref[out$stats$cov_var != "Reference"]))
})

test_that("The flag identifies the reference when `cov_var` is not `cov_name_ref`", {
  d <- flag_data("Typical subject")
  out <- df_forest(d, cov_name_ref = NULL)
  expect_equal(out$stats$cov_var[out$stats$is_ref], "Typical subject")
  # the flagged panel still sorts to the top
  expect_equal(out$stats$cov_var[1], "Typical subject")
})

test_that("The flag takes precedence over a disagreeing `cov_name_ref`", {
  d <- flag_data("Typical subject")
  with_flag <- df_forest(d)$stats
  without <- df_forest(d, is_ref_var = NULL)$stats
  expect_equal(with_flag$cov_var[with_flag$is_ref], "Typical subject")
  expect_false(any(without$is_ref))
})

test_that("An absent default flag column falls back to `cov_name_ref`", {
  d <- flag_data()
  no_flag <- d
  no_flag$is_ref <- NULL
  expect_equal(df_forest(no_flag)$stats, df_forest(d)$stats)
  expect_null(df_forest(no_flag)$config$is_ref_var)
})

test_that("An explicitly named flag column that is missing aborts", {
  d <- flag_data()
  d$is_ref <- NULL
  expect_error(df_forest(d, is_ref_var = "ref_flag"), "is_ref_var")
})

test_that("`cov_level_ref` disperses from the flag without `cov_name_ref`", {
  d <- flag_data("Typical subject")
  s <- df_forest(d, cov_name_ref = NULL, cov_level_ref = ref_map)$stats
  expect_equal(s$cov_val[s$cov_var == "FOOD"], c("Fasted", "Fed"))
  expect_equal(s$cov_val[s$cov_var == "WTBL"],
               c("50 kg", "60 kg", "70 kg", "80 kg", "90 kg"))
  expect_true(all(s$is_ref[s$cov_val %in% c("Fasted", "70 kg")]))
  expect_false("Typical subject" %in% s$cov_var)
})

test_that("Dispersal aborts when no covariate is entirely flagged", {
  d <- flag_data()
  d$is_ref <- FALSE
  expect_error(
    df_forest(d, cov_name_ref = NULL, cov_level_ref = ref_map),
    "no covariate"
  )
})

test_that("A flag that varies within a group aborts", {
  d <- flag_data()
  d$is_ref[which(d$is_ref)[1]] <- FALSE
  expect_error(df_forest(d), "not constant within")
})

test_that("A non-logical flag column is coerced, and `NA` aborts", {
  d <- flag_data()
  d$is_ref <- as.numeric(d$is_ref)
  expect_true(all(df_forest(d)$stats$is_ref[df_forest(d)$stats$cov_var == "Reference"]))
  d$is_ref <- NA
  expect_error(df_forest(d), "no `NA` values")
})

test_that("`plot_forest()` forwards `is_ref_var` on the raw path only", {
  d <- flag_data("Typical subject")
  expect_s3_class(plot_forest(d, cov_name_ref = NULL), "ggplot")
  stats <- df_forest(d)
  expect_error(plot_forest(stats, is_ref_var = "is_ref"), "pipeline arguments")
})
