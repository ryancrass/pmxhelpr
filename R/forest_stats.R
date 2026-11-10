#' Internal helper: display order of a grouping column
#'
#' Factor levels when `x` is a factor (restricted to the levels present),
#' otherwise order of first appearance.
#'
#' @param x Factor or atomic vector.
#' @return Character vector of unique values in display order.
#' @keywords internal
level_order <- function(x) {
  if (is.factor(x)) {
    lv <- levels(x)
    return(lv[lv %in% x])
  }
  unique(as.character(x))
}


#' Compute Summary Statistics for Covariate Forest Plots
#'
#' @description
#' Summarizes replicate draws for each metric and covariate level into a
#' median and a `ci` percentile interval, with formatted labels, and returns
#' a `forest_stats` object for [plot_forest()] / [plot_build_forest()]. Rows
#' are in plot order, top to bottom; reorder `stats$stats` to change it. One
#' object draws either reference layout (see `ref_layout` in
#' [plot_build_forest()]) without re-aggregating.
#'
#' @param data Input data.frame. Replicate level long format dataset: one row
#'    per metric, covariate, covariate level, and replicate.
#' @param metric_name_var Column in `data` containing metric names.
#'    Accepts bare names or strings. Default is `metric`.
#' @param metric_value_var Column in `data` containing replicate draws of the
#'    test/reference comparison statistic.
#'    Accepts bare names or strings. Default `value`.
#' @param cov_name_var Column in `data` containing covariate variable names.
#'    (e.g., `WTBL`,`FOOD`, etc).
#'    Accepts bare names or strings. Default is `cov_name`.
#' @param cov_level_var Column in `data` containing covariate levels (e.g., `70 kg`).
#'    Accepts bare names or strings. Default is `cov_level`.
#' @param ci Numeric scalar in `(0, 1)` specifying the central interval
#'    width (e.g., `0.9` for 90% CI).
#' @param sigdigits Number of significant digits used to format the label
#'    columns. Digits are counted on the median estimate of each metric,
#'    which fixes one count of decimal places for every value of that metric
#'    (`"1.00"`, `"0.95"`, `"2.32"` for a ratio; `"12500"`, `"9500"` for an
#'    exposure near 12500). Trailing zeros are kept. Default `3`.
#' @param cov_name_ref The value of `cov_name_var` that marks the reference
#'    rows, drawn in the reference panel at the top. Default `"Reference"`,
#'    as in [df_forest_preprocess()] output; `NULL` for no reference row.
#' @param cov_level_ref Optional named vector of per-covariate reference
#'    labels, e.g. `c(FOOD = "Fasted", WTBL = "70 kg")`. When given, the
#'    reference row is also drawn inside each named covariate panel under
#'    that label, and this becomes the default `ref_layout`. Unnamed
#'    covariates show no reference row. Requires `cov_name_ref`. Default
#'    `NULL` (reference panel only).
#'
#' @section Reference rows:
#' Reference rows are flagged `is_ref` and drawn through the `ref_point` style
#' role. With `cov_level_ref`, `stats$stats` holds both the reference-panel
#' rows and a copy in each named covariate panel, told apart by the `layout`
#' column. `cov_level_ref` names not found in `cov_name_var`, or naming the
#' reference itself, are ignored with a warning; a label equal to an
#' existing level of that covariate also warns, since both rows would share
#' it.
#'
#' @section Row order:
#' Rows of `stats$stats` are sorted into display order, top to bottom:
#' metrics and covariates follow their factor levels when the input columns
#' are factors, otherwise their order of first appearance in `data`, with the
#' reference panel moved first. Within a covariate, factor levels of
#' `cov_level_var` win when they cover every label (including the
#' covariate's reference label); otherwise, if every label carries a distinct
#' number, labels sort ascending by it, sign included (`50 kg` above `90 kg`);
#' any other set puts the reference label on top followed by the data order.
#' The reference copies are placed by the same rules, so the covariate rows keep one order in both
#' layouts.
#'
#' @family forest plot
#' @return A `forest_stats` container (subclass of `pmx_stats`) with three
#'    slots:
#'    \describe{
#'      \item{`stats`}{One row per plotted point, in display order, with
#'        character columns named in `metric_name_var` / `cov_name_var` /
#'        `cov_level_var`, canonical numeric columns `est` (median of the
#'        draws), `lo`, `hi` (the `(1 - ci) / 2` and `1 - (1 - ci) / 2`
#'        percentiles), the integer `n` (the number of non-`NA` draws behind
#'        the row), the character columns `est_label`, `lo_label`, and
#'        `hi_label` (those values formatted per `sigdigits`), `ci_label`
#'        (the three combined for tables, e.g. `"1.12 (0.95, 1.32)"`), the
#'        logical `is_ref`, and `layout`, the reference layout(s) a row draws
#'        in: `"both"` (covariate rows), `"consolidated"` (the reference
#'        panel), or `"distributed"` (the reference copies placed in each
#'        `cov_level_ref` covariate). The `cov_name_var` value drives the per-row
#'        facet strip; the `cov_level_var` value drives the y-axis tick label;
#'        [plot_build_forest()] assembles `est_label`, `lo_label`, and
#'        `hi_label` into the right-hand secondary y-axis text, so edit those
#'        columns (not `ci_label`) to change the plotted text.}
#'      \item{`obs`}{`NULL`. No scatter overlay for the forest family.}
#'      \item{`config`}{Named list with column names, `ci`,
#'        `sigdigits`, `cov_name_ref`, and `cov_level_ref`.}
#'    }
#'    Pass directly to [plot_forest()] or [plot_build_forest()] to render.
#' @export df_forest
#'
#' @examples
#' # Aggregate replicate simulations to point estimate + 90% CI
#' df_forest(dplyr::filter(data_sad_pkforest, grepl("RATIO$", metric)))
#'
#' # A reference named something other than "Reference"
#' renamed <- dplyr::mutate(
#'   data_sad_pkforest,
#'   cov_level = ifelse(cov_name == "Reference", "70 kg, fasted", cov_level),
#'   cov_name = ifelse(cov_name == "Reference", "Typical subject", cov_name)
#' )
#' df_forest(renamed, cov_name_ref = "Typical subject")
#'
#' # Per-covariate reference labels (the dataset's attribute,
#' # c(FOOD = "Fasted", WTBL = "70 kg")): the reference row is drawn in each
#' # covariate panel
#' df_forest(
#'   dplyr::filter(data_sad_pkforest, metric == "AUCRATIO"),
#'   cov_level_ref = attr(data_sad_pkforest, "cov_level_ref")
#' )
#'
#' # Control the row order with factor levels on the input
#' d <- dplyr::filter(data_sad_pkforest, metric == "AUCRATIO")
#' d$cov_level <- factor(d$cov_level, levels = c("Reference", "Fed", "Fasted",
#'                                           "50 kg", "70 kg", "90 kg"))
#' df_forest(d, cov_level_ref = attr(data_sad_pkforest, "cov_level_ref"))

df_forest <- function(
  data,
  metric_name_var = "metric",
  metric_value_var = "value",
  cov_name_var = "cov_name",
  cov_level_var = "cov_level",
  ci = 0.9,
  sigdigits = 3,
  cov_name_ref = "Reference",
  cov_level_ref = NULL
) {
  metric_name_var_str <- resolve_var(rlang::enquo(metric_name_var))
  metric_value_var_str <- resolve_var(rlang::enquo(metric_value_var))
  cov_name_var_str <- resolve_var(rlang::enquo(cov_name_var))
  cov_level_var_str <- resolve_var(rlang::enquo(cov_level_var))

  check_df(data, "data")
  check_quantile_scalar(ci, "ci")
  check_positive_integer(sigdigits, "sigdigits")
  check_string(cov_name_ref, "cov_name_ref", null_ok = TRUE)
  if (!is.null(cov_level_ref)) check_named_vector(cov_level_ref, "cov_level_ref")
  check_varsindf(
    data,
    c(metric_name_var_str, metric_value_var_str, cov_name_var_str, cov_level_var_str),
    "data",
    "metric_name_var/metric_value_var/cov_name_var/cov_level_var"
  )
  ## Metric and covariate names key the panels and the row order, so a
  ## missing name has nowhere to draw.
  has_na <- Filter(function(v) anyNA(data[[v]]), c(metric_name_var_str, cov_name_var_str))
  if (length(has_na) > 0) {
    rlang::abort(message = paste0(
      "columns `", paste(has_na, collapse = "`, `"), "` of `data` must not contain `NA`"
    ))
  }

  if (!is.null(cov_level_ref) && is.null(cov_name_ref)) {
    rlang::abort(message = paste0(
      "argument `cov_level_ref` requires a non-NULL `cov_name_ref` ",
      "identifying the reference row to distribute."
    ))
  }

  ## Display order is read from the input before `summarise()` sorts the groups.
  metric_order <- level_order(data[[metric_name_var_str]])
  cov_order <- level_order(data[[cov_name_var_str]])
  cov_name_chr <- as.character(data[[cov_name_var_str]])
  level_from_factor <- is.factor(data[[cov_level_var_str]])
  ## Full factor levels (not just those present) so a distributed reference
  ## label the factor declares still places by its level.
  level_given <- lapply(stats::setNames(nm = cov_order), function(cn) {
    x <- data[[cov_level_var_str]][cov_name_chr == cn]
    if (level_from_factor) levels(x) else unique(as.character(x))
  })

  probs <- c((1 - ci) / 2, 1 - (1 - ci) / 2)
  grp_vars <- c(metric_name_var_str, cov_name_var_str, cov_level_var_str)
  stats <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_vars))) |>
    dplyr::summarise(
      est = stats::median(.data[[metric_value_var_str]], na.rm = TRUE),
      lo = unname(stats::quantile(.data[[metric_value_var_str]], probs[1], na.rm = TRUE)),
      hi = unname(stats::quantile(.data[[metric_value_var_str]], probs[2], na.rm = TRUE)),
      n = sum(!is.na(.data[[metric_value_var_str]])),
      .groups = "drop"
    ) |>
    as.data.frame()
  for (v in grp_vars) stats[[v]] <- as.character(stats[[v]])

  ## Reference rows match `cov_name_ref` and form the reference panel, drawn
  ## only in the consolidated layout. With `cov_level_ref`, a
  ## copy of each metric's reference row is added to every named covariate
  ## that metric has, under that covariate's reference label, drawn only in
  ## the distributed layout. All other rows draw in both.
  stats$is_ref <- FALSE
  ref_covs <- character(0)
  if (!is.null(cov_name_ref)) {
    stats$is_ref <- stats[[cov_name_var_str]] == cov_name_ref
    ref_covs <- intersect(cov_name_ref, stats[[cov_name_var_str]])
  }
  if (!is.null(cov_level_ref) && length(ref_covs) == 0L) {
    rlang::abort(message = paste0(
      "cannot distribute `", cov_name_ref, "` row: no rows with `",
      cov_name_var_str, " == \"", cov_name_ref, "\"` were found in `data`."
    ))
  }
  unmatched <- setdiff(names(cov_level_ref), cov_order)
  if (length(unmatched) > 0) {
    rlang::warn(paste0(
      "`cov_level_ref` names covariates not found in `", cov_name_var_str,
      "`: ", paste(unmatched, collapse = ", "),
      ". No reference row is distributed for them."
    ))
  }
  self_ref <- intersect(names(cov_level_ref), ref_covs)
  if (length(self_ref) > 0) {
    rlang::warn(paste0(
      "`cov_level_ref` names the reference row itself (`", self_ref,
      "`); the entry is ignored."
    ))
  }
  stats$layout <- ifelse(stats$is_ref, "consolidated", "both")
  ref_rows <- stats[stats$is_ref, , drop = FALSE]
  targets <- intersect(names(cov_level_ref), setdiff(cov_order, ref_covs))
  clash <- Filter(function(cn) {
    as.character(cov_level_ref[[cn]]) %in%
      stats[[cov_level_var_str]][stats[[cov_name_var_str]] == cn]
  }, targets)
  if (length(clash) > 0) {
    rlang::warn(paste0(
      "`cov_level_ref` labels match an existing level of the same covariate: ",
      paste0(clash, " = \"", cov_level_ref[clash], "\"", collapse = ", "),
      ". The distributed reference row and the test row share that label."
    ))
  }
  copies <- lapply(targets, function(cn) {
    present <- stats[[metric_name_var_str]][stats[[cov_name_var_str]] == cn]
    copy <- ref_rows[ref_rows[[metric_name_var_str]] %in% present, , drop = FALSE]
    copy[[cov_name_var_str]] <- rep(cn, nrow(copy))
    copy[[cov_level_var_str]] <- rep(as.character(cov_level_ref[[cn]]), nrow(copy))
    copy$layout <- rep("distributed", nrow(copy))
    copy
  })
  stats <- do.call(rbind, c(list(stats), copies))
  cov_order <- c(intersect(ref_covs, cov_order), setdiff(cov_order, ref_covs))

  ## Sort into display order (top to bottom): metric, covariate, then the
  ## per-panel label order from `order_col_factor()`. Factor levels win when
  ## they cover every label in the panel (so the caller controls placement,
  ## including the reference label); otherwise the panel's labels go in
  ## reference first, then data order, and `order_col_factor()` sorts them
  ## by value when every label carries a distinct number. Distributed reference
  ## labels are ordered with the covariate's own labels, so both layouts
  ## share one test-row order.
  level_rank <- integer(nrow(stats))
  for (cn in cov_order) {
    in_panel <- stats[[cov_name_var_str]] == cn
    labels <- stats[[cov_level_var_str]][in_panel]
    given <- level_given[[cn]]
    panel <- if (level_from_factor && all(labels %in% given)) {
      factor(labels, levels = given)
    } else {
      c(labels[stats$is_ref[in_panel]], given[given %in% labels])
    }
    level_rank[in_panel] <- match(labels, levels(order_col_factor(panel)))
  }
  stats <- stats[order(
    match(stats[[metric_name_var_str]], metric_order),
    match(stats[[cov_name_var_str]], cov_order),
    level_rank
  ), , drop = FALSE]
  rownames(stats) <- NULL

  ## One decimal count per metric, set by `sigdigits` of its median estimate.
  anchor <- stats::ave(abs(stats$est), stats[[metric_name_var_str]],
                       FUN = function(x) stats::median(x, na.rm = TRUE))
  stats$est_label <- fmt_sig(stats$est, sigdigits, anchor = anchor)
  stats$lo_label <- fmt_sig(stats$lo, sigdigits, anchor = anchor)
  stats$hi_label <- fmt_sig(stats$hi, sigdigits, anchor = anchor)
  stats$ci_label <- paste0(
    stats$est_label, " (", stats$lo_label, ", ", stats$hi_label, ")"
  )
  stats$ci_label[is.na(stats$est)] <- NA_character_
  stats <- stats[, c(grp_vars, "est", "lo", "hi", "n", "est_label", "lo_label",
                     "hi_label", "ci_label", "is_ref", "layout")]

  pmx_stats(
    stats = stats,
    obs = NULL,
    config = list(
      metric_name_var = metric_name_var_str,
      metric_value_var = metric_value_var_str,
      cov_name_var = cov_name_var_str,
      cov_level_var = cov_level_var_str,
      ci = ci,
      sigdigits = sigdigits,
      cov_name_ref = cov_name_ref,
      cov_level_ref = cov_level_ref
    ),
    subclass = "forest_stats"
  )
}


#' Validate a `forest_stats` object
#'
#' @description
#' Internal helper. Asserts that `x` carries the `forest_stats` class,
#' contains the canonical stats columns, and carries the config keys
#' downstream consumers (notably [plot_build_forest()]) depend on. Aborts
#' with a clear message on failure; returns `x` invisibly on success.
#'
#' @param x Object to validate.
#'
#' @return `invisible(x)` on success.
#' @keywords internal

validate_forest_stats <- function(x) {
  validate_pmx_stats(
    x, "forest_stats", "df_forest",
    stats_cols = c("est", "lo", "hi", "est_label", "lo_label", "hi_label",
                   "ci_label", "is_ref", "layout"),
    config_keys = c("metric_name_var", "cov_name_var", "cov_level_var",
                    "ci", "sigdigits")
  )
  check_varsindf(
    x$stats,
    c(x$config$metric_name_var, x$config$cov_name_var, x$config$cov_level_var),
    "stats",
    "config$metric_name_var/cov_name_var/cov_level_var"
  )
  ## Column types and values the renderer relies on, since users may edit
  ## `stats$stats` directly. Estimates and labels may be NA (all-NA draws).
  s <- x$stats
  bad_type <- function(cols, pred, what) {
    bad <- cols[!vapply(s[cols], pred, logical(1))]
    if (length(bad) > 0) {
      rlang::abort(message = paste0(
        "`stats$stats` columns must be ", what, ": ", paste(bad, collapse = ", ")
      ))
    }
  }
  bad_type(c("est", "lo", "hi"), is.numeric, "numeric")
  bad_type(c("est_label", "lo_label", "hi_label", "ci_label"), is.character, "character")
  if (!is.logical(s$is_ref) || anyNA(s$is_ref)) {
    rlang::abort(message = "`stats$stats$is_ref` must be logical with no `NA`")
  }
  bad_layout <- setdiff(unique(s$layout), c("both", "consolidated", "distributed"))
  if (length(bad_layout) > 0) {
    rlang::abort(message = paste0(
      "`stats$stats$layout` must be \"both\", \"consolidated\", or \"distributed\"; found: ",
      paste(bad_layout, collapse = ", ")
    ))
  }
  check_quantile_scalar(x$config$ci, "config$ci")
  check_positive_integer(x$config$sigdigits, "config$sigdigits")
  invisible(x)
}


#' Test whether an object is a `forest_stats` container
#'
#' @description
#' Predicate that returns `TRUE` if `x` carries the `"forest_stats"` class
#' -- i.e., it is the container returned by [df_forest()] (also accepted by
#' [plot_forest()] and [plot_build_forest()] on the precomputed-stats
#' fast path).
#'
#' @param x Object to test.
#' @param strict Logical. When `TRUE`, additionally runs
#'    `validate_forest_stats()` and returns `FALSE` on validation failure.
#'    Default `FALSE` (class-tag check only, cheap).
#'
#' @family forest plot
#' @return Logical scalar.
#' @export is_forest_stats
#'
#' @examples
#' set.seed(1)
#' draws <- expand.grid(
#'   METRIC = c("AUC", "CMAX"),
#'   COV = c("WTBL", "AGE"),
#'   COVLVL = c("Low", "High"),
#'   REP = 1:50,
#'   stringsAsFactors = FALSE
#' )
#' draws$VALUE <- rlnorm(nrow(draws), 0, 0.2)
#' stats <- df_forest(
#'   draws,
#'   metric_name_var = "METRIC",
#'   cov_name_var = "COV",
#'   cov_level_var = "COVLVL",
#'   metric_value_var = "VALUE",
#'   cov_name_ref = NULL
#' )
#' is_forest_stats(stats)                  # TRUE
#' is_forest_stats(as.data.frame(stats))   # FALSE -- coerced to plain frame

is_forest_stats <- function(x, strict = FALSE) {
  ok <- inherits(x, "forest_stats")
  if (!ok || !isTRUE(strict)) return(ok)
  tryCatch({validate_forest_stats(x); TRUE}, error = function(e) FALSE)
}


#' Print method for `forest_stats`
#'
#' @description
#' Focused summary of a [df_forest()] result: object dimensions, the
#' configuration values (`metric_name_var`, `cov_name_var`, `cov_level_var`,
#' `ci`, `sigdigits`, `cov_name_ref`, `cov_level_ref`), and the first rows of
#' the stats frame. Inspect the full frame via `x$stats`; inspect run config
#' via `x$config`.
#'
#' @param x A `forest_stats` object.
#' @param n_head Number of rows of `x$stats` to preview. Default `3`; `0`
#'    omits the preview.
#' @param ... Passed to the underlying `print.data.frame()` call for the
#'    preview.
#'
#' @return `invisible(x)`.
#' @export
#' @method print forest_stats

print.forest_stats <- function(x, n_head = 3, ...) {
  forest_stats_header(x)
  if (n_head > 0 && nrow(x$stats) > 0) {
    cat(sprintf("\n  head(stats, %d):\n", n_head))
    print(utils::head(x$stats, n_head), ...)
  }
  cat("\n  Use `x$stats` for the full data.frame.\n")
  invisible(x)
}


forest_stats_header <- function(x) {
  pmx_stats_header(x, c("metric_name_var", "metric_value_var", "cov_name_var",
                        "cov_level_var", "ci", "sigdigits", "cov_name_ref",
                        "cov_level_ref"))
}


#' Summary method for `forest_stats`
#'
#' @description
#' Compact summary of a [df_forest()] result: the same header shown by
#' [print.forest_stats()], then one line per metric with its draw count `n`
#' and each covariate's levels in plot order. A covariate whose level repeats
#' its name (the reference panel) shows the name alone; reference copies
#' drawn in a covariate panel are marked `*`.
#'
#' @param object A `forest_stats` object.
#' @param ... Currently unused.
#'
#' @return `invisible(object)`.
#' @export
#' @method summary forest_stats

summary.forest_stats <- function(object, ...) {
  forest_stats_header(object)
  s <- object$stats
  metric <- as.character(s[[object$config$metric_name_var]])
  cov_name <- as.character(s[[object$config$cov_name_var]])
  cov_level <- as.character(s[[object$config$cov_level_var]])
  level_label <- ifelse(s$layout == "distributed", paste0(cov_level, "*"), cov_level)
  metric_line <- function(m) {
    in_m <- metric == m
    covs <- unique(cov_name[in_m])
    panels <- vapply(covs, function(cn) {
      k <- in_m & cov_name == cn
      if (all(cov_level[k] == cn)) return(cn)
      paste0(cn, ": ", paste(level_label[k], collapse = ", "))
    }, character(1))
    n_tag <- if (is.null(s$n)) "" else {
      n_rng <- unique(range(s$n[in_m]))
      paste0("  n = ", paste(n_rng, collapse = "-"))
    }
    sprintf("    [%s]%s  %s\n", m, n_tag, paste(panels, collapse = " | "))
  }
  cat("\n  per metric:\n")
  cat(vapply(unique(metric), metric_line, character(1)), sep = "")
  if (any(s$layout == "distributed")) {
    cat("  * reference copy in a covariate panel (ref_layout = \"distributed\")\n")
  }
  invisible(object)
}
