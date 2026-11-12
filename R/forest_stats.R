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
  data_cols <- names(data)
  metric_name_var_str <- resolve_var(rlang::enquo(metric_name_var), cols = data_cols)
  metric_value_var_str <- resolve_var(rlang::enquo(metric_value_var), cols = data_cols)
  cov_name_var_str <- resolve_var(rlang::enquo(cov_name_var), cols = data_cols)
  cov_level_var_str <- resolve_var(rlang::enquo(cov_level_var), cols = data_cols)

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
    pmx_abort("{cli::qty(has_na)}Column{?s} {.var {has_na}} of {.arg data} must not contain {.code NA}.",
              "bad_data")
  }

  if (!is.null(cov_level_ref) && is.null(cov_name_ref)) {
    pmx_abort("{.arg cov_level_ref} requires a non-NULL {.arg cov_name_ref} identifying the reference row to distribute.",
              "bad_arg")
  }

  grp_vars <- c(metric_name_var_str, cov_name_var_str, cov_level_var_str)
  display <- forest_input_order(data, metric_name_var_str, cov_name_var_str,
                                cov_level_var_str)
  stats <- forest_aggregate(data, grp_vars, metric_value_var_str, ci)
  ref <- forest_distribute_ref(stats, cov_name_ref, cov_level_ref,
                               display$cov_order, grp_vars)
  display$cov_order <- ref$cov_order
  stats <- forest_sort_rows(ref$stats, display, grp_vars)
  stats <- forest_format_labels(stats, sigdigits, metric_name_var_str)
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


#' Internal: display order read from the raw `df_forest()` input
#'
#' Read before `summarise()` sorts the groups.
#'
#' @param data Raw replicate draws.
#' @param metric_var,cov_var,level_var Column names.
#' @return `list(metric_order, cov_order, level_from_factor, level_given)`;
#'    `level_given` holds each covariate's labels in input order.
#' @noRd
forest_input_order <- function(data, metric_var, cov_var, level_var) {
  cov_order <- level_order(data[[cov_var]])
  cov_name_chr <- as.character(data[[cov_var]])
  level_from_factor <- is.factor(data[[level_var]])
  ## Full factor levels (not just those present) so a distributed reference
  ## label the factor declares still places by its level.
  level_given <- lapply(stats::setNames(nm = cov_order), function(cn) {
    x <- data[[level_var]][cov_name_chr == cn]
    if (level_from_factor) levels(x) else unique(as.character(x))
  })
  list(metric_order = level_order(data[[metric_var]]), cov_order = cov_order,
       level_from_factor = level_from_factor, level_given = level_given)
}


#' Internal: median and `ci` percentile interval per metric/covariate/level
#'
#' @param data Raw replicate draws.
#' @param grp_vars Metric, covariate, and level column names.
#' @param value_var Column of draws.
#' @param ci Central interval width.
#' @return A data.frame with character `grp_vars` and `est`, `lo`, `hi`, `n`.
#' @noRd
forest_aggregate <- function(data, grp_vars, value_var, ci) {
  probs <- c((1 - ci) / 2, 1 - (1 - ci) / 2)
  stats <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_vars))) |>
    dplyr::summarise(
      est = stats::median(.data[[value_var]], na.rm = TRUE),
      lo = unname(stats::quantile(.data[[value_var]], probs[1], na.rm = TRUE)),
      hi = unname(stats::quantile(.data[[value_var]], probs[2], na.rm = TRUE)),
      n = sum(!is.na(.data[[value_var]])),
      .groups = "drop"
    ) |>
    as.data.frame()
  for (v in grp_vars) stats[[v]] <- as.character(stats[[v]])
  stats
}


#' Internal: flag reference rows and distribute copies into covariate panels
#'
#' @param stats Aggregated rows from `forest_aggregate()`.
#' @param cov_name_ref,cov_level_ref As in [df_forest()].
#' @param cov_order Covariate display order.
#' @param grp_vars Metric, covariate, and level column names.
#' @param call The call reported in errors.
#' @return `list(stats, cov_order)`: rows gain `is_ref` and `layout` (plus any
#'    distributed copies), and the reference panel moves first in `cov_order`.
#' @noRd
forest_distribute_ref <- function(stats, cov_name_ref, cov_level_ref, cov_order,
                                  grp_vars, call = rlang::caller_env()) {
  metric_name_var_str <- grp_vars[1]
  cov_name_var_str <- grp_vars[2]
  cov_level_var_str <- grp_vars[3]

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
    pmx_abort("Cannot distribute the {.val {cov_name_ref}} row: no rows with {.code {cov_name_var_str} == \"{cov_name_ref}\"} were found in {.arg data}.",
              "bad_data", call = call)
  }
  unmatched <- setdiff(names(cov_level_ref), cov_order)
  if (length(unmatched) > 0) {
    pmx_warn("{.arg cov_level_ref} names {cli::qty(unmatched)}{?a covariate/covariates} not found in {.var {cov_name_var_str}}: {.val {unmatched}}. No reference row is distributed for {?it/them}.",
             "ignored_arg")
  }
  self_ref <- intersect(names(cov_level_ref), ref_covs)
  if (length(self_ref) > 0) {
    pmx_warn("{.arg cov_level_ref} names the reference row itself ({.val {self_ref}}); the entry is ignored.",
             "ignored_arg")
  }
  stats$layout <- ifelse(stats$is_ref, "consolidated", "both")
  ref_rows <- stats[stats$is_ref, , drop = FALSE]
  targets <- intersect(names(cov_level_ref), setdiff(cov_order, ref_covs))
  clash <- Filter(function(cn) {
    as.character(cov_level_ref[[cn]]) %in%
      stats[[cov_level_var_str]][stats[[cov_name_var_str]] == cn]
  }, targets)
  if (length(clash) > 0) {
    clash_labels <- paste0(clash, " = \"", cov_level_ref[clash], "\"")
    pmx_warn(c(
      "{.arg cov_level_ref} labels match an existing level of the same covariate: {clash_labels}.",
      "i" = "The distributed reference row and the test row share that label."
    ), "ignored_arg")
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
  list(stats = stats, cov_order = cov_order)
}


#' Internal: sort `df_forest()` rows into display order
#'
#' @param stats Rows from `forest_distribute_ref()`.
#' @param display Display order from `forest_input_order()`, with the
#'    reference-first `cov_order`.
#' @param grp_vars Metric, covariate, and level column names.
#' @return `stats`, reordered top to bottom with row names reset.
#' @noRd
forest_sort_rows <- function(stats, display, grp_vars) {
  metric_name_var_str <- grp_vars[1]
  cov_name_var_str <- grp_vars[2]
  cov_level_var_str <- grp_vars[3]

  ## Sort into display order (top to bottom): metric, covariate, then the
  ## per-panel label order from `order_col_factor()`. Factor levels win when
  ## they cover every label in the panel (so the caller controls placement,
  ## including the reference label); otherwise the panel's labels go in
  ## reference first, then data order, and `order_col_factor()` sorts them
  ## by value when every label carries a distinct number. Distributed reference
  ## labels are ordered with the covariate's own labels, so both layouts
  ## share one test-row order.
  level_rank <- integer(nrow(stats))
  for (cn in display$cov_order) {
    in_panel <- stats[[cov_name_var_str]] == cn
    labels <- stats[[cov_level_var_str]][in_panel]
    given <- display$level_given[[cn]]
    panel <- if (display$level_from_factor && all(labels %in% given)) {
      factor(labels, levels = given)
    } else {
      c(labels[stats$is_ref[in_panel]], given[given %in% labels])
    }
    level_rank[in_panel] <- match(labels, levels(order_col_factor(panel)))
  }
  stats <- stats[order(
    match(stats[[metric_name_var_str]], display$metric_order),
    match(stats[[cov_name_var_str]], display$cov_order),
    level_rank
  ), , drop = FALSE]
  rownames(stats) <- NULL
  stats
}


#' Internal: formatted estimate and interval labels for `df_forest()` rows
#'
#' @param stats Sorted rows with `est`, `lo`, `hi`.
#' @param sigdigits As in [df_forest()].
#' @param metric_var Metric column name.
#' @return `stats` with `est_label`, `lo_label`, `hi_label`, and `ci_label`.
#' @noRd
forest_format_labels <- function(stats, sigdigits, metric_var) {
  ## One decimal count per metric, set by `sigdigits` of its median estimate.
  anchor <- stats::ave(abs(stats$est), stats[[metric_var]],
                       FUN = function(x) stats::median(x, na.rm = TRUE))
  stats$est_label <- fmt_sig(stats$est, sigdigits, anchor = anchor)
  stats$lo_label <- fmt_sig(stats$lo, sigdigits, anchor = anchor)
  stats$hi_label <- fmt_sig(stats$hi, sigdigits, anchor = anchor)
  stats$ci_label <- paste0(
    stats$est_label, " (", stats$lo_label, ", ", stats$hi_label, ")"
  )
  stats$ci_label[is.na(stats$est)] <- NA_character_
  stats
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
#' @param arg,call The argument name and call reported in errors.
#'
#' @return `invisible(x)` on success.
#' @keywords internal

validate_forest_stats <- function(x, arg = rlang::caller_arg(x),
                                  call = rlang::caller_env()) {
  check_pmx_stats(
    x, "forest_stats", "df_forest",
    stats_cols = c("est", "lo", "hi", "est_label", "lo_label", "hi_label",
                   "ci_label", "is_ref", "layout"),
    config_keys = c("metric_name_var", "cov_name_var", "cov_level_var",
                    "ci", "sigdigits"),
    arg = arg, call = call
  )
  stats_name <- paste0(arg, "$stats")
  check_varsindf(
    x$stats,
    c(x$config$metric_name_var, x$config$cov_name_var, x$config$cov_level_var),
    stats_name,
    "config$metric_name_var/cov_name_var/cov_level_var",
    call = call
  )
  ## Column types and values the renderer relies on, since users may edit
  ## `stats$stats` directly. Estimates and labels may be NA (all-NA draws).
  s <- x$stats
  bad_type <- function(cols, pred, what) {
    bad <- cols[!vapply(s[cols], pred, logical(1))]
    if (length(bad) > 0) {
      pmx_abort("{.code {stats_name}} {cli::qty(bad)}column{?s} {.var {bad}} must be {what}.",
                "bad_stats", call = call)
    }
  }
  bad_type(c("est", "lo", "hi"), is.numeric, "numeric")
  bad_type(c("est_label", "lo_label", "hi_label", "ci_label"), is.character, "character")
  if (!is.logical(s$is_ref) || anyNA(s$is_ref)) {
    pmx_abort("{.code {stats_name}$is_ref} must be logical with no {.code NA}.", "bad_stats",
              call = call)
  }
  bad_layout <- setdiff(unique(s$layout), c("both", "consolidated", "distributed"))
  if (length(bad_layout) > 0) {
    pmx_abort("{.code {stats_name}$layout} must be {.val both}, {.val consolidated}, or {.val distributed}; found {.val {bad_layout}}.",
              "bad_stats", call = call)
  }
  check_quantile_scalar(x$config$ci, paste0(arg, "$config$ci"), call = call)
  check_positive_integer(x$config$sigdigits, paste0(arg, "$config$sigdigits"), call = call)
  invisible(x)
}


#' @rdname validate_pmx_stats
#' @export
validate_pmx_stats.forest_stats <- function(x, ..., arg = rlang::caller_arg(x),
                                            call = rlang::caller_env()) {
  validate_forest_stats(x, arg = arg, call = call)
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
#'    [validate_pmx_stats()] and returns `FALSE` on validation failure.
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
  cat_summary_header(forest_stats_header(x))
  if (n_head > 0 && nrow(x$stats) > 0) {
    cat(sprintf("\n  head(stats, %d):\n", n_head))
    print(utils::head(x$stats, n_head), ...)
  }
  cat("\n  Use `x$stats` for the full data.frame.\n")
  invisible(x)
}


forest_stats_header <- function(x) {
  pmx_summary_header(x, c("metric_name_var", "metric_value_var", "cov_name_var",
                          "cov_level_var", "ci", "sigdigits", "cov_name_ref",
                          "cov_level_ref"))
}


#' Summary method for `forest_stats`
#'
#' @description
#' Compact summary of a [df_forest()] result. Printing it shows the same
#' header as [print.forest_stats()], then one line per metric with its draw
#' count `n` and each covariate's levels in plot order. A covariate whose
#' level repeats its name (the reference panel) shows the name alone;
#' reference copies drawn in a covariate panel are marked `*`.
#'
#' @param object A `forest_stats` object.
#' @param x A `summary.forest_stats` object.
#' @param ... Currently unused.
#'
#' @return A `summary.forest_stats` object (also `summary.pmx_stats`): a list
#'    with `header` (the class tag, `dim` of `stats`, `obs_rows`, and the
#'    `config` values shown by [print.forest_stats()]), `metrics` (a
#'    data.frame with one row per metric and columns `metric`, `n_min` and
#'    `n_max` (the range of `n`, `NA` when `stats` has no `n` column), and
#'    `panels`, the covariate panels and levels in plot order as printed), and
#'    `has_distributed` (whether any reference copy sits in a covariate
#'    panel).
#' @export
#' @method summary forest_stats

summary.forest_stats <- function(object, ...) {
  s <- object$stats
  metric <- as.character(s[[object$config$metric_name_var]])
  cov_name <- as.character(s[[object$config$cov_name_var]])
  cov_level <- as.character(s[[object$config$cov_level_var]])
  level_label <- ifelse(s$layout == "distributed", paste0(cov_level, "*"), cov_level)
  panels <- function(m) {
    in_m <- metric == m
    covs <- unique(cov_name[in_m])
    lines <- vapply(covs, function(cn) {
      k <- in_m & cov_name == cn
      if (all(cov_level[k] == cn)) return(cn)
      paste0(cn, ": ", paste(level_label[k], collapse = ", "))
    }, character(1))
    paste(lines, collapse = " | ")
  }
  n_range <- function(m) {
    if (is.null(s$n)) return(c(NA_integer_, NA_integer_))
    as.integer(range(s$n[metric == m]))
  }
  metrics_u <- unique(metric)
  n_rng <- vapply(metrics_u, n_range, integer(2))
  structure(
    list(
      header = forest_stats_header(object),
      metrics = data.frame(metric = metrics_u, n_min = n_rng[1, ], n_max = n_rng[2, ],
                           panels = vapply(metrics_u, panels, character(1)),
                           row.names = NULL),
      has_distributed = any(s$layout == "distributed")
    ),
    class = c("summary.forest_stats", "summary.pmx_stats")
  )
}


#' @rdname summary.forest_stats
#' @export
print.summary.forest_stats <- function(x, ...) {
  cat_summary_header(x$header)
  m <- x$metrics
  n_tag <- ifelse(is.na(m$n_min), "",
                  ifelse(m$n_min == m$n_max, paste0("  n = ", m$n_min),
                         paste0("  n = ", m$n_min, "-", m$n_max)))
  cat("\n  per metric:\n")
  cat(sprintf("    [%s]%s  %s\n", m$metric, n_tag, m$panels), sep = "")
  if (isTRUE(x$has_distributed)) {
    cat("  * reference copy in a covariate panel (ref_layout = \"distributed\")\n")
  }
  invisible(x)
}
