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


#' Internal helper: top-to-bottom order of the rows within one forest panel
#'
#' @param labels Character; the unique `cov_level` labels in the panel.
#' @param is_ref Logical the length of `labels`; `TRUE` for the distributed
#'    reference label.
#' @param given Character; the order the input data declared for this
#'    covariate (factor levels, or first appearance), from [level_order()].
#' @param from_factor Logical; whether `given` came from factor levels.
#'
#' @return `labels` reordered top to bottom. Factor levels win when they
#'    cover every label (so the caller controls placement, including the
#'    reference label). Otherwise labels carrying a number sort ascending by
#'    it, sign included (so e.g. `50 kg` sits above `90 kg` and a `70 kg`
#'    reference lands between them, and `-20%` sits above `-5%`); any other
#'    set keeps the reference label on top followed by the data order.
#' @keywords internal
order_panel_labels <- function(labels, is_ref, given, from_factor) {
  if (from_factor && all(labels %in% given)) {
    return(labels[order(match(labels, given))])
  }
  ## Sort key shared with `order_col_factor()` so the two cannot drift apart;
  ## it handles the minus-vs-hyphen distinction. The tie-break below stays
  ## here: `given` can differ from `labels` once a distributed reference label
  ## has been synthesized, so `match()` is not interchangeable with
  ## `seq_along()` the way it is in `order_col_factor()`.
  nums <- label_sort_num(labels)
  if (all(!is.na(nums))) {
    return(labels[order(nums, match(labels, given))])
  }
  c(labels[is_ref], labels[!is_ref][order(match(labels[!is_ref], given))])
}


#' Internal helper: collapse a reference flag column onto aggregated groups
#'
#' Reduces the per-draw reference flag in `data` to one value per
#' `(metric, covariate, level)` group, aligned to the rows of the aggregated
#' `stats` frame. The flag must be constant within a group; a group that
#' mixes flagged and unflagged draws is a caller error, not something to
#' silently resolve.
#'
#' @param data Input draws data.frame carrying the flag column.
#' @param stats Aggregated stats frame, group columns already character.
#' @param grp_vars Character vector of grouping column names.
#' @param is_ref_var_str Name of the flag column in `data`.
#'
#' @return Logical vector the length of `nrow(stats)`.
#' @keywords internal
aggregate_ref_flag <- function(data, stats, grp_vars, is_ref_var_str) {
  flag <- data[[is_ref_var_str]]
  if (is.factor(flag)) flag <- as.character(flag)
  if (!is.logical(flag)) flag <- as.logical(flag)
  if (any(is.na(flag))) {
    rlang::abort(message = paste0(
      "reference flag column `", is_ref_var_str,
      "` must be logical (or coercible to logical) with no `NA` values."
    ))
  }
  group_key <- function(df) {
    do.call(
      paste,
      c(lapply(grp_vars, function(v) as.character(df[[v]])), list(sep = "\r"))
    )
  }
  key_data <- group_key(data)
  any_by <- tapply(flag, key_data, any)
  all_by <- tapply(flag, key_data, all)
  mixed <- names(any_by)[any_by != all_by]
  if (length(mixed) > 0) {
    rlang::abort(message = paste0(
      "reference flag column `", is_ref_var_str, "` is not constant within ",
      paste(grp_vars, collapse = "/"), " groups: ",
      paste(gsub("\r", " / ", mixed[seq_len(min(5L, length(mixed)))], fixed = TRUE), collapse = "; "),
      ". Every draw in a group must share the same flag value."
    ))
  }
  ## `tapply()` returns a 1-d array; strip it back to a plain logical vector
  ## so the flag path and the string path produce identical `stats` frames.
  as.vector(any_by[group_key(stats)])
}


#' Internal helper: assemble the forest estimate column
#'
#' Joins the formatted pieces as `"est  (lo, hi)"`.
#'
#' @param est_label,lo_label,hi_label Character vectors of formatted values.
#' @param is_ref Logical; reference rows show the estimate alone, since
#'    their interval collapses onto the estimate.
#'
#' @return Character vector of display labels.
#' @keywords internal
forest_col_labels <- function(est_label, lo_label, hi_label, is_ref) {
  ifelse(is_ref, est_label, paste0(est_label, "  (", lo_label, ", ", hi_label, ")"))
}


#' Internal helper: the rows a forest plot draws, in draw order
#'
#' Returns `stats$stats` (optionally restricted to one metric) laid out for
#' the requested reference layout. `"consolidated"` draws the rows as they
#' are; `"distributed"` replaces the standalone reference panel(s) with a
#' copy of the reference row in each covariate panel named in
#' `config$cov_level_ref`, under that label, directly below the
#' `config$ref_after` label (top of the panel when `NA` or absent).
#'
#' @param stats A `forest_stats` object.
#' @param ref_layout `NULL`, `"distributed"`, or `"consolidated"`. `NULL`
#'    resolves to `"distributed"` when `config$cov_level_ref` is set,
#'    otherwise `"consolidated"`.
#' @param metric Optional metric value to restrict the rows to.
#'
#' @return A data.frame of rows in top-to-bottom draw order.
#' @keywords internal
forest_rows <- function(stats, ref_layout = NULL, metric = NULL) {
  cfg <- stats$config
  rows <- stats$stats
  if (!is.null(metric)) {
    rows <- rows[as.character(rows[[cfg$metric_name_var]]) == metric, , drop = FALSE]
  }
  if (is.null(ref_layout)) {
    ref_layout <- if (is.null(cfg$cov_level_ref)) "consolidated" else "distributed"
  }
  if (ref_layout == "distributed") {
    if (is.null(cfg$cov_level_ref)) {
      rlang::abort(message = paste0(
        "`ref_layout = \"distributed\"` needs per-covariate reference labels; ",
        "pass `cov_level_ref` to `df_forest()`."
      ))
    }
    metric_chr <- as.character(rows[[cfg$metric_name_var]])
    by_metric <- split(rows, factor(metric_chr, levels = unique(metric_chr)))
    by_metric <- lapply(by_metric, distribute_ref_rows, cfg = cfg)
    if (!any(vapply(by_metric, function(x) isTRUE(attr(x, "distributed")), logical(1)))) {
      rlang::abort(message = paste0(
        "`ref_layout = \"distributed\"` found no standalone reference panel ",
        "(a covariate whose rows are all flagged `is_ref`) to distribute."
      ))
    }
    rows <- do.call(rbind, by_metric)
  }
  rownames(rows) <- NULL
  rows
}


#' Internal helper: distribute one metric's reference panel
#'
#' @param rows One metric's rows of `stats$stats`, in draw order.
#' @param cfg The `forest_stats` config list.
#'
#' @return `rows` with the standalone reference panel(s) replaced by copies
#'    in each `cfg$cov_level_ref` covariate, carrying a `"distributed"`
#'    attribute (`TRUE` when a reference panel was found).
#' @keywords internal
distribute_ref_rows <- function(rows, cfg) {
  cov_name_var_str <- cfg$cov_name_var
  cov_level_var_str <- cfg$cov_level_var
  cov <- as.character(rows[[cov_name_var_str]])
  ref_covs <- Filter(
    function(cn) all(rows$is_ref[cov == cn]),
    unique(cov[rows$is_ref])
  )
  if (length(ref_covs) == 0L) return(rows)

  ref_rows <- rows[cov %in% ref_covs, , drop = FALSE]
  rows <- rows[!cov %in% ref_covs, , drop = FALSE]
  cov <- cov[!cov %in% ref_covs]
  for (cn in intersect(names(cfg$cov_level_ref), unique(cov))) {
    copy <- ref_rows
    copy[[cov_name_var_str]] <- cn
    copy[[cov_level_var_str]] <- as.character(cfg$cov_level_ref[[cn]])
    in_panel <- which(as.character(rows[[cov_name_var_str]]) == cn)
    after <- unname(cfg$ref_after[cn])
    hit <- if (length(after) == 1L && !is.na(after)) {
      in_panel[as.character(rows[[cov_level_var_str]][in_panel]) == after]
    }
    pos <- if (length(hit) > 0L) max(hit) else min(in_panel) - 1L
    idx <- seq_len(nrow(rows))
    rows <- rbind(rows[idx <= pos, , drop = FALSE], copy, rows[idx > pos, , drop = FALSE])
  }
  attr(rows, "distributed") <- TRUE
  rows
}


#' Compute Summary Statistics for Covariate Forest Plots
#'
#' @description
#' Aggregates a dataset with replicate level data into a cacheable, replottable
#' container suitable for a covariate forest plots. Returns a [pmx_stats()]
#' container with class `c("forest_stats", "pmx_stats")` and three slots:
#'
#'   + `stats` (plot-ready frame: one row per plotted point, in display order,
#'      with the reference in its own panel, canonical `est`, `lo`, `hi`, the formatted `est_label`,
#'      `lo_label`, `hi_label`, and `ci_label`, and `is_ref` columns),
#'   + `obs` (always `NULL`; no scatter overlay),
#'   + `config` (column names + CI configuration)
#'
#' The rows are sorted into the order [plot_build_forest()] draws them, top
#' to bottom; reorder the rows of `stats$stats` to change the plotted order.
#' The reference row stays in its own panel at the top of `stats$stats`. When
#' `cov_level_ref` is supplied, [plot_build_forest()] distributes it into each
#' covariate panel at plot time (switch with its `ref_layout` argument), so
#' one container renders either layout without re-aggregating.
#'
#' @param data Input data.frame. Replicate level long format dataset: one row
#'    per metric, covariate, covariate level, and replicate.
#' @param metric_name_var Column in `data` contain metric variable names.
#'    Accepts bare names or strings. Default is `metric`.
#' @param cov_name_var Column in `data` containing covariate variable names.
#'    (e.g., `WTBL`,`FOOD`, etc).
#'    Accepts bare names or strings. Default is `cov_var`.
#' @param cov_level_var Column in `data` containing covariate levels (e.g., `70 kg`).
#'    Accepts bare names or strings. Default is `cov_val`.
#' @param metric_value_var Column in `data` containing replicate draws of the
#'    test/reference comparison statistic.
#'    Accepts bare names or strings. Default `value`.
#' @param ci Numeric scalar in `(0, 1)` specifying the central interval
#'    width (e.g., `0.9` for 90% CI).
#' @param sigdigits Number of significant digits used to format the label
#'    columns. Digits are counted on the median estimate of each metric,
#'    which fixes one count of decimal places for every value of that metric
#'    (`"1.00"`, `"0.95"`, `"2.32"` for a ratio; `"12500"`, `"9500"` for an
#'    exposure near 12500), so the plotted estimate column aligns on the
#'    decimal point. Trailing zeros are kept. Default `3`.
#' @param cov_name_ref Character string identifying the reference row(s) in the
#'    `cov_name_var` column. Default is `"Reference"` (matches the bundled
#'    `data_sad_pkforest` convention). Reference rows draw through the
#'    `ref_point` style role and sort to the top of `stats$stats`
#'    (standalone panel); they can be distributed into each covariate panel
#'    at plot time (see `cov_level_ref`). `NULL` marks no row as the reference.
#'    Consulted only when no reference flag column is present (see
#'    `is_ref_var`), so `cov_name_ref = NULL` suppresses the reference row
#'    on the string path alone; pass `is_ref_var = NULL` as well to ignore a
#'    flag column.
#'    Not to be confused with `ref` in [plot_build_forest()] / [plot_forest()],
#'    which is the numeric value of the x-intercept for the vertical reference line.
#' @param is_ref_var Column in `data` holding a logical reference flag, one
#'    value per draw, marking the rows of the reference scenario. Accepts
#'    bare names or strings; `NULL` disables the flag path. Default
#'    `is_ref`, the column [df_forest_preprocess()] emits. When the named
#'    column is present it identifies the reference rows and `cov_name_ref`
#'    is not consulted; when the default name is absent the `cov_name_ref`
#'    string match is used instead; a non-default name that is absent is an
#'    error. The flag must be constant within a
#'    `(metric, covariate, level)` group. A covariate whose rows are all
#'    flagged is a standalone reference panel, the flag-path equivalent of
#'    `cov_name_ref`.
#' @param cov_level_ref Optional named vector mapping `cov_name_var`
#'    values to the per-covariate reference label (e.g.
#'    `c(FOOD = "Fasted", WTBL = "70 kg")`). Enables the distributed
#'    reference layout and makes it the default in [plot_build_forest()] /
#'    [plot_forest()]: the reference row of each metric is drawn in every
#'    named covariate that is present in `data`, under that label, in place
#'    of the standalone reference panel (`ref_layout = "consolidated"` draws
#'    the standalone panel instead). Covariates not named show no reference
#'    row. `stats$stats` itself always keeps the standalone reference rows.
#'    Requires a reference row, identified by the flag column or a
#'    non-`NULL` `cov_name_ref`. Default is `NULL` (the reference renders in
#'    its own panel at the top).
#'
#' @section Row order:
#' Rows of `stats$stats` are sorted into display order, top to bottom:
#' metrics and covariates follow their factor levels when the input columns
#' are factors, otherwise their order of first appearance in `data`, with the
#' standalone reference covariate moved first. Within a covariate, factor
#' levels of `cov_level_var` win when they cover every label (including the
#' distributed reference label); otherwise labels carrying a number sort ascending by it,
#' sign included (`50 kg` above `90 kg`), and any other set puts the reference
#' label on top followed by the data order. The distributed reference label is
#' placed by the same rules and its position recorded in `config$ref_after`,
#' so the covariate rows keep one order in both layouts.
#'
#' @family forest plot
#' @return A `forest_stats` container (subclass of `pmx_stats`) with three
#'    slots:
#'    \describe{
#'      \item{`stats`}{One row per plotted point, in display order, with
#'        character columns named in `metric_name_var` / `cov_name_var` /
#'        `cov_level_var`, canonical numeric columns `est` (median of the
#'        draws), `lo`, `hi` (the `(1 - ci) / 2` and `1 - (1 - ci) / 2`
#'        percentiles), the character columns `est_label`, `lo_label`, and
#'        `hi_label` (those values formatted per `sigdigits`), `ci_label`
#'        (the three combined for tables, e.g. `"1.12 (0.95, 1.32)"`), and
#'        the logical `is_ref`. The `cov_name_var` value drives the per-row
#'        facet strip; the `cov_level_var` value drives the y-axis tick label;
#'        [plot_build_forest()] assembles `est_label`, `lo_label`, and
#'        `hi_label` into the right-hand secondary y-axis text, so edit those
#'        columns (not `ci_label`) to change the plotted text.}
#'      \item{`obs`}{`NULL`. No scatter overlay for the forest family.}
#'      \item{`config`}{Named list with column names, `ci`,
#'        `sigdigits`, `cov_name_ref`, `cov_level_ref`, and `ref_after` (a
#'        named character vector giving, per `cov_level_ref` covariate, the
#'        label the distributed reference row is drawn directly below; `NA`
#'        for the top of the panel).}
#'    }
#'    Pass directly to [plot_forest()] or [plot_build_forest()] to render.
#' @export df_forest
#'
#' @examples
#' # Aggregate replicate simulations to point estimate + 90% CI
#' df_forest(dplyr::filter(data_sad_pkforest, grepl("RATIO$", metric)))
#'
#' # Custom reference level (default is "Reference")
#' df_forest(data_sad_pkforest, cov_name_ref = "Reference")
#'
#' # Per-covariate reference labels (Reference distributed into each panel
#' # at plot time)
#' df_forest(
#'   dplyr::filter(data_sad_pkforest, metric == "AUCRATIO"),
#'   cov_level_ref = c(FOOD = "Fasted", WTBL = "70 kg")
#' )
#'
#' # Control the row order with factor levels on the input
#' d <- dplyr::filter(data_sad_pkforest, metric == "AUCRATIO")
#' d$cov_val <- factor(d$cov_val, levels = c("Reference", "Fed", "Fasted",
#'                                           "50 kg", "70 kg", "90 kg"))
#' df_forest(d, cov_level_ref = c(FOOD = "Fasted", WTBL = "70 kg"))

df_forest <- function(
  data,
  metric_name_var = "metric",
  cov_name_var = "cov_var",
  cov_level_var = "cov_val",
  metric_value_var = "value",
  ci = 0.9,
  sigdigits = 3,
  cov_name_ref = "Reference",
  cov_level_ref = NULL,
  is_ref_var = "is_ref"
) {
  metric_name_var_str <- resolve_var(rlang::enquo(metric_name_var))
  cov_name_var_str <- resolve_var(rlang::enquo(cov_name_var))
  cov_level_var_str <- resolve_var(rlang::enquo(cov_level_var))
  metric_value_var_str <- resolve_var(rlang::enquo(metric_value_var))
  is_ref_var_str <- resolve_var(rlang::enquo(is_ref_var), nullable = TRUE)

  check_df(data, "data")
  check_forest_args(ci, sigdigits, cov_name_ref)
  check_cov_level_ref(cov_level_ref)
  check_varsindf(
    data,
    c(metric_name_var_str, cov_name_var_str, cov_level_var_str, metric_value_var_str),
    "data",
    "metric_name_var/cov_name_var/cov_level_var/metric_value_var"
  )

  ## A reference flag column in `data` takes precedence over the
  ## `cov_name_ref` string match. The default name falls back silently when
  ## absent; an explicitly named column that is missing is an error.
  use_flag <- FALSE
  if (!is.null(is_ref_var_str)) {
    if (is_ref_var_str %in% colnames(data)) {
      use_flag <- TRUE
    } else if (!identical(is_ref_var_str, "is_ref")) {
      check_varsindf(data, is_ref_var_str, "data", "is_ref_var")
    }
  }
  if (!is.null(cov_level_ref) && is.null(cov_name_ref) && !use_flag) {
    rlang::abort(message = paste0(
      "argument `cov_level_ref` requires a non-NULL `cov_name_ref`, or a ",
      "reference flag column named by `is_ref_var`, identifying the ",
      "reference row to distribute."
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
      .groups = "drop"
    ) |>
    as.data.frame()
  for (v in grp_vars) stats[[v]] <- as.character(stats[[v]])

  ## Reference rows: identified by the flag column when `data` carries one,
  ## otherwise by matching `cov_name_ref`. They stay in their own panel(s),
  ## sorted first; distributing them into each covariate panel under its
  ## `cov_level_ref` label happens at plot time (see `forest_rows()`).
  stats$is_ref <- FALSE
  ref_covs <- character(0)
  if (use_flag) {
    stats$is_ref <- aggregate_ref_flag(data, stats, grp_vars, is_ref_var_str)
    ## A covariate whose rows are *all* reference rows is a standalone
    ## reference panel, the flag-path equivalent of `cov_name_ref`.
    ref_covs <- Filter(
      function(cn) all(stats$is_ref[stats[[cov_name_var_str]] == cn]),
      unique(stats[[cov_name_var_str]][stats$is_ref])
    )
    ref_covs <- as.character(ref_covs)
  } else if (!is.null(cov_name_ref)) {
    stats$is_ref <- stats[[cov_name_var_str]] == cov_name_ref
    ref_covs <- intersect(cov_name_ref, stats[[cov_name_var_str]])
  }
  if (!is.null(cov_level_ref) && length(ref_covs) == 0L) {
    rlang::abort(message = if (use_flag) paste0(
      "cannot distribute the reference row: no covariate in `",
      cov_name_var_str, "` is made up entirely of rows flagged by `",
      is_ref_var_str, "`."
    ) else paste0(
      "cannot distribute `", cov_name_ref, "` row: no rows with `",
      cov_name_var_str, " == \"", cov_name_ref, "\"` were found in `data`."
    ))
  }
  cov_order <- c(intersect(ref_covs, cov_order), setdiff(cov_order, ref_covs))

  ## Sort into display order (top to bottom): metric, covariate, then the
  ## per-panel label order. A covariate named in `cov_level_ref` is ordered
  ## with its reference label included, and the label the distributed
  ## reference row sits directly below is recorded in `ref_after` (`NA` for
  ## the top of the panel), so both layouts share one test-row order.
  targets <- intersect(names(cov_level_ref), setdiff(cov_order, ref_covs))
  ref_after <- stats::setNames(rep(NA_character_, length(targets)), targets)
  level_rank <- integer(nrow(stats))
  for (cn in cov_order) {
    in_panel <- stats[[cov_name_var_str]] == cn
    labels <- unique(stats[[cov_level_var_str]][in_panel])
    is_ref_label <- labels %in% stats[[cov_level_var_str]][in_panel & stats$is_ref]
    if (cn %in% targets) {
      ref_label <- as.character(cov_level_ref[[cn]])
      is_ref_label <- is_ref_label[labels != ref_label]
      labels <- c(labels[labels != ref_label], ref_label)
      is_ref_label <- c(is_ref_label, TRUE)
    }
    ordered <- order_panel_labels(
      labels,
      is_ref = is_ref_label,
      given = level_given[[cn]],
      from_factor = level_from_factor
    )
    if (cn %in% targets) {
      pos <- match(ref_label, ordered)
      if (pos > 1L) ref_after[[cn]] <- ordered[pos - 1L]
    }
    level_rank[in_panel] <- match(stats[[cov_level_var_str]][in_panel], ordered)
  }
  stats <- stats[order(
    match(stats[[metric_name_var_str]], metric_order),
    match(stats[[cov_name_var_str]], cov_order),
    level_rank
  ), , drop = FALSE]
  rownames(stats) <- NULL

  ## One decimal count per metric, set by `sigdigits` of its median estimate,
  ## so the plotted estimate column aligns on the decimal point.
  anchor <- stats::ave(abs(stats$est), stats[[metric_name_var_str]], FUN = stats::median)
  stats$est_label <- fmt_sig(stats$est, sigdigits, anchor = anchor)
  stats$lo_label <- fmt_sig(stats$lo, sigdigits, anchor = anchor)
  stats$hi_label <- fmt_sig(stats$hi, sigdigits, anchor = anchor)
  stats$ci_label <- paste0(
    stats$est_label, " (", stats$lo_label, ", ", stats$hi_label, ")"
  )
  stats <- stats[, c(grp_vars, "est", "lo", "hi", "est_label", "lo_label",
                     "hi_label", "ci_label", "is_ref")]

  pmx_stats(
    stats = stats,
    obs = NULL,
    config = list(
      metric_name_var = metric_name_var_str,
      cov_name_var = cov_name_var_str,
      cov_level_var = cov_level_var_str,
      metric_value_var = metric_value_var_str,
      ci = ci,
      sigdigits = sigdigits,
      cov_name_ref = cov_name_ref,
      cov_level_ref = cov_level_ref,
      ref_after = if (!is.null(cov_level_ref)) ref_after,
      is_ref_var = if (use_flag) is_ref_var_str else NULL
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
  if (!inherits(x, "forest_stats")) {
    rlang::abort(
      "`x` must be a `forest_stats` object (output of `df_forest()`)."
    )
  }
  validate_pmx_stats(x)
  required_cols <- c("est", "lo", "hi", "est_label", "lo_label", "hi_label",
                     "ci_label", "is_ref")
  missing_cols <- setdiff(required_cols, colnames(x$stats))
  if (length(missing_cols) > 0) {
    rlang::abort(paste0(
      "`forest_stats` is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    ))
  }
  required_config <- c(
    "metric_name_var", "cov_name_var", "cov_level_var",
    "ci", "sigdigits"
  )
  missing_config <- setdiff(required_config, names(x$config))
  if (length(missing_config) > 0) {
    rlang::abort(paste0(
      "`forest_stats` is missing required config keys: ",
      paste(missing_config, collapse = ", ")
    ))
  }
  check_varsindf(
    x$stats,
    c(x$config$metric_name_var, x$config$cov_name_var, x$config$cov_level_var),
    "stats",
    "config$metric_name_var/cov_name_var/cov_level_var"
  )
  invisible(x)
}


#' Build a forest plot ggplot from a `forest_stats` object
#'
#' @description
#' Constructs a covariate forest plot from a [df_forest()] result (or any
#' object satisfying the `forest_stats` contract). Renders per-row point
#' estimates with horizontal CI lines, optional shaded equivalence band, and
#' a vertical reference line, one facet row per covariate name. The
#' `est_label`, `lo_label`, and `hi_label` columns are assembled into the
#' right-hand secondary y-axis text (`"1.29  (1.19, 1.41)"`) under a header
#' built from the container's `ci` (e.g. `"Estimate (90% CI)"`), so each
#' row's estimate sits beside its point. The header is the plot subtitle,
#' right-aligned so it sits on the top edge of the panels above the column;
#' the rendered metric labels the x-axis, and the plot has no title by
#' default.
#'
#' Rows are drawn in the order they appear in `stats$stats`, top to bottom
#' (see the "Row order" section of [df_forest()]); reorder that frame to
#' change the plot. Each row is its own y position, so covariate level
#' labels may repeat across panels (`Low` / `High` under two covariates).
#' The one exception is the reference row in the distributed layout (see
#' `ref_layout`): it is inserted into each covariate panel at draw time,
#' below the label recorded in `config$ref_after`.
#'
#' Rows flagged `is_ref` (the distributed per-covariate reference label, or the
#' standalone `cov_name_ref` panel) draw through the `ref_point` style role
#' (an open circle by default) and show the point estimate alone in the
#' estimate column, since their CI collapses onto the estimate. A row whose
#' level label repeats its covariate name (the standalone reference panel)
#' gets an empty tick label, since the strip already carries the name.
#'
#' Returns a plain `ggplot` object so callers can compose additional layers,
#' themes, and scales with the standard `+` operator.
#'
#' Most users will reach this function indirectly via [plot_forest()].
#' Call `plot_build_forest()` directly when working from a manually-
#' constructed or cached `forest_stats` object — for example, plotting a
#' precomputed result from disk or a custom pipeline that produces
#' compatible columns and config.
#'
#' @param stats A `forest_stats` object (typically the output of
#'    [df_forest()]). Validated by [validate_forest_stats()] at entry.
#' @param style A [ggstylekit::style_spec()] controlling plot aesthetics.
#'    Defaults to [style_forest()]; view the defaults by running
#'    `style_forest()` with no arguments. Customize by passing
#'    `style = style_forest(...)`, or restyle the returned plot with
#'    [restyle_plot()]. Series roles are `point`, `errorbar`, `ref_point`,
#'    `ref_line`, and `ref_band`; `colors` entries keyed by covariate name
#'    opt into per-covariate coloring (see [style_forest()]). The estimate
#'    column is axis text styled through the theme's `axis.text.y.right`
#'    element; its header is the plot subtitle (`plot.subtitle`).
#' @param ref The x-intercept of the vertical reference line: a numeric
#'    scalar, `NULL` to suppress the line, or `"reference"` (default) to use
#'    the estimate of the metric's reference row (the row flagged `is_ref`;
#'    exactly `1` for a ratio metric, the reference exposure for a raw
#'    metric). With `"reference"` and no reference row in the plotted
#'    metric, a warning is issued and no line is drawn. Distinct from
#'    `cov_name_ref` in [df_forest()], which identifies the reference row.
#' @param ref_band Length-2 numeric vector giving the shaded equivalence
#'    interval, read according to `ref_band_type`. Default `NULL`
#'    (no shaded band).
#' @param ref_band_type How `ref_band` is placed around the resolved `ref`:
#'    `"multiplicative"` (default) draws `ref * ref_band`, so `c(0.8, 1.25)`
#'    is the bioequivalence band on a ratio or a raw exposure scale;
#'    `"additive"` draws `ref + ref_band`, so `c(-10, 10)` brackets a
#'    reference change of zero for a difference metric such as percent
#'    change from baseline; `"absolute"` draws `ref_band` as given. The
#'    first two require a resolved `ref`; multiplicative limits must be
#'    positive.
#' @param metric Character scalar identifying which value of `metric_name_var` to
#'    render. `plot_forest()` renders one metric per call (no metric
#'    facet); the covariate name is the only facet dimension. If `NULL`
#'    (default) and `stats$stats` contains exactly one unique value of
#'    `metric_name_var`, that value is used. If `NULL` and multiple metrics
#'    are present, aborts and asks the caller to pick one.
#' @param ref_layout Where the reference row is drawn: `"distributed"`
#'    copies it into each covariate panel named in the container's
#'    `cov_level_ref`, under that covariate's reference label;
#'    `"consolidated"` draws it in its own panel at the top. `NULL`
#'    (default) uses `"distributed"` when [df_forest()] was given
#'    `cov_level_ref`, otherwise `"consolidated"`. `"distributed"` without
#'    `cov_level_ref` is an error. The summary statistics are not
#'    recomputed; only the drawn rows change.
#'
#' @family forest plot
#' @return a `ggplot` plot object
#' @export plot_build_forest
#'
#' @examples
#' stats <- df_forest(
#'   data_sad_pkforest,
#'   cov_level_ref = c(FOOD = "Fasted", WTBL = "70 kg")
#' )
#' plot_build_forest(stats, metric = "AUCRATIO")
#' plot_build_forest(stats, metric = "AUCRATIO", ref_band = c(0.8, 1.25))
#' # The same stats with the reference in its own panel
#' plot_build_forest(stats, metric = "AUCRATIO", ref_layout = "consolidated")
#' # Raw exposure: the line sits at the reference exposure and the band is
#' # 0.8 to 1.25 times it
#' plot_build_forest(stats, metric = "AUC", ref_band = c(0.8, 1.25))
#' # Difference metric: an additive band around the reference estimate
#' plot_build_forest(stats, metric = "AUC", ref_band = c(-2000, 2000),
#'                   ref_band_type = "additive")
#' # Per-covariate coloring via the style's `colors` map
#' plot_build_forest(
#'   stats, metric = "AUCRATIO",
#'   style = style_forest(colors = c(FOOD = "darkgreen", WTBL = "darkblue"))
#' )
#' # Log x-axis: through the style, or compose with ggplot2 directly
#' plot_build_forest(stats, metric = "AUCRATIO", style = style_forest(logx = TRUE))
#' plot_build_forest(stats, metric = "AUCRATIO") +
#'   ggplot2::scale_x_log10(guide = "axis_logticks")

plot_build_forest <- function(
  stats,
  style = NULL,
  ref = "reference",
  ref_band = NULL,
  ref_band_type = c("multiplicative", "additive", "absolute"),
  metric = NULL,
  ref_layout = NULL
) {
  validate_forest_stats(stats)
  check_ref(ref)
  check_ref_layout(ref_layout)
  ref_band_type <- match.arg(ref_band_type)
  check_ref_band(ref_band, ref_band_type)
  plotstyle <- resolve_style(style, style_forest)

  metric_name_var_str <- stats$config$metric_name_var
  cov_name_var_str <- stats$config$cov_name_var
  cov_level_var_str <- stats$config$cov_level_var

  metric_col <- as.character(stats$stats[[metric_name_var_str]])
  metric <- check_forest_metric(metric, unique(metric_col), metric_name_var_str)
  plot_data <- forest_rows(stats, ref_layout, metric)

  ## `ref = "reference"` resolves to the metric's reference-row estimate (1
  ## for a ratio, the reference exposure for a raw metric); the band is then
  ## placed around that value according to `ref_band_type`.
  if (identical(ref, "reference")) {
    ref <- unique(plot_data$est[plot_data$is_ref])
    if (length(ref) == 0L) {
      rlang::warn(paste0(
        "`ref = \"reference\"` but metric `", metric, "` has no reference row; ",
        "no reference line is drawn. Pass a numeric `ref`, or set `cov_name_ref` in `df_forest()`."
      ))
      ref <- NULL
    } else if (length(ref) > 1L) {
      rlang::abort(message = paste0(
        "metric `", metric, "` has multiple distinct reference-row estimates (",
        paste(fmt_sig(ref, stats$config$sigdigits), collapse = ", "),
        "); pass a numeric `ref`."
      ))
    }
  }
  if (!is.null(ref_band) && ref_band_type != "absolute") {
    if (is.null(ref)) {
      rlang::abort(message = paste0(
        "`ref_band_type = \"", ref_band_type, "\"` needs a reference value; ",
        "`ref` resolved to NULL. Pass a numeric `ref`, or use `ref_band_type = \"absolute\"`."
      ))
    }
    ref_band <- switch(
      ref_band_type,
      multiplicative = ref * ref_band,
      additive = ref + ref_band
    )
  }

  ## Rows draw top to bottom in `stats$stats` order. `.row` is the y key, one
  ## level per row, so cov_level labels may repeat across panels; the axis
  ## text is supplied positionally in level order (bottom to top).
  row_ids <- as.character(seq_len(nrow(plot_data)))
  plot_data$.row <- factor(row_ids, levels = rev(row_ids))
  cov_levels <- unique(as.character(plot_data[[cov_name_var_str]]))
  plot_data[[cov_name_var_str]] <- factor(plot_data[[cov_name_var_str]], levels = cov_levels)
  test_data <- plot_data[!plot_data$is_ref, , drop = FALSE]
  ref_data <- plot_data[plot_data$is_ref, , drop = FALSE]

  ## Per-covariate coloring: `colors` entries keyed by a covariate name present
  ## in the plotted data switch point/errorbar color to a covariate mapping.
  has_panel_color <- !is.function(plotstyle$colors) &&
    any(cov_levels %in% names(plotstyle$colors))
  pt_mapping <- if (has_panel_color) ggplot2::aes(color = .data[[cov_name_var_str]])
  eb_mapping <- ggplot2::aes(xmin = .data[["lo"]], xmax = .data[["hi"]], !!!pt_mapping)

  ## geom_blank() trains every panel's y scale (and the covariate colour
  ## scale, when mapped) on the full data first, so the row order set above
  ## survives the subset point layers that follow.
  base <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data[["est"]], y = .data[[".row"]])
  ) +
    ggplot2::geom_blank(pt_mapping)

  ## The equivalence band is an unkeyed annotation; style_plot() fills a
  ## role's colour/alpha but not a rect's `fill`, so both are set inline from
  ## the style's `ref_band` role.
  if (!is.null(ref_band)) {
    rb <- series_aes(plotstyle, "ref_band")
    base <- base +
      ggplot2::geom_rect(
        data = data.frame(
          xmin = ref_band[1], xmax = ref_band[2], ymin = -Inf, ymax = Inf
        ),
        mapping = ggplot2::aes(
          xmin = .data[["xmin"]], xmax = .data[["xmax"]],
          ymin = .data[["ymin"]], ymax = .data[["ymax"]]
        ),
        inherit.aes = FALSE,
        fill = rb$fill %||% "grey80",
        alpha = rb$alpha %||% 0.3,
        color = NA
      )
  }

  ## The reference line is a tagged `ref_line` role layer, so it restyles.
  if (!is.null(ref)) {
    base <- base +
      ggstylekit::series_layer(ggplot2::geom_vline(xintercept = ref), "ref_line")
  }

  base <- base +
    ggstylekit::series_layer(
      ggplot2::geom_linerange(eb_mapping, data = test_data),
      "errorbar"
    ) +
    ggstylekit::series_layer(
      ggplot2::geom_point(pt_mapping, data = test_data),
      "point"
    )
  ## style_plot() fills a point's colour/shape/size/alpha from the role, but
  ## not its `fill`, so the open-shape fill is set inline from the style.
  if (nrow(ref_data) > 0L) {
    rp_args <- compact(list(
      mapping = pt_mapping,
      data = ref_data,
      fill = series_aes(plotstyle, "ref_point")$fill
    ))
    base <- base +
      ggstylekit::series_layer(do.call(ggplot2::geom_point, rp_args), "ref_point")
  }

  ## Left axis: the covariate level labels, blanked where the level repeats
  ## the covariate name (the strip already says it, e.g. a standalone
  ## Reference panel). Right axis: the estimate column (reference rows show
  ## the estimate alone), under a header ("Estimate (90% CI)") drawn as the
  ## right-aligned plot subtitle, the row directly above the panels (a
  ## secondary-axis *title* would land beside the labels instead).
  level_labels <- as.character(plot_data[[cov_level_var_str]])
  level_labels[level_labels == as.character(plot_data[[cov_name_var_str]])] <- ""
  est_labels <- forest_col_labels(
    plot_data$est_label, plot_data$lo_label, plot_data$hi_label, plot_data$is_ref
  )
  header <- paste0("Estimate (", format(stats$config$ci * 100), "% CI)")
  base <- base +
    ggplot2::scale_y_discrete(
      breaks = rev(row_ids),
      labels = rev(level_labels),
      sec.axis = ggplot2::dup_axis(
        name = NULL,
        breaks = rev(row_ids),
        labels = rev(est_labels)
      )
    ) +
    ## The covariate row facet is built here (switch = "y" pairs with the
    ## forest panel theme's outside strips), bypassing ggstylekit's facet path.
    ggplot2::facet_grid(
      rows = ggplot2::vars(!!rlang::sym(cov_name_var_str)),
      scales = "free_y",
      space = "free_y",
      switch = "y"
    ) +
    ggplot2::labs(x = metric, y = NULL, subtitle = header)

  if (has_panel_color) {
    panel_colors <- plotstyle$colors[intersect(cov_levels, names(plotstyle$colors))]
    missing <- setdiff(cov_levels, names(panel_colors))
    if (length(missing) > 0) {
      rlang::warn(paste0(
        "`style` `colors` is missing entries for covariates: ",
        paste(missing, collapse = ", "),
        ". These covariates will render in grey50."
      ))
    }
    base <- base +
      ggplot2::scale_color_manual(
        values = panel_colors,
        na.value = "grey50",
        guide = "none"
      )
  }

  ggstylekit::style_plot(base, plotstyle)
}


#' Plot a forest plot of test and reference comparisons
#'
#' @description
#' Dual-mode wrapper that delegates to [df_forest()] for aggregation and
#' [plot_build_forest()] for rendering. Accepts either:
#'
#' * raw replicate-draws data plus column-name arguments — the common
#'   one-shot mode; or
#' * a precomputed `forest_stats` object returned by [df_forest()] — skip
#'   the aggregation and replot with different `style` / `ref` / `ref_band`
#'   / `ref_band_type` / `ref_layout` settings.
#'
#' On the precomputed path, pipeline arguments (`metric_name_var`, `cov_name_var`,
#' `cov_level_var`, `metric_value_var`, `ci`,
#' `sigdigits`, `cov_name_ref`, `cov_level_ref`) cannot be honored because
#' the aggregation does not run again — passing any of them aborts with a
#' message pointing the caller at [df_forest()]. Only `style`, `ref`,
#' `ref_band`, `ref_band_type`, `ref_layout`, and `metric` are accepted on
#' both paths.
#'
#' @param data Either raw observation/draws data (data.frame) or a
#'    `forest_stats` object returned by [df_forest()].
#' @inheritParams df_forest
#' @inheritParams plot_build_forest
#'
#' @family forest plot
#' @return a `ggplot` plot object
#' @export plot_forest
#'
#' @examples
#' # Raw-data path: one metric per call
#' plot_forest(
#'   dplyr::filter(data_sad_pkforest, metric == "AUCRATIO"),
#'   cov_level_ref = c(FOOD = "Fasted", WTBL = "70 kg")
#' )
#'
#' # Precomputed path: compute once, render different metrics from the same stats
#' stats <- df_forest(
#'   data_sad_pkforest,
#'   cov_level_ref = c(FOOD = "Fasted", WTBL = "70 kg")
#' )
#' plot_forest(stats, metric = "AUCRATIO")
#' plot_forest(stats, metric = "CMAXRATIO", ref_band = c(0.8, 1.25))
#'
#' # Same stats, reference in its own panel instead of each covariate panel
#' plot_forest(stats, metric = "AUCRATIO", ref_layout = "consolidated")
#'
#' # Raw exposure from the same stats: reference line at the reference
#' # exposure, band at 0.8 to 1.25 times it (log x-axis keeps it symmetric)
#' plot_forest(stats, metric = "AUC", ref_band = c(0.8, 1.25),
#'             style = style_forest(logx = TRUE))
#'
#' # Per-covariate coloring via the style's `colors` map
#' plot_forest(
#'   stats, metric = "AUCRATIO",
#'   style = style_forest(colors = c(FOOD = "darkgreen", WTBL = "darkblue"))
#' )

plot_forest <- function(
  data,
  metric = NULL,
  metric_name_var = "metric",
  cov_name_var = "cov_var",
  cov_level_var = "cov_val",
  metric_value_var = "value",
  ci = 0.9,
  sigdigits = 3,
  cov_name_ref = "Reference",
  cov_level_ref = NULL,
  is_ref_var = "is_ref",
  ref = "reference",
  ref_band = NULL,
  ref_band_type = c("multiplicative", "additive", "absolute"),
  ref_layout = NULL,
  style = NULL
) {
  if (inherits(data, "forest_stats")) {
    check_pipeline_args_dropped(
      call = match.call(),
      plot_only_args = c(
        "data", "style", "ref", "ref_band", "ref_band_type", "ref_layout",
        "metric"
      ),
      fn_name = "plot_forest"
    )
    stats <- data
  } else {
    metric_name_var_str <- resolve_var(rlang::enquo(metric_name_var))
    cov_name_var_str <- resolve_var(rlang::enquo(cov_name_var))
    cov_level_var_str <- resolve_var(rlang::enquo(cov_level_var))
    metric_value_var_str <- resolve_var(rlang::enquo(metric_value_var))
    stats <- df_forest(
      data,
      metric_name_var = metric_name_var_str,
      cov_name_var = cov_name_var_str,
      cov_level_var = cov_level_var_str,
      metric_value_var = metric_value_var_str,
      ci = ci,
      sigdigits = sigdigits,
      cov_name_ref = cov_name_ref,
      cov_level_ref = cov_level_ref,
      is_ref_var = is_ref_var
    )
  }

  plot_build_forest(
    stats,
    style = style,
    ref = ref,
    ref_band = ref_band,
    ref_band_type = ref_band_type,
    metric = metric,
    ref_layout = ref_layout
  )
}
