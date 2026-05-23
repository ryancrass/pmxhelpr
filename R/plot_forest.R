#' Compute test/reference comparison statistics for a forest plot
#'
#' @description
#' Aggregates replicate draws (or accepts pre-summarized estimates) into a
#' cacheable, replottable container suitable for a covariate forest plot.
#' Returns a [pmx_stats()] container with class
#' `c("forest_stats", "pmx_stats")` and three slots — `stats` (per-row
#' summary frame with canonical `est`, `lo`, `hi`, `ci_label`, `y_label`
#' columns), `obs` (always `NULL`; no scatter overlay), and `config` (column
#' names + CI configuration) — so that [plot_forest()] / [plot_build_forest()]
#' can render directly from this object without re-aggregating.
#'
#' Two input modes are supported. The function switches on whether
#' `replicate_var` is `NULL`:
#'
#' * **Draws path** (`replicate_var` non-`NULL`): `data` is long-form with a
#'   numeric `value_var` column (e.g., test/reference ratios per replicate)
#'   and a replicate index in `replicate_var`. Per `(metric × cov_name ×
#'   cov_level)` group, the point estimate is computed via `statistic` and
#'   `lo`/`hi` are the `(1-ci)/2` and `1-(1-ci)/2` quantiles of the draws.
#'
#' * **Pre-summarized path** (`replicate_var` `NULL`): `data` already has
#'   per-row point estimates in `est_var`, `lo_var`, `hi_var`. These are
#'   passed through unchanged after a rename to canonical `est`/`lo`/`hi`.
#'
#' Passing arguments from both paths in the same call aborts.
#'
#' @param data Input data.frame.
#' @param metric_var Column in `data` naming the PK/PD parameter (drives
#'    facet panels in the plot). Accepts bare names or strings. Default
#'    `metric`.
#' @param cov_name_var Column in `data` naming the covariate (e.g., `WTBL`,
#'    `FOOD`). Accepts bare names or strings. Default `cov_var`.
#' @param cov_level_var Column in `data` naming the level within each
#'    covariate (e.g., `> 70 kg`). Accepts bare names or strings. Default
#'    `cov_val`.
#' @param value_var Column in `data` containing replicate draws of the
#'    test/reference comparison statistic. Accepts bare names or strings.
#'    Required on the draws path; ignored on the pre-summarized path.
#'    Default `value`.
#' @param replicate_var Column in `data` indexing the replicate (e.g.,
#'    posterior/bootstrap draw id). Accepts bare names or strings, or
#'    `NULL`. When non-`NULL`, selects the draws path. Default `NULL`.
#' @param est_var,lo_var,hi_var Columns in `data` containing the point
#'    estimate and lower / upper bounds of the pre-computed confidence
#'    interval. Accept bare names or strings. All three required together
#'    on the pre-summarized path; `NULL` selects the draws path. Defaults
#'    `NULL`.
#' @param statistic Central-tendency statistic on the draws path. One of
#'    `"median"` (default), `"mean"`, or `"geo_mean"`. Ignored on the
#'    pre-summarized path.
#' @param ci Numeric scalar in `(0, 1)` specifying the central interval
#'    width (e.g., `0.9` for 90% CI). Used to derive symmetric
#'    quantiles on the draws path. Ignored on the pre-summarized path.
#'    Default `0.9`.
#' @param sigdigits Number of significant digits used to format the
#'    `ci_label` and `y_label` columns. Default `3`.
#'
#' @family forest plot
#' @return A `forest_stats` container (subclass of `pmx_stats`) with three
#'    slots:
#'    \describe{
#'      \item{`stats`}{One row per `(metric × cov_name × cov_level)` group
#'        with columns named in `metric_var` / `cov_name_var` /
#'        `cov_level_var`, canonical numeric columns `est`, `lo`, `hi`,
#'        and character columns `ci_label` (e.g. `"1.12 [0.95, 1.32]"`)
#'        and `y_label` (e.g. `"WTBL: > 70 kg — 1.12 [0.95, 1.32]"`).}
#'      \item{`obs`}{`NULL`. No scatter overlay for the forest family.}
#'      \item{`config`}{Named list with column names, `statistic`, `ci`,
#'        and `sigdigits`.}
#'    }
#'    Pass directly to [plot_forest()] or [plot_build_forest()] to render.
#' @export df_forest
#'
#' @examples
#' # Draws path: aggregate replicate simulations to point estimate + 90% CI
#' df_forest(
#'   dplyr::filter(data_sad_pkforest, grepl("RATIO$", metric)),
#'   replicate_var = "SIM"
#' )
#'
#' # Pre-summarized path: pass precomputed estimates through unchanged
#' df_forest(
#'   dplyr::filter(data_sad_pkforest_sum, grepl("RATIO$", metric)),
#'   est_var = "P50", lo_var = "P05", hi_var = "P95"
#' )

df_forest <- function(data,
                      metric_var    = "metric",
                      cov_name_var  = "cov_var",
                      cov_level_var = "cov_val",
                      value_var     = "value",
                      replicate_var = NULL,
                      est_var       = NULL,
                      lo_var        = NULL,
                      hi_var        = NULL,
                      statistic     = "median",
                      ci            = 0.9,
                      sigdigits     = 3) {

  metric_var_str    <- resolve_var(rlang::enquo(metric_var))
  cov_name_var_str  <- resolve_var(rlang::enquo(cov_name_var))
  cov_level_var_str <- resolve_var(rlang::enquo(cov_level_var))
  value_var_str     <- resolve_var(rlang::enquo(value_var),     nullable = TRUE)
  replicate_var_str <- resolve_var(rlang::enquo(replicate_var), nullable = TRUE)
  est_var_str       <- resolve_var(rlang::enquo(est_var),       nullable = TRUE)
  lo_var_str        <- resolve_var(rlang::enquo(lo_var),        nullable = TRUE)
  hi_var_str        <- resolve_var(rlang::enquo(hi_var),        nullable = TRUE)

  check_df(data, "data")
  check_forest_args(statistic, ci, sigdigits)
  check_varsindf(data, c(metric_var_str, cov_name_var_str, cov_level_var_str),
                 "data", "metric_var/cov_name_var/cov_level_var")

  draws_path  <- !is.null(replicate_var_str)
  presum_args <- c(est_var_str, lo_var_str, hi_var_str)
  presum_path <- length(presum_args) > 0L

  if (draws_path && presum_path) {
    rlang::abort(message = paste0(
      "argument `replicate_var` cannot be combined with `est_var`/`lo_var`/`hi_var`. ",
      "Use the draws path (replicate_var + value_var) OR the pre-summarized path ",
      "(est_var + lo_var + hi_var), not both."
    ))
  }
  if (!draws_path && !presum_path) {
    rlang::abort(message = paste0(
      "argument `replicate_var` or `est_var`/`lo_var`/`hi_var` must be supplied. ",
      "Use the draws path (replicate_var + value_var) to aggregate replicate draws, ",
      "or the pre-summarized path (est_var + lo_var + hi_var) to pass through ",
      "precomputed estimates."
    ))
  }

  if (draws_path) {
    if (is.null(value_var_str)) {
      rlang::abort(message = "argument `value_var` is required on the draws path (`replicate_var` non-`NULL`)")
    }
    check_varsindf(data, c(value_var_str, replicate_var_str),
                   "data", "value_var/replicate_var")

    probs <- c((1 - ci) / 2, 1 - (1 - ci) / 2)
    stats_fn <- switch(statistic,
      median   = function(x) stats::median(x, na.rm = TRUE),
      mean     = function(x) mean(x, na.rm = TRUE),
      geo_mean = function(x) exp(mean(log(x), na.rm = TRUE))
    )

    stats <- data |>
      dplyr::group_by(.data[[metric_var_str]],
                      .data[[cov_name_var_str]],
                      .data[[cov_level_var_str]]) |>
      dplyr::summarise(
        est = stats_fn(.data[[value_var_str]]),
        lo  = unname(stats::quantile(.data[[value_var_str]], probs[1], na.rm = TRUE)),
        hi  = unname(stats::quantile(.data[[value_var_str]], probs[2], na.rm = TRUE)),
        .groups = "drop"
      )
  } else {
    missing_presum <- c(
      if (is.null(est_var_str)) "est_var",
      if (is.null(lo_var_str))  "lo_var",
      if (is.null(hi_var_str))  "hi_var"
    )
    if (length(missing_presum) > 0L) {
      rlang::abort(message = paste0(
        "pre-summarized path requires all of `est_var`, `lo_var`, `hi_var`. ",
        "Missing: ", paste(missing_presum, collapse = ", ")
      ))
    }
    check_varsindf(data, c(est_var_str, lo_var_str, hi_var_str),
                   "data", "est_var/lo_var/hi_var")

    keep_cols <- c(metric_var_str, cov_name_var_str, cov_level_var_str,
                   est_var_str, lo_var_str, hi_var_str)
    stats <- data |>
      dplyr::select(dplyr::all_of(keep_cols)) |>
      dplyr::rename(dplyr::any_of(c(est = est_var_str,
                                    lo  = lo_var_str,
                                    hi  = hi_var_str)))
  }

  stats <- stats |>
    dplyr::mutate(
      ci_label = paste0(signif(.data$est, sigdigits), " [",
                        signif(.data$lo,  sigdigits), ", ",
                        signif(.data$hi,  sigdigits), "]"),
      y_label  = paste0(.data[[cov_name_var_str]], ": ",
                        .data[[cov_level_var_str]], " - ",
                        .data$ci_label)
    )

  pmx_stats(
    stats  = as.data.frame(stats),
    obs    = NULL,
    config = list(
      metric_var    = metric_var_str,
      cov_name_var  = cov_name_var_str,
      cov_level_var = cov_level_var_str,
      value_var     = if (draws_path) value_var_str else NULL,
      replicate_var = replicate_var_str,
      est_var       = if (!draws_path) est_var_str else NULL,
      lo_var        = if (!draws_path) lo_var_str  else NULL,
      hi_var        = if (!draws_path) hi_var_str  else NULL,
      statistic     = statistic,
      ci            = ci,
      sigdigits     = sigdigits
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
    rlang::abort("`x` must be a `forest_stats` object (output of `df_forest()`).")
  }
  validate_pmx_stats(x)
  required_cols <- c("est", "lo", "hi", "ci_label", "y_label")
  missing_cols <- setdiff(required_cols, colnames(x$stats))
  if (length(missing_cols) > 0) {
    rlang::abort(paste0("`forest_stats` is missing required columns: ",
                        paste(missing_cols, collapse = ", ")))
  }
  required_config <- c("metric_var", "cov_name_var", "cov_level_var",
                       "statistic", "ci", "sigdigits")
  missing_config <- setdiff(required_config, names(x$config))
  if (length(missing_config) > 0) {
    rlang::abort(paste0("`forest_stats` is missing required config keys: ",
                        paste(missing_config, collapse = ", ")))
  }
  check_varsindf(x$stats, x$config$metric_var,    "stats", "config$metric_var")
  check_varsindf(x$stats, x$config$cov_name_var,  "stats", "config$cov_name_var")
  check_varsindf(x$stats, x$config$cov_level_var, "stats", "config$cov_level_var")
  invisible(x)
}



#' Build a forest plot ggplot from a `forest_stats` object
#'
#' @description
#' Constructs a covariate forest plot from a [df_forest()] result (or any
#' object satisfying the `forest_stats` contract). Renders per-row point
#' estimates with horizontal CI lines, optional shaded equivalence band, and
#' a no-effect vertical reference line; facets across `metric_var`.
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
#' @param theme Named list of aesthetic parameters for the plot created by
#'    [plot_forest_theme()]. Defaults can be viewed by running
#'    `plot_forest_theme()` with no arguments.
#' @param ref Numeric scalar specifying the x-intercept of the no-effect
#'    vertical reference line. Default `1` (the ratio-scale no-effect
#'    value). Pass `NULL` to suppress the reference line.
#' @param ref_band Length-2 numeric vector giving the shaded equivalence
#'    interval (e.g., `c(0.8, 1.25)` for bioequivalence). Default `NULL`
#'    (no shaded band).
#' @param annotate_ci Logical. When `TRUE` (default), the y-axis tick labels
#'    concatenate the row's covariate name, level, and formatted
#'    `est [lo, hi]` text. When `FALSE`, labels show only
#'    `cov_name: cov_level`; the `ci_label` column is still available in
#'    `stats$stats` for custom downstream annotation.
#'
#' @family forest plot
#' @return a `ggplot` plot object
#' @export plot_build_forest
#'
#' @examples
#' stats <- df_forest(
#'   dplyr::filter(data_sad_pkforest, grepl("RATIO$", metric)),
#'   replicate_var = "SIM"
#' )
#' plot_build_forest(stats)
#' plot_build_forest(stats, ref_band = c(0.8, 1.25))
#' plot_build_forest(stats, annotate_ci = FALSE)

plot_build_forest <- function(stats,
                              theme       = NULL,
                              ref         = 1,
                              ref_band    = NULL,
                              annotate_ci = TRUE) {

  validate_forest_stats(stats)
  check_ref_band(ref_band)
  if (!is.null(ref)) check_numeric_strict(ref, "ref")
  check_boolean(annotate_ci, "annotate_ci")

  metric_var_str    <- stats$config$metric_var
  cov_name_var_str  <- stats$config$cov_name_var
  cov_level_var_str <- stats$config$cov_level_var

  plottheme <- merge_theme(theme, plot_forest_theme())

  plot_data <- stats$stats

  if (isTRUE(annotate_ci)) {
    y_aes_col <- "y_label"
  } else {
    plot_data$y_plain <- paste0(plot_data[[cov_name_var_str]], ": ",
                                plot_data[[cov_level_var_str]])
    y_aes_col <- "y_plain"
  }
  plot_data[[y_aes_col]] <- factor(plot_data[[y_aes_col]],
                                   levels = rev(unique(plot_data[[y_aes_col]])))

  base <- init_plot(plot_data, x_var = "est", y_var = y_aes_col)

  if (!is.null(ref_band)) {
    rb <- plottheme$ref_band
    base <- base + ggplot2::annotate(
      "rect",
      xmin = ref_band[1], xmax = ref_band[2],
      ymin = -Inf, ymax = Inf,
      fill  = rb$fill,
      alpha = rb$alpha,
      color = rb$color
    )
  }

  if (!is.null(ref)) {
    rl <- plottheme$ref_line
    base <- base + do.call(ggplot2::geom_vline, compact(list(
      xintercept = ref,
      linewidth  = rl$linewidth,
      linetype   = rl$linetype,
      alpha      = rl$alpha,
      color      = rl$color
    )))
  }

  eb <- plottheme$errorbar
  base <- base + do.call(ggplot2::geom_linerange, compact(list(
    mapping   = ggplot2::aes(xmin = .data$lo, xmax = .data$hi),
    linewidth = eb$linewidth,
    linetype  = eb$linetype,
    alpha     = eb$alpha,
    color     = eb$color
  )))

  pt <- plottheme$point
  base <- base + do.call(ggplot2::geom_point, compact(list(
    shape = pt$shape,
    size  = pt$size,
    alpha = pt$alpha,
    color = pt$color
  )))

  base +
    ggplot2::facet_wrap(stats::reformulate(metric_var_str),
                        scales = "free_x") +
    ggplot2::xlab("Test / Reference") +
    ggplot2::ylab(NULL)
}



#' Plot a forest plot of test/reference comparisons
#'
#' @description
#' Dual-mode wrapper that delegates to [df_forest()] for aggregation and
#' [plot_build_forest()] for rendering. Accepts either:
#'
#' * raw replicate-draws or pre-summarized data plus column-name arguments —
#'   the common one-shot mode; or
#' * a precomputed `forest_stats` object returned by [df_forest()] — skip
#'   the aggregation and replot with different `theme` / `ref` / `ref_band`
#'   / `annotate_ci` settings.
#'
#' On the precomputed path, pipeline arguments (`metric_var`, `cov_name_var`,
#' `cov_level_var`, `value_var`, `replicate_var`, `est_var`, `lo_var`,
#' `hi_var`, `statistic`, `ci`, `sigdigits`) cannot be honored because the
#' aggregation does not run again — passing any of them aborts with a
#' message pointing the caller at [df_forest()]. Only `theme`, `ref`,
#' `ref_band`, and `annotate_ci` are accepted on both paths.
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
#' # Raw-data path
#' plot_forest(
#'   dplyr::filter(data_sad_pkforest, grepl("RATIO$", metric)),
#'   replicate_var = "SIM"
#' )
#'
#' # Precomputed path: compute once, replot many times
#' stats <- df_forest(
#'   dplyr::filter(data_sad_pkforest, grepl("RATIO$", metric)),
#'   replicate_var = "SIM"
#' )
#' plot_forest(stats)
#' plot_forest(stats, ref_band = c(0.8, 1.25))

plot_forest <- function(data,
                        metric_var    = "metric",
                        cov_name_var  = "cov_var",
                        cov_level_var = "cov_val",
                        value_var     = "value",
                        replicate_var = NULL,
                        est_var       = NULL,
                        lo_var        = NULL,
                        hi_var        = NULL,
                        statistic     = "median",
                        ci            = 0.9,
                        sigdigits     = 3,
                        ref           = 1,
                        ref_band      = NULL,
                        annotate_ci   = TRUE,
                        theme         = NULL) {

  if (inherits(data, "forest_stats")) {
    check_pipeline_args_dropped(
      call           = match.call(),
      plot_only_args = c("data", "theme", "ref", "ref_band", "annotate_ci"),
      fn_name        = "plot_forest"
    )
    return(plot_build_forest(data, theme = theme, ref = ref,
                             ref_band = ref_band, annotate_ci = annotate_ci))
  }

  metric_var_str    <- resolve_var(rlang::enquo(metric_var))
  cov_name_var_str  <- resolve_var(rlang::enquo(cov_name_var))
  cov_level_var_str <- resolve_var(rlang::enquo(cov_level_var))
  value_var_str     <- resolve_var(rlang::enquo(value_var),     nullable = TRUE)
  replicate_var_str <- resolve_var(rlang::enquo(replicate_var), nullable = TRUE)
  est_var_str       <- resolve_var(rlang::enquo(est_var),       nullable = TRUE)
  lo_var_str        <- resolve_var(rlang::enquo(lo_var),        nullable = TRUE)
  hi_var_str        <- resolve_var(rlang::enquo(hi_var),        nullable = TRUE)

  stats <- df_forest(
    data,
    metric_var    = metric_var_str,
    cov_name_var  = cov_name_var_str,
    cov_level_var = cov_level_var_str,
    value_var     = value_var_str,
    replicate_var = replicate_var_str,
    est_var       = est_var_str,
    lo_var        = lo_var_str,
    hi_var        = hi_var_str,
    statistic     = statistic,
    ci            = ci,
    sigdigits     = sigdigits
  )

  plot_build_forest(stats, theme = theme, ref = ref,
                    ref_band = ref_band, annotate_ci = annotate_ci)
}
