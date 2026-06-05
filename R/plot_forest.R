#' Compute test/reference comparison statistics for a forest plot
#'
#' @description
#' Aggregates replicate draws into a cacheable, replottable container
#' suitable for a covariate forest plot. Returns a [pmx_stats()] container
#' with class `c("forest_stats", "pmx_stats")` and three slots — `stats`
#' (per-row summary frame with canonical `est`, `lo`, `hi`, `ci_label`,
#' `y_label` columns), `obs` (always `NULL`; no scatter overlay), and
#' `config` (column names + CI configuration) — so that [plot_forest()] /
#' [plot_build_forest()] can render directly from this object without
#' re-aggregating.
#'
#' `data` is long-form with a numeric `metric_value_var` column (e.g.,
#' test/reference ratios per replicate) and a replicate index in
#' `replicate_var`. Per `(metric × cov_name × cov_level)` group, the point
#' estimate is computed via `statistic` and `lo`/`hi` are the `(1-ci)/2` and
#' `1-(1-ci)/2` quantiles of the draws.
#'
#' @param data Input data.frame.
#' @param metric_name_var Column in `data` naming the PK/PD parameter (drives
#'    facet panels in the plot). Accepts bare names or strings. Default
#'    `metric`.
#' @param cov_name_var Column in `data` naming the covariate (e.g., `WTBL`,
#'    `FOOD`). Accepts bare names or strings. Default `cov_var`.
#' @param cov_level_var Column in `data` naming the level within each
#'    covariate (e.g., `> 70 kg`). Accepts bare names or strings. Default
#'    `cov_val`.
#' @param metric_value_var Column in `data` containing replicate draws of the
#'    test/reference comparison statistic. Accepts bare names or strings.
#'    Default `value`.
#' @param replicate_var Column in `data` indexing the replicate (e.g.,
#'    posterior/bootstrap draw id). Accepts bare names or strings.
#'    Required.
#' @param statistic Central-tendency statistic. One of `"median"` (default),
#'    `"mean"`, or `"geo_mean"`.
#' @param ci Numeric scalar in `(0, 1)` specifying the central interval
#'    width (e.g., `0.9` for 90% CI). Used to derive symmetric
#'    quantiles. Default `0.9`.
#' @param sigdigits Number of significant digits used to format the
#'    `ci_label` and `y_label` columns. Default `3`.
#' @param cov_name_ref Character scalar identifying the reference row(s) in the
#'    `cov_name_var` column. Rows whose `cov_name_var` value equals
#'    `cov_name_ref` are sorted to the top of the rendered plot (first facet
#'    row). Default `"REF"` (matches the bundled `data_sad_pkforest`
#'    convention). Pass `NULL` to disable REF-first sorting; rows then
#'    appear in data order. Not to be confused with `ref` in
#'    [plot_build_forest()] / [plot_forest()], which is the numeric
#'    x-intercept of the no-effect vertical reference line.
#' @param cov_level_ref Optional named atomic vector mapping `cov_name_var`
#'    values to the per-covariate reference label to display when the
#'    REF row is dispersed into each panel (see [plot_build_forest()]).
#'    For example, `c(FOOD = "Fasted", WTBL = "70 kg")`. Default `NULL`
#'    (REF row renders in its own facet at the top, no dispersal).
#'    `df_forest()` builds the canonical `cov_ref` column inside
#'    `stats$stats` from this mapping; names absent from `cov_level_ref`
#'    receive `NA` (those panels show no dispersed REF row). The
#'    mapping is stored in `config$cov_level_ref` for the renderer.
#'
#'    Alternative input mode: if `data` already carries a `cov_ref` column,
#'    it is used directly (per-row reference labels) and `cov_level_ref` is
#'    ignored with an informational `message()`. This is the preferred form
#'    when reference labels can't be reduced to a single value per
#'    `cov_name_var` — for example, when a study has multiple dosing
#'    cohorts sharing one covariate dimension but different REF doses.
#'
#' @family forest plot
#' @return A `forest_stats` container (subclass of `pmx_stats`) with three
#'    slots:
#'    \describe{
#'      \item{`stats`}{One row per `(metric × cov_name × cov_level)` group
#'        with columns named in `metric_name_var` / `cov_name_var` /
#'        `cov_level_var`, canonical numeric columns `est`, `lo`, `hi`,
#'        and character columns `ci_label` (e.g. `"1.12 [0.95, 1.32]"`)
#'        and `y_label` (a stable canonical name for the y-axis tick
#'        text; equal to the row's `cov_level_var` value as character).
#'        The `cov_name_var` value drives the per-row facet strip; the
#'        `cov_level_var` value drives the y-axis tick label;
#'        [plot_build_forest()] renders `ci_label` as a right-side
#'        secondary-axis text layer.}
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
#' # Custom reference level (default is "REF")
#' df_forest(data_sad_pkforest, replicate_var = "SIM", cov_name_ref = "REF")
#'
#' # Per-covariate reference labels (dispersed REF in each panel)
#' df_forest(
#'   dplyr::filter(data_sad_pkforest, metric == "AUCRATIO"),
#'   replicate_var = "SIM",
#'   cov_level_ref = c(FOOD = "Fasted", WTBL = "70 kg")
#' )
#'
#' # Alternative input: per-row `cov_ref` column already attached to `data`
#' d <- dplyr::filter(data_sad_pkforest, metric == "AUCRATIO")
#' d$cov_ref <- c(FOOD = "Fasted", WTBL = "70 kg", REF = NA)[
#'   as.character(d$cov_var)
#' ]
#' df_forest(d, replicate_var = "SIM")

df_forest <- function(data,
                      metric_name_var  = "metric",
                      cov_name_var     = "cov_var",
                      cov_level_var    = "cov_val",
                      metric_value_var = "value",
                      replicate_var    = NULL,
                      statistic        = "median",
                      ci               = 0.9,
                      sigdigits        = 3,
                      cov_name_ref     = "REF",
                      cov_level_ref    = NULL) {

  metric_name_var_str  <- resolve_var(rlang::enquo(metric_name_var))
  cov_name_var_str     <- resolve_var(rlang::enquo(cov_name_var))
  cov_level_var_str    <- resolve_var(rlang::enquo(cov_level_var))
  metric_value_var_str <- resolve_var(rlang::enquo(metric_value_var))
  replicate_var_str    <- resolve_var(rlang::enquo(replicate_var), nullable = TRUE)

  check_df(data, "data")
  check_forest_args(statistic, ci, sigdigits, cov_name_ref)
  check_cov_level_ref(cov_level_ref)
  check_varsindf(data, c(metric_name_var_str, cov_name_var_str, cov_level_var_str),
                 "data", "metric_name_var/cov_name_var/cov_level_var")

  if (is.null(replicate_var_str)) {
    rlang::abort(message = "argument `replicate_var` is required")
  }
  check_varsindf(data, c(metric_value_var_str, replicate_var_str),
                 "data", "metric_value_var/replicate_var")

  if ("cov_ref" %in% colnames(data)) {
    if (!is.null(cov_level_ref)) {
      message("Inheriting per-row `cov_ref` from `cov_ref` column in `data`; ignoring `cov_level_ref` argument.")
      cov_level_ref <- NULL
    }
    data$cov_ref <- as.character(data$cov_ref)
  } else if (!is.null(cov_level_ref)) {
    data$cov_ref <- as.character(unname(
      cov_level_ref[as.character(data[[cov_name_var_str]])]
    ))
  }

  probs <- c((1 - ci) / 2, 1 - (1 - ci) / 2)
  stats_fn <- switch(statistic,
    median   = function(x) stats::median(x, na.rm = TRUE),
    mean     = function(x) mean(x, na.rm = TRUE),
    geo_mean = function(x) exp(mean(log(x), na.rm = TRUE))
  )

  grp_vars <- c(metric_name_var_str, cov_name_var_str, cov_level_var_str,
                if ("cov_ref" %in% colnames(data)) "cov_ref")
  stats <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_vars))) |>
    dplyr::summarise(
      est = stats_fn(.data[[metric_value_var_str]]),
      lo  = unname(stats::quantile(.data[[metric_value_var_str]], probs[1], na.rm = TRUE)),
      hi  = unname(stats::quantile(.data[[metric_value_var_str]], probs[2], na.rm = TRUE)),
      .groups = "drop"
    )

  stats <- stats |>
    dplyr::mutate(
      ci_label = paste0(signif(.data$est, sigdigits), " [",
                        signif(.data$lo,  sigdigits), ", ",
                        signif(.data$hi,  sigdigits), "]"),
      y_label  = as.character(.data[[cov_level_var_str]])
    )

  pmx_stats(
    stats  = as.data.frame(stats),
    obs    = NULL,
    config = list(
      metric_name_var  = metric_name_var_str,
      cov_name_var     = cov_name_var_str,
      cov_level_var    = cov_level_var_str,
      metric_value_var = metric_value_var_str,
      replicate_var    = replicate_var_str,
      statistic        = statistic,
      ci               = ci,
      sigdigits        = sigdigits,
      cov_name_ref     = cov_name_ref,
      cov_level_ref    = cov_level_ref
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
  required_config <- c("metric_name_var", "cov_name_var", "cov_level_var",
                       "statistic", "ci", "sigdigits", "cov_name_ref",
                       "cov_level_ref")
  missing_config <- setdiff(required_config, names(x$config))
  if (length(missing_config) > 0) {
    rlang::abort(paste0("`forest_stats` is missing required config keys: ",
                        paste(missing_config, collapse = ", ")))
  }
  check_varsindf(x$stats, x$config$metric_name_var,    "stats", "config$metric_name_var")
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
#' a no-effect vertical reference line; facets across `metric_name_var`.
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
#'    value). Pass `NULL` to suppress the reference line. Distinct from
#'    `cov_name_ref` in [df_forest()], which identifies the reference row to
#'    sort to the top of the plot.
#' @param ref_band Length-2 numeric vector giving the shaded equivalence
#'    interval (e.g., `c(0.8, 1.25)` for bioequivalence). Default `NULL`
#'    (no shaded band).
#' @param metric Character scalar identifying which row of `metric_name_var` to
#'    render. `plot_forest()` renders one metric per call (no metric
#'    facet); the covariate name is the only facet dimension. If `NULL`
#'    (default) and `stats$stats` contains exactly one unique value of
#'    `metric_name_var`, that value is used. If `NULL` and multiple metrics
#'    are present, aborts and asks the caller to pick one.
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
#' plot_build_forest(stats, metric = "AUCRATIO")
#' plot_build_forest(stats, metric = "AUCRATIO", ref_band = c(0.8, 1.25))
#' # Log x-axis: compose with ggplot2 directly
#' plot_build_forest(stats, metric = "AUCRATIO") +
#'   ggplot2::scale_x_log10(guide = "axis_logticks")

plot_build_forest <- function(stats,
                              theme    = NULL,
                              ref      = 1,
                              ref_band = NULL,
                              metric   = NULL) {

  validate_forest_stats(stats)
  check_ref_band(ref_band)
  if (!is.null(ref)) check_numeric_strict(ref, "ref")

  metric_name_var_str <- stats$config$metric_name_var
  cov_name_var_str    <- stats$config$cov_name_var
  cov_level_var_str   <- stats$config$cov_level_var
  cov_name_ref        <- stats$config$cov_name_ref
  disperse_ref        <- "cov_ref" %in% colnames(stats$stats)

  if (disperse_ref && is.null(cov_name_ref)) {
    rlang::abort(message = paste0(
      "cannot disperse REF row when `cov_name_ref` is NULL. ",
      "Set a non-NULL `cov_name_ref` in `df_forest()`, or remove the `cov_ref` ",
      "column from `data` / omit `cov_level_ref`."
    ))
  }

  plottheme <- merge_theme(theme, plot_forest_theme())

  plot_data <- stats$stats

  metric_values <- unique(as.character(plot_data[[metric_name_var_str]]))
  if (is.null(metric)) {
    if (length(metric_values) != 1L) {
      rlang::abort(message = paste0(
        "argument `metric` is required when `stats$stats` contains multiple values of `",
        metric_name_var_str, "` (", paste(metric_values, collapse = ", "), "). ",
        "Pass `metric = <one of the listed values>` to pick which to render."
      ))
    }
    metric <- metric_values
  } else {
    if (!is.character(metric) || length(metric) != 1L ||
        is.na(metric) || !nzchar(metric)) {
      rlang::abort(message = "argument `metric` must be a single non-empty character string, or `NULL`")
    }
    if (!metric %in% metric_values) {
      rlang::abort(message = paste0(
        "argument `metric` (", metric, ") not found in `", metric_name_var_str,
        "`. Available: ", paste(metric_values, collapse = ", ")
      ))
    }
    plot_data <- plot_data[as.character(plot_data[[metric_name_var_str]]) == metric, , drop = FALSE]
  }

  if (disperse_ref) {
    ref_rows <- plot_data[as.character(plot_data[[cov_name_var_str]]) == cov_name_ref, , drop = FALSE]
    if (nrow(ref_rows) == 0L) {
      rlang::abort(message = paste0(
        "cannot disperse REF row: no rows with `", cov_name_var_str, " == \"",
        cov_name_ref, "\"` were found for metric `", metric, "`."
      ))
    }
    ref_row <- ref_rows[1L, , drop = FALSE]
    non_ref <- plot_data[as.character(plot_data[[cov_name_var_str]]) != cov_name_ref, , drop = FALSE]

    cov_names_non_ref <- unique(as.character(non_ref[[cov_name_var_str]]))
    synth_rows_list <- list()
    for (cn in cov_names_non_ref) {
      vals <- unique(non_ref$cov_ref[as.character(non_ref[[cov_name_var_str]]) == cn])
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0L) next
      if (length(vals) > 1L) {
        rlang::abort(message = paste0(
          "covariate `", cn, "` has multiple distinct `cov_ref` values (",
          paste(vals, collapse = ", "), "). ",
          "Each non-REF covariate must declare a single reference label."
        ))
      }
      row <- ref_row
      row[[cov_name_var_str]]  <- cn
      row[[cov_level_var_str]] <- vals
      row$y_label              <- vals
      synth_rows_list[[cn]]    <- row
    }
    synth_rows <- if (length(synth_rows_list)) do.call(rbind, synth_rows_list) else ref_row[FALSE, , drop = FALSE]
    plot_data  <- rbind(synth_rows, non_ref)
  }

  cov_names_unique <- unique(as.character(plot_data[[cov_name_var_str]]))
  if (!disperse_ref && !is.null(cov_name_ref) && cov_name_ref %in% cov_names_unique) {
    cov_names_ordered <- c(cov_name_ref, setdiff(cov_names_unique, cov_name_ref))
  } else {
    cov_names_ordered <- cov_names_unique
  }

  # Per-panel y_label ordering: parse cov_level as numeric when possible
  # (so e.g. WTBL levels "50 kg"/"70 kg"/"90 kg" sort ascending → 90 on top of
  # the discrete y-axis). Falls back to reverse-data-encounter order for
  # categorical labels (so e.g. FOOD "Fasted" stays above "Fed").
  y_levels <- character(0)
  for (cn in cov_names_ordered) {
    panel_labels <- unique(as.character(
      plot_data$y_label[as.character(plot_data[[cov_name_var_str]]) == cn]
    ))
    if (length(panel_labels) == 0L) next
    nums <- suppressWarnings(as.numeric(gsub("[^0-9.eE+\\-]", "", panel_labels)))
    if (all(!is.na(nums))) {
      panel_order <- panel_labels[order(nums)]
    } else {
      panel_order <- rev(panel_labels)
    }
    y_levels <- c(y_levels, panel_order)
  }
  plot_data$y_label <- factor(plot_data$y_label, levels = y_levels)

  plot_data[[cov_name_var_str]] <- factor(plot_data[[cov_name_var_str]],
                                          levels = cov_names_ordered)

  base <- init_plot(plot_data, x_var = "est", y_var = "y_label",
                    forest_panel = TRUE)

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

  base <- base +
    ggplot2::geom_text(
      mapping = ggplot2::aes(x = Inf, label = .data$ci_label),
      hjust   = -0.1,
      size    = 3,
      color   = "grey20"
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 5.5, r = 80,
                                                 b = 5.5, l = 5.5))

  base +
    ggplot2::facet_grid(
      rows   = ggplot2::vars(!!rlang::sym(cov_name_var_str)),
      scales = "free_y",
      space  = "free_y",
      switch = "y"
    ) +
    ggplot2::xlab("Test / Reference") +
    ggplot2::ylab(NULL) +
    ggplot2::ggtitle(metric)
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
#'   settings.
#'
#' On the precomputed path, pipeline arguments (`metric_name_var`, `cov_name_var`,
#' `cov_level_var`, `metric_value_var`, `replicate_var`, `statistic`, `ci`,
#' `sigdigits`, `cov_name_ref`, `cov_level_ref`) cannot be honored because
#' the aggregation does not run again — passing any of them aborts with a
#' message pointing the caller at [df_forest()]. Only `theme`, `ref`,
#' `ref_band`, and `metric` are accepted on both paths.
#'
#' @param data Either raw observation/draws data (data.frame) or a
#'    `forest_stats` object returned by [df_forest()].
#' @param metric Character scalar identifying which row of `metric_name_var` to
#'    render. `plot_forest()` renders one metric per call (no metric
#'    facet); the covariate name is the only facet dimension. If `NULL`
#'    (default) and `stats$stats` contains exactly one unique value of
#'    `metric_name_var`, that value is used. If `NULL` and multiple metrics
#'    are present, aborts and asks the caller to pick one.
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
#'   replicate_var = "SIM"
#' )
#'
#' # Precomputed path: compute once, render different metrics from the same stats
#' stats <- df_forest(
#'   dplyr::filter(data_sad_pkforest, grepl("RATIO$", metric)),
#'   replicate_var = "SIM"
#' )
#' plot_forest(stats, metric = "AUCRATIO")
#' plot_forest(stats, metric = "CMAXRATIO", ref_band = c(0.8, 1.25))

plot_forest <- function(data,
                        metric           = NULL,
                        metric_name_var  = "metric",
                        cov_name_var     = "cov_var",
                        cov_level_var    = "cov_val",
                        metric_value_var = "value",
                        replicate_var    = NULL,
                        statistic        = "median",
                        ci               = 0.9,
                        sigdigits        = 3,
                        cov_name_ref     = "REF",
                        cov_level_ref    = NULL,
                        ref              = 1,
                        ref_band         = NULL,
                        theme            = NULL) {

  if (inherits(data, "forest_stats")) {
    check_pipeline_args_dropped(
      call           = match.call(),
      plot_only_args = c("data", "theme", "ref", "ref_band", "metric"),
      fn_name        = "plot_forest"
    )
    return(plot_build_forest(data, theme = theme, ref = ref,
                             ref_band = ref_band, metric = metric))
  }

  metric_name_var_str  <- resolve_var(rlang::enquo(metric_name_var))
  cov_name_var_str     <- resolve_var(rlang::enquo(cov_name_var))
  cov_level_var_str    <- resolve_var(rlang::enquo(cov_level_var))
  metric_value_var_str <- resolve_var(rlang::enquo(metric_value_var))
  replicate_var_str    <- resolve_var(rlang::enquo(replicate_var), nullable = TRUE)

  stats <- df_forest(
    data,
    metric_name_var  = metric_name_var_str,
    cov_name_var     = cov_name_var_str,
    cov_level_var    = cov_level_var_str,
    metric_value_var = metric_value_var_str,
    replicate_var    = replicate_var_str,
    statistic        = statistic,
    ci               = ci,
    sigdigits        = sigdigits,
    cov_name_ref     = cov_name_ref,
    cov_level_ref    = cov_level_ref
  )

  plot_build_forest(stats, theme = theme, ref = ref,
                    ref_band = ref_band, metric = metric)
}
