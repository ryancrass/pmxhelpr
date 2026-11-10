#' Internal helper: the rows a forest plot draws, in draw order
#'
#' Returns `stats$stats` (optionally restricted to one metric) filtered to the
#' requested reference layout through its `layout` column: rows marked
#' `"both"` always draw, `"consolidated"` rows (the reference panel) only in
#' that layout, `"distributed"` rows (the reference copies in each covariate
#' panel) only in that one.
#'
#' @param stats A `forest_stats` object.
#' @param ref_layout `NULL`, `"distributed"`, or `"consolidated"`. `NULL`
#'    resolves to `"distributed"` when the rows of `metric` (all rows if
#'    `metric` is `NULL`) include distributed rows, otherwise `"consolidated"`.
#' @param metric Optional metric value to restrict the rows to.
#'
#' @return A data.frame of rows in top-to-bottom draw order.
#' @keywords internal
forest_rows <- function(stats, ref_layout = NULL, metric = NULL) {
  rows <- stats$stats
  if (!is.null(metric)) {
    rows <- rows[as.character(rows[[stats$config$metric_name_var]]) == metric, , drop = FALSE]
  }
  ## Resolved per metric: a metric whose covariates have no `cov_level_ref`
  ## entry keeps its reference panel.
  has_distributed <- any(rows$layout == "distributed")
  if (is.null(ref_layout)) {
    ref_layout <- if (has_distributed) "distributed" else "consolidated"
  }
  if (ref_layout == "distributed" && !has_distributed) {
    rlang::abort(message = paste0(
      "`ref_layout = \"distributed\"` needs per-covariate reference rows",
      if (!is.null(metric)) paste0(" for metric `", metric, "`"),
      "; pass `cov_level_ref` naming its covariates to `df_forest()`."
    ))
  }
  rows <- rows[rows$layout %in% c("both", ref_layout), , drop = FALSE]
  rownames(rows) <- NULL
  rows
}


#' Build a forest plot ggplot from a `forest_stats` object
#'
#' @description
#' Constructs a covariate forest plot from a [df_forest()] result (or any
#' object satisfying the `forest_stats` contract). Renders per-row point
#' estimates with horizontal CI lines, optional shaded equivalence band, and
#' a vertical reference line, one facet row per covariate name. The
#' `est_label`, `lo_label`, and `hi_label` columns are assembled into the
#' right-hand secondary y-axis text (`"1.29 (1.19, 1.41)"`) under a header
#' built from the container's `ci` (e.g. `"Estimate (90% CI)"`), so each
#' row's estimate sits beside its point. The header is the plot subtitle,
#' right-aligned so it sits on the top edge of the panels above the column;
#' change it with `+ ggplot2::labs(subtitle = "Ratio (90% CI)")`. The
#' rendered metric labels the x-axis, and the plot has no title by default.
#'
#' Rows are drawn in the order they appear in `stats$stats`, top to bottom
#' (see the "Row order" section of [df_forest()]); reorder that frame to
#' change the plot. Each row is its own y position, so covariate level
#' labels may repeat across panels (`Low` / `High` under two covariates).
#' The `layout` column selects the rows for the chosen reference layout (see
#' `ref_layout`).
#'
#' Rows flagged `is_ref` (the reference copies in each covariate panel, or the
#' reference panel) draw through the `ref_point` style role
#' (an open circle by default) and show the point estimate alone in the
#' estimate column, since their CI collapses onto the estimate. A row whose
#' level label repeats its covariate name (the reference panel) has no
#' left-axis tick or label, since the strip already carries the name.
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
#'    [df_forest()]), checked on entry; see "Building a `forest_stats` by
#'    hand" for the required columns and config.
#' @param metric Character scalar identifying which value of `metric_name_var` to
#'    render. `plot_forest()` renders one metric per call (no metric
#'    facet); the covariate name is the only facet dimension. If `NULL`
#'    (default) and `stats$stats` contains exactly one unique value of
#'    `metric_name_var`, that value is used. If `NULL` and multiple metrics
#'    are present, aborts and asks the caller to pick one.
#' @param style A [ggstylekit::style_spec()]; default [style_forest()], which
#'    documents the roles, per-covariate coloring, and theme elements.
#'    Alternatives: [style_forest_pmforest()], [style_forest_coveffectsplot()],
#'    [style_forest_pmxforest()], [style_forest_pumas()].
#' @param ref The x-intercept of the vertical reference line: a numeric
#'    scalar, `NULL` to suppress the line, or `"reference"` (default) to use
#'    the estimate of the metric's reference row (the row flagged `is_ref`;
#'    exactly `1` for a ratio metric, the reference exposure for a raw
#'    metric). With `"reference"` and no reference row in the plotted
#'    metric, a warning is issued and no line is drawn.
#' @param ref_band Length-2 increasing numeric vector (`ref_band[1] <
#'    ref_band[2]`) giving the equivalence band, read according to
#'    `ref_band_type`. Default `NULL` (no band).
#' @param ref_band_type How `ref_band` is placed around the resolved `ref`:
#'    `"multiplicative"` (default) draws `ref * ref_band`, so `c(0.8, 1.25)`
#'    gives the bioequivalence bounds on a ratio or a raw exposure scale;
#'    `"additive"` draws `ref + ref_band`, so `c(-10, 10)` brackets a
#'    reference change of zero for a difference metric such as percent
#'    change from baseline; `"absolute"` draws `ref_band` as given. The
#'    first two require a resolved `ref`; multiplicative limits must be
#'    positive.
#' @param ref_layout `"distributed"` draws the reference row inside each
#'    covariate panel named in `cov_level_ref`; `"consolidated"` draws it once
#'    in the reference panel at the top. Default `NULL`: distributed if
#'    [df_forest()] was given `cov_level_ref` for a covariate of `metric`,
#'    else consolidated.
#' @param show_rep Logical. Display the number of replicates (the `n`
#'    column of the plotted rows) as a plot caption. Warns, and shows the
#'    range, when rows of the metric have different counts. Default `FALSE`.
#'    A `caption` set in the style is drawn instead, whatever this value.
#'
#' @section Building a `forest_stats` by hand:
#' Summaries computed elsewhere can be plotted by wrapping them with
#' [pmx_stats()] and `subclass = "forest_stats"` (see the last example).
#' `stats` needs one row per plotted point, in display order, with:
#' * the metric, covariate name, and covariate level columns named in
#'   `config` (character);
#' * `est`, `lo`, `hi` (numeric; the point and interval);
#' * `est_label`, `lo_label`, `hi_label` (character; the plotted text) and
#'   `ci_label` (character; their combined form);
#' * `is_ref` (logical, no `NA`; the reference rows);
#' * `layout`: `"both"` for covariate rows, `"consolidated"` for a
#'   reference-panel row, `"distributed"` for a reference copy inside a
#'   covariate panel;
#' * optionally `n` (integer draw count), needed only for `show_rep`.
#'
#' `config` needs `metric_name_var`, `cov_name_var`, and `cov_level_var`
#' (the column names), `ci` (in `(0, 1)`, used in the header), and
#' `sigdigits` (a whole number `>= 1`). The object is checked when plotted.
#'
#' @family forest plot
#' @return a `ggplot` plot object
#' @export plot_build_forest
#'
#' @examples
#' stats <- df_forest(
#'   data_sad_pkforest,
#'   cov_level_ref = attr(data_sad_pkforest, "cov_level_ref")
#' )
#' plot_build_forest(stats, metric = "AUCRATIO")
#' plot_build_forest(stats, metric = "AUCRATIO", ref_band = c(0.8, 1.25))
#' # The same stats with the reference panel
#' plot_build_forest(stats, metric = "AUCRATIO", ref_layout = "consolidated")
#' # Raw exposure: the line sits at the reference exposure and the band is
#' # 0.8 to 1.25 times it
#' plot_build_forest(stats, metric = "AUC", ref_band = c(0.8, 1.25))
#' # Difference metric: percent change from the reference, with an additive
#' # band around the reference change of 0
#' pchg <- dplyr::filter(data_sad_pkforest, metric == "AUCRATIO") |>
#'   dplyr::mutate(metric = "AUCPCHG", value = 100 * (value - 1))
#' plot_build_forest(df_forest(pchg), ref_band = c(-20, 25),
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
#' # A summary built by hand
#' manual <- pmx_stats(
#'   stats = data.frame(
#'     metric = "AUCRATIO",
#'     cov_name = c("Reference", "WTBL", "WTBL"),
#'     cov_level = c("Reference", "50 kg", "90 kg"),
#'     est = c(1, 1.29, 0.83), lo = c(1, 1.19, 0.77), hi = c(1, 1.41, 0.88),
#'     est_label = c("1.00", "1.29", "0.83"),
#'     lo_label = c("1.00", "1.19", "0.77"),
#'     hi_label = c("1.00", "1.41", "0.88"),
#'     ci_label = c("1.00 (1.00, 1.00)", "1.29 (1.19, 1.41)", "0.83 (0.77, 0.88)"),
#'     is_ref = c(TRUE, FALSE, FALSE),
#'     layout = c("consolidated", "both", "both")
#'   ),
#'   config = list(metric_name_var = "metric", cov_name_var = "cov_name",
#'                 cov_level_var = "cov_level", ci = 0.9, sigdigits = 3),
#'   subclass = "forest_stats"
#' )
#' plot_build_forest(manual)

plot_build_forest <- function(
  stats,
  metric = NULL,
  style = NULL,
  ref = "reference",
  ref_band = NULL,
  ref_band_type = c("multiplicative", "additive", "absolute"),
  ref_layout = NULL,
  show_rep = FALSE
) {
  validate_forest_stats(stats)
  check_ref(ref)
  check_ref_layout(ref_layout)
  check_boolean(show_rep, "show_rep")
  ref_band_type <- match.arg(ref_band_type)
  check_ref_band(ref_band, ref_band_type)
  plotstyle <- resolve_style(style, style_forest)

  metric_name_var_str <- stats$config$metric_name_var
  cov_name_var_str <- stats$config$cov_name_var
  cov_level_var_str <- stats$config$cov_level_var

  metric_col <- as.character(stats$stats[[metric_name_var_str]])
  metric <- check_forest_metric(metric, unique(metric_col), metric_name_var_str)
  plot_data <- forest_rows(stats, ref_layout, metric)

  resolved <- forest_resolve_ref(ref, ref_band, ref_band_type, plot_data,
                                 metric, stats$config$sigdigits)
  ref <- resolved$ref
  ref_band <- resolved$ref_band

  ## Rows draw top to bottom in `stats$stats` order. `.row` is the y key, one
  ## level per row, so cov_level labels may repeat across panels; the axis
  ## text is supplied positionally in level order (bottom to top).
  row_ids <- as.character(seq_len(nrow(plot_data)))
  plot_data$.row <- factor(row_ids, levels = rev(row_ids))
  cov_levels <- unique(as.character(plot_data[[cov_name_var_str]]))
  plot_data[[cov_name_var_str]] <- factor(plot_data[[cov_name_var_str]], levels = cov_levels)
  test_data <- plot_data[!plot_data$is_ref, , drop = FALSE]
  ref_data <- plot_data[plot_data$is_ref, , drop = FALSE]

  colors <- forest_color_plan(plotstyle, test_data, ref_data, cov_name_var_str)
  plotstyle <- colors$style
  has_panel_color <- colors$has_panel_color
  pt_mapping <- colors$pt_mapping
  eb_mapping <- colors$eb_mapping
  pt_layer_mapping <- colors$pt_layer_mapping
  pt_outline <- colors$pt_outline

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

  ## The reference line is a tagged `ref_line` role layer.
  if (!is.null(ref)) {
    base <- base +
      ggstylekit::series_layer(ggplot2::geom_vline(xintercept = ref), "ref_line")
  }

  ## Intervals are always error bars, so one layer serves capped and uncapped
  ## styles. style_plot() fills the cap height (in row heights) from the style's
  ## `errorbar_width`; unset, it is back-filled to 0, an uncapped line.
  if (is.null(plotstyle$errorbar_width)) {
    plotstyle <- ggstylekit::set_style(plotstyle, errorbar_width = 0)
  }
  base <- base +
    ggstylekit::series_layer(
      ggplot2::geom_errorbar(eb_mapping, data = test_data, orientation = "y"),
      "errorbar"
    ) +
    ggstylekit::series_layer(
      do.call(ggplot2::geom_point, compact(list(
        mapping = pt_layer_mapping, data = test_data, colour = pt_outline
      ))),
      "point"
    )
  ## style_plot() fills a point's colour/shape/size/alpha from the role, but
  ## not its `fill`, so the open-shape fill is set inline from the style.
  ## When test points carry the covariate as fill, a fill-shape reference
  ## point keeps a role-colour outline to match them, not the covariate colour.
  rp <- series_aes(plotstyle, "ref_point")
  rp_mapping <- pt_mapping
  rp_outline <- NULL
  if (!is.null(pt_outline) && isTRUE(rp$shape %in% 21:25)) {
    rp_mapping <- NULL
    rp_outline <- rp$colour %||% pt_outline
  }
  if (nrow(ref_data) > 0L) {
    rp_args <- compact(list(
      mapping = rp_mapping,
      data = ref_data,
      colour = rp_outline,
      fill = rp$fill
    ))
    base <- base +
      ggstylekit::series_layer(do.call(ggplot2::geom_point, rp_args), "ref_point")
  }

  ## Left axis: the covariate level labels, dropped (label and tick) where the
  ## level repeats the covariate name (the strip already says it, e.g. a
  ## reference panel). Right axis: the estimate column (reference rows show
  ## the estimate alone), under a header ("Estimate (90% CI)") drawn as the
  ## right-aligned plot subtitle, the row directly above the panels (a
  ## secondary-axis *title* would land beside the labels instead).
  level_labels <- as.character(plot_data[[cov_level_var_str]])
  show_level <- level_labels != as.character(plot_data[[cov_name_var_str]])
  est_labels <- ifelse(
    plot_data$is_ref,
    plot_data$est_label,
    paste0(plot_data$est_label, " (", plot_data$lo_label, ", ", plot_data$hi_label, ")")
  )
  header <- paste0("Estimate (", format(stats$config$ci * 100), "% CI)")
  base <- base +
    ggplot2::scale_y_discrete(
      breaks = rev(row_ids[show_level]),
      labels = rev(level_labels[show_level]),
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

  if (isTRUE(show_rep)) {
    if (is.null(plot_data$n)) {
      rlang::abort(message = "`show_rep = TRUE` needs an `n` column in `stats$stats`.")
    }
    n_rep <- range(plot_data$n)
    if (n_rep[1] != n_rep[2]) {
      short <- plot_data[plot_data$n < n_rep[2], , drop = FALSE]
      short <- unique(paste0(short[[cov_name_var_str]], " ", short[[cov_level_var_str]],
                             " (n = ", short$n, ")"))
      rlang::warn(paste0(
        "metric `", metric, "` rows have different replicate counts; fewer than ",
        n_rep[2], " in: ", paste(short, collapse = "; ")
      ))
    }
    base <- base + ggplot2::labs(caption = paste0(
      "Replicates = ", paste(unique(n_rep), collapse = "-")
    ))
  }

  ## style_plot() builds the covariate colour scale from the `colors` entries
  ## (covariates without one take the house palette); the facet strips
  ## already name each covariate, so the legend is dropped.
  if (has_panel_color) base <- base + ggplot2::guides(colour = "none", fill = "none")

  ggstylekit::style_plot(base, plotstyle)
}


#' Internal: resolve the forest reference value and place the reference band
#'
#' @param ref,ref_band,ref_band_type As in [plot_build_forest()]
#'    (`ref_band_type` already matched).
#' @param plot_data Rows drawn for `metric`, from `forest_rows()`.
#' @param metric The plotted metric.
#' @param sigdigits Label digits, for the multiple-reference error.
#' @return `list(ref, ref_band)`: the numeric reference (or `NULL`) and the
#'    band on the data scale (or `NULL`).
#' @noRd
forest_resolve_ref <- function(ref, ref_band, ref_band_type, plot_data, metric,
                               sigdigits) {
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
        paste(fmt_sig(ref, sigdigits), collapse = ", "),
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
  list(ref = ref, ref_band = ref_band)
}


#' Internal: per-covariate colour plan for a forest plot
#'
#' @param plotstyle The resolved forest `style_spec`.
#' @param test_data,ref_data Test and reference rows being drawn.
#' @param cov_name_var_str Covariate column name.
#' @return `list(style, has_panel_color, pt_mapping, eb_mapping,
#'    pt_layer_mapping, pt_outline)`: the style with palette, reference-panel,
#'    and fill entries filled in, and the point/error bar mappings.
#' @noRd
forest_color_plan <- function(plotstyle, test_data, ref_data, cov_name_var_str) {
  ## Per-covariate coloring: a palette `function(n)` in `colors`, or entries
  ## keyed by a covariate name present in the plotted data, switch
  ## point/errorbar color to a covariate mapping. Only covariates with test
  ## rows are colored, so the reference panel takes no palette slot
  ## and colors match across `ref_layout`. A palette is resolved to named
  ## covariate entries here so the fill map below can reuse them; a palette
  ## returning fewer than n colors is recycled.
  color_levels <- unique(as.character(test_data[[cov_name_var_str]]))
  if (is.function(plotstyle$colors)) {
    pal <- plotstyle$colors(length(color_levels))
    if (is.null(names(pal))) pal <- stats::setNames(rep_len(pal, length(color_levels)), color_levels)
    if (!any(color_levels %in% names(pal))) {
      rlang::warn(paste0(
        "The `colors` palette function returned names matching no covariate (",
        paste(color_levels, collapse = ", "), "); covariates are not colored. ",
        "Return an unnamed vector, or name entries by covariate."
      ))
    }
    plotstyle <- ggstylekit::set_style(plotstyle, colors = pal)
  }
  has_panel_color <- any(color_levels %in% names(plotstyle$colors))
  ## The reference panel keeps the `ref_point`
  ## role colour unless `colors` names it.
  ref_only <- setdiff(unique(as.character(ref_data[[cov_name_var_str]])), color_levels)
  ref_only <- setdiff(ref_only, names(plotstyle$colors))
  if (has_panel_color && length(ref_only) > 0L) {
    ref_colors <- plotstyle$colors
    ref_colors[ref_only] <- series_aes(plotstyle, "ref_point")$colour %||% "black"
    plotstyle <- ggstylekit::set_style(plotstyle, colors = ref_colors)
  }
  pt_mapping <- if (has_panel_color) ggplot2::aes(color = .data[[cov_name_var_str]])
  eb_mapping <- ggplot2::aes(xmin = .data[["lo"]], xmax = .data[["hi"]], !!!pt_mapping)

  ## A fill shape (21-25) carries the covariate color as its fill, keeping the
  ## `point` role colour as the outline; the covariate colors are copied into
  ## the fill map so the fill scale matches the error bars.
  pt_layer_mapping <- pt_mapping
  pt_outline <- NULL
  if (has_panel_color && isTRUE(series_aes(plotstyle, "point")$shape %in% 21:25)) {
    pt_layer_mapping <- ggplot2::aes(fill = .data[[cov_name_var_str]])
    pt_outline <- series_aes(plotstyle, "point")$colour %||% "black"
    cov_colors <- plotstyle$colors[intersect(color_levels, names(plotstyle$colors))]
    fill_map <- if (is.function(plotstyle$fill)) NULL else plotstyle$fill
    fill_map[names(cov_colors)] <- cov_colors
    plotstyle <- ggstylekit::set_style(plotstyle, fill = fill_map)
  }
  list(style = plotstyle, has_panel_color = has_panel_color,
       pt_mapping = pt_mapping, eb_mapping = eb_mapping,
       pt_layer_mapping = pt_layer_mapping, pt_outline = pt_outline)
}
