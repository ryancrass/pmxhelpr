#' Build a VPC ggplot from a `vpc_stats` object
#'
#' @description
#' Constructs a ggplot2 VPC plot from a [df_vpcstats()] result (or any object
#' satisfying the `vpc_stats` contract). Applies the `min_bin_count` filter,
#' draws the simulated and observed quantile layers, overlays the observation
#' scatter, draws the LOQ reference line, applies stratification facets, and
#' adds the replicates caption and panel theme. The `type` argument selects
#' the continuous (concentration quantile) or censored (BLQ proportion) layer
#' set; the `pcvpc` argument selects the standard or prediction-corrected
#' column set within the continuous type.
#'
#' Most users will reach this function indirectly via [plot_vpc_cont()] or
#' [plot_vpc_cens()]. Call `plot_build_vpc()` directly when working from a
#' manually-constructed or cached `vpc_stats` object — for example, plotting
#' a precomputed result from disk or a custom pipeline that produces
#' compatible columns.
#'
#' Stratification and binning are read from the container: facets come from
#' `stats$config$strat_var` and the x-axis from the `BIN_MID` column. To
#' stratify, pass `strat_var` to [df_vpcstats()].
#'
#' @param stats A `vpc_stats` object (typically the output of
#'    [df_vpcstats()]). Must contain `stats` and `obs` data.frames with the
#'    columns documented in [df_vpcstats()]. Validated by
#'    `validate_vpc_stats()` at entry.
#' @param type One of `"cont"` (default) or `"cens"`. `"cont"` plots the
#'    continuous concentration VPC (quantile ribbons and obs scatter).
#'    `"cens"` plots the BLQ-proportion VPC (proportion CI ribbon plus
#'    observed per-bin proportion line/points); requires `sim_prop_blq_*`
#'    columns in `stats$stats` (i.e., `df_vpcstats()` was called with
#'    a LOQ source). `type = "cens"` is incompatible with `pcvpc = TRUE`.
#' @param min_bin_count Minimum number of observations per bin required for
#'    inclusion in binned plot layers. For `type = "cont"`, the threshold is
#'    applied to *quantifiable* obs only (`obs_n - obs_n_blq`); BLQ-encoded
#'    records do not count toward it. For `type = "cens"`, the threshold is
#'    applied to total obs (`obs_n`) — BLQ-heavy bins are the most
#'    informative on a cens VPC and are retained.
#' @param show_rep Logical. Display replicate count as a plot caption.
#'    A `caption` set in
#'    the style is drawn instead, whatever this value.
#' @param shown Layer visibility settings created by [plot_vpc_shown()].
#'    For `type = "cens"`, only the `obs_point`, `obs_median_line`,
#'    `sim_median_line`, and `sim_median_ci` keys are read. Defaults follow
#'    `plot_vpc_shown()`: observed proportion line/points and simulated CI
#'    ribbon are shown; the simulated median line is off by default (pass
#'    `plot_vpc_shown(sim_median_line = TRUE)` to enable it).
#' @param style A [ggstylekit::style_spec()] controlling plot aesthetics.
#'    Defaults to [style_vpc()]; view the defaults by running `style_vpc()`
#'    with no arguments. Customize by passing `style = style_vpc(...)`.
#'    Series names are the VPC roles. For
#'    `type = "cens"`, only the keys corresponding to the four cens layers
#'    above are read; other keys are ignored. The cens `obs_point` color is
#'    inherited from `obs_median_line` (so points match the obs line); the
#'    `obs_point` style key still controls shape/alpha/size.
#' @param pcvpc Logical. When `TRUE`, plot the prediction-corrected columns
#'    (`pc_*` for stats, `PC_OBSDV` for the obs scatter) and suppress the
#'    LOQ reference line. Default is `FALSE` (standard VPC). Only meaningful
#'    when `type = "cont"`. Not available when `PRED` differs by replicate
#'    (simulations with parameter uncertainty, see the `params` argument of
#'    [df_mrgsim_replicate()]).
#' @param loq Numeric scalar or vector of LOQ values for the reference line,
#'    or `NULL` to suppress. One line is drawn per panel, at the highest LLOQ
#'    when there are several (the most censoring). When omitted (inherited
#'    from `stats$config$loq`), the container is stratified
#'    (`stats$config$strat_var`), and `stats$obs` carries a row-aligned `LOQ`
#'    column, each facet draws the highest LLOQ of its own strat-level rows.
#'    When `loq` is supplied explicitly, the value always wins and one global
#'    reference line is drawn at `max(loq)` (the per-facet column-based
#'    dispatch is reserved for the inherited case). In the no-stratification
#'    or no-`obs$LOQ` cases, one global line is drawn at the highest `loq`
#'    regardless of inheritance.
#'    The legend entry for LLOQ is *not* attached here — pass the same
#'    value to [plot_vpc_legend()] when composing the legend panel, or pass
#'    the container as `plot_vpc_legend(stats = )`, which reads
#'    `stats$config$loq`.
#'    `stats$config$loq` is already a vector of unique non-NA LOQ
#'    values when [df_vpcstats()] populated the container. Forced to `NULL`
#'    when `pcvpc = TRUE` (LOQ has no meaning on the prediction-corrected
#'    scale).
#' @param bin_style How the simulated intervals are drawn: `"line"`
#'    (default) as shaded ribbons through the bin positions (`BIN_MID`);
#'    `"rect"` as shaded rectangles spanning each bin (`BIN_LOW` to
#'    `BIN_HI`). The observed and simulated lines are drawn through the bin
#'    positions either way; with `"rect"`, each line also gets a point at
#'    every bin position, in the line's colour. `"rect"` needs interval bins:
#'    compute the stats with `bin_breaks`.
#'
#' @inheritParams plot_dvtime
#' @family vpc
#' @seealso [autoplot()][autoplot.pmx_stats], which draws a container with its builder.
#' @return A `ggplot2` object.
#' @export plot_build_vpc
#'
#' @examples
#' model <- model_mread_load(model = "pkmodel")
#' simout <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT %in% c(1, 2)),
#'                               model = model, replicates = 5,
#'                               dv_var = ODV, carry_out = "FOOD")
#' out <- df_vpcstats(simout, strat_var = FOOD)
#' p <- plot_build_vpc(out, pcvpc = FALSE)

plot_build_vpc <- function(stats,
                           type = c("cont", "cens"),
                           min_bin_count = 0,
                           show_rep = TRUE,
                           shown = NULL,
                           style = NULL,
                           pcvpc = FALSE,
                           loq = NULL,
                           time_unit = "hours",
                           bin_style = c("line", "rect")) {

  type <- match.arg(type)
  bin_style <- match.arg(bin_style)
  check_boolean(show_rep, "show_rep")
  check_boolean(pcvpc, "pcvpc")
  time_unit <- normalize_time_unit(time_unit)

  validate_vpc_stats(stats)

  if (bin_style == "rect" && !all(.vpc_bin_bounds %in% names(stats$stats))) {
    pmx_abort(c(
      "{.code bin_style = \"rect\"} needs interval bins.",
      "i" = "Compute the stats with {.arg bin_breaks} (e.g. {.code bin_breaks = bins_kmeans(8)})."
    ), "bad_arg")
  }

  if (type == "cens") {
    if (is.null(stats$config$loq)) {
      pmx_abort(c(
        "{.code type = \"cens\"} requires a LOQ source.",
        "i" = "Call {.fn df_vpcstats} with {.code loq = <numeric>}, or include an {.var LLOQ} column in {.arg data}."
      ), "bad_stats")
    }
    if (isTRUE(pcvpc)) {
      pmx_abort("{.code pcvpc = TRUE} is not supported for {.code type = \"cens\"}; LOQ has no meaning on the prediction-corrected scale.",
                "bad_arg")
    }
  }

  if (isTRUE(pcvpc) && isFALSE(stats$config$pred_corrected)) {
    pmx_abort(c(
      "{.code pcvpc = TRUE} is not available: {.var PRED} differs by replicate.",
      "i" = "Prediction correction is not appropriate with parameter uncertainty ({.code df_mrgsim_replicate(params = )}); plot the standard VPC."
    ), "bad_stats")
  }

  ## Stratification and binning are fixed by df_vpcstats(); facets and x
  ## positions must match the summary stats, so both come from the container.
  strat_var_str <- stats$config$strat_var

  ## LOQ dispatch: missing() distinguishes "not passed" (inherit from config),
  ## "explicit NULL" (suppress ref line), and "explicit value" (override). The
  ## inherited flag is preserved so the per-facet column-based rendering in
  ## the cont internal is reserved for the inherited case; an explicit `loq`
  ## always draws a global hline.
  loq_inherited <- missing(loq)
  if (loq_inherited) loq <- stats$config$loq

  shown    <- resolve_shown(shown, plot_vpc_shown())
  vpcstyle <- resolve_style(style, style_vpc)
  if (type == "cens") vpcstyle <- vpc_cens_style(vpcstyle)

  plot <- switch(type,
    cont = plot_build_vpc_cont(stats, min_bin_count, shown, vpcstyle,
                               pcvpc, loq, loq_inherited,
                               strat_var_str, bin_style),
    cens = plot_build_vpc_cens(stats, min_bin_count, shown, vpcstyle, bin_style)
  )

  ## Stratification facets are built here rather than by ggstylekit (the
  ## strat var must match the summary stats), so the style's facet layout
  ## fields are read off the spec and forwarded explicitly.
  if (!is.null(strat_var_str)) {
    plot <- plot +
      ggplot2::facet_wrap(ggplot2::vars(.data[[strat_var_str]]),
                          nrow   = vpcstyle$facet_nrow,
                          ncol   = vpcstyle$facet_ncol,
                          scales = vpcstyle$facet_scales %||% "fixed")
  }

  ## Default axis labels follow the dvtime/gof convention (standardized names);
  ## a user's `+ labs()` or the style's xlabel/ylabel still wins.
  y_label <- if (type == "cens") {
    "Proportion BLQ"
  } else if (isTRUE(pcvpc)) {
    "Prediction-corrected DV"
  } else {
    "DV"
  }
  plot <- plot + ggplot2::labs(x = "TIME", y = y_label)

  if (isTRUE(show_rep)) {
    plot <- plot +
      ggplot2::labs(caption = paste0(
        "Replicates = ", stats$config$n_replicates))
  }

  ## Finalize via style_plot(): it fills each tagged role layer's aesthetics
  ## from the style and applies the house theme (white VPC panel).
  as_pmx_vpc_plot(style_plot_time(plot, vpcstyle, time_unit))
}


## Internal: build the continuous (concentration quantile) VPC plot.
## Walks the role table (vpc_roles.R) in drawing order: each shown role adds
## its tagged statistic layers, then the LOQ reference line and the obs
## scatter at their table positions. Aesthetics come from the style via
## style_plot(); facets, labels, caption, panel theme, and class are added by
## the dispatcher.
##
## @noRd
plot_build_vpc_cont <- function(stats, min_bin_count, shown, vpcstyle,
                                pcvpc, loq, loq_inherited,
                                strat_var_str, bin_style = "line") {

  ## min_bin_count predicate for cont: only quantifiable obs count
  ## (BLQ-encoded records excluded).
  vpcstats <- dplyr::filter(stats$stats,
                            (.data$obs_n - .data$obs_n_blq) >= min_bin_count)
  obs <- stats$obs

  ## Column-name dispatch: std flavor uses unprefixed stats columns and
  ## OBSDV in the obs frame; pc flavor uses pc_* stats columns and PC_OBSDV
  ## in the obs frame.
  prefix <- if (isTRUE(pcvpc)) "pc_" else ""
  col <- function(x) paste0(prefix, x)
  obs_y_col <- if (isTRUE(pcvpc)) "PC_OBSDV" else "OBSDV"

  ## LOQ ref line is meaningless on the pc scale; force off.
  if (isTRUE(pcvpc)) loq <- NULL

  plot <- ggplot2::ggplot(vpcstats, ggplot2::aes(x = .data[[.vpc_bin_col]]))

  for (role in names(.vpc_roles)) {
    if (!isTRUE(shown[[role]])) next
    if (role == "loq_line") {
      ## One line per panel at the highest LLOQ: per facet when the LLOQ is
      ## inherited per row and the plot is stratified, otherwise one global
      ## line.
      if (is.null(loq)) next
      lines <- if (loq_inherited && !is.null(strat_var_str) && "LOQ" %in% colnames(obs)) {
        loq_lines(data = obs, by = strat_var_str)
      } else {
        loq_lines(loq)
      }
      plot <- plot + loq_line_layer(lines, vpcstyle)
    } else if (role == "obs_point") {
      if (nrow(obs) == 0) next
      plot <- plot + ggstylekit::series_layer(
        ggplot2::geom_point(ggplot2::aes(y = .data[[obs_y_col]], x = .data$TIME),
                            data = obs, inherit.aes = FALSE),
        "obs_point")
    } else {
      plot <- plot + vpc_role_layers(role, "cont", col, bin_style)
    }
  }
  plot
}


## Internal: build the censored (BLQ-proportion) VPC plot. Walks the roles
## with a `cens` column set (sim_median_ci, sim_median_line, obs_median_line)
## plus the per-bin obs points, and applies coord_cartesian; facets, labels,
## caption, panel theme, and class are added by the dispatcher.
##
## @noRd
plot_build_vpc_cens <- function(stats, min_bin_count, shown, vpcstyle,
                                bin_style = "line") {

  ## min_bin_count predicate for cens: applied to total obs. BLQ-heavy bins
  ## are the most informative on a cens VPC and are retained.
  vpcstats <- dplyr::filter(stats$stats, .data$obs_n >= min_bin_count)

  plot <- ggplot2::ggplot(vpcstats, ggplot2::aes(x = .data[[.vpc_bin_col]]))

  for (role in names(.vpc_roles)) {
    if (!isTRUE(shown[[role]])) next
    if (role == "obs_point") {
      ## Per-bin points at obs_prop_blq (from stats, not obs); their colour
      ## follows the obs line (see vpc_cens_style()).
      plot <- plot + ggstylekit::series_layer(
        ggplot2::geom_point(ggplot2::aes(y = .data[["obs_prop_blq"]])), "obs_point")
    } else if (!is.null(.vpc_roles[[role]]$cens)) {
      ## In rect style, lines get bin-position points; the obs line's are
      ## already drawn by obs_point when it is shown.
      mid_points <- bin_style == "rect" &&
        !(role == "obs_median_line" && isTRUE(shown[["obs_point"]]))
      plot <- plot + vpc_role_layers(role, "cens", bin_style = bin_style,
                                     mid_points = mid_points)
    }
  }

  ## Cens proportions are bounded [0, 1]. Use coord_cartesian (not
  ## scale_y_continuous limits) so boundary values aren't dropped; users
  ## can still override by adding their own coord_*() / scale_y_*() layer.
  plot + ggplot2::coord_cartesian(ylim = c(0, 1))
}


## Internal: the style for a censored VPC. `logy` is ignored, with a warning:
## the proportion BLQ is drawn on a fixed 0-1 axis. Its obs points show the
## same per-bin statistic as the obs line, so they take the line's colour
## (the obs_point entry still controls shape/alpha/size).
##
## @noRd
vpc_cens_style <- function(style) {
  if (isTRUE(style$logy)) {
    warn_vpc_cens_logy()
    style <- ggstylekit::set_style(style, logy = FALSE)
  }
  colors <- style$colors
  if (!is.character(colors) || !"obs_median_line" %in% names(colors)) return(style)
  colors[["obs_point"]] <- colors[["obs_median_line"]]
  ggstylekit::set_style(style, colors = colors)
}
