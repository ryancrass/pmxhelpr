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
#' @param compute_out A `vpc_stats` object (typically the output of
#'    [df_vpcstats()]). Must contain `stats` and `obs` data.frames with the
#'    columns documented in [df_vpcstats()]. Validated by
#'    `validate_vpc_stats()` at entry.
#' @param type One of `"cont"` (default) or `"cens"`. `"cont"` plots the
#'    continuous concentration VPC (quantile ribbons and obs scatter).
#'    `"cens"` plots the BLQ-proportion VPC (proportion CI ribbon plus
#'    observed per-bin proportion line/points); requires `sim_prop_blq_*`
#'    columns in `compute_out$stats` (i.e., `df_vpcstats()` was called with
#'    a LOQ source). `type = "cens"` is incompatible with `pcvpc = TRUE`.
#' @param min_bin_count Minimum number of observations per bin required for
#'    inclusion in binned plot layers. For `type = "cont"`, the threshold is
#'    applied to *quantifiable* obs only (`obs_n - obs_n_blq`); BLQ-encoded
#'    records do not count toward it. For `type = "cens"`, the threshold is
#'    applied to total obs (`obs_n`) — BLQ-heavy bins are the most
#'    informative on a cens VPC and are retained.
#' @param show_rep Logical. Display replicate count as a plot caption.
#' @param shown Layer visibility settings created by [plot_vpc_shown()].
#'    For `type = "cens"`, only the `obs_point`, `obs_median_line`,
#'    `sim_median_line`, and `sim_median_ci` keys are read. Defaults follow
#'    `plot_vpc_shown()`: observed proportion line/points and simulated CI
#'    ribbon are shown; the simulated median line is off by default (pass
#'    `plot_vpc_shown(sim_median_line = TRUE)` to enable it).
#' @param theme Aesthetic parameters created by [plot_vpc_theme()]. For
#'    `type = "cens"`, only the keys corresponding to the four cens layers
#'    above are read; other keys are ignored. The cens `obs_point` color is
#'    inherited from `obs_median_line` (so points match the obs line); the
#'    `obs_point` theme key still controls shape/alpha/size.
#' @param pcvpc Logical. When `TRUE`, plot the prediction-corrected columns
#'    (`pc_*` for stats, `PC_OBSDV` for the obs scatter) and suppress the
#'    LOQ reference line. Default is `FALSE` (standard VPC). Only meaningful
#'    when `type = "cont"`.
#' @param loq Numeric scalar or vector of LOQ values for the reference line,
#'    or `NULL` to suppress. When omitted (inherited from
#'    `compute_out$config$loq`) and `strat_var` is set and `compute_out$obs`
#'    carries a row-aligned `LOQ` column, ref lines are drawn per facet —
#'    each facet shows only the LLOQ values applicable to its strat-level
#'    rows. When `loq` is supplied explicitly, the value always wins and
#'    one global reference line is drawn per unique value of `loq` (the
#'    per-facet column-based dispatch is reserved for the inherited case).
#'    In the no-stratification or no-`obs$LOQ` cases, a global reference
#'    line is drawn per unique value of `loq` regardless of inheritance.
#'    The legend entry for LLOQ is *not* attached here — pass the same
#'    value to [plot_vpc_legend()] when composing the legend panel.
#'    `compute_out$config$loq` is already a vector of unique non-NA LOQ
#'    values when [df_vpcstats()] populated the container. Forced to `NULL`
#'    when `pcvpc = TRUE` (LOQ has no meaning on the prediction-corrected
#'    scale).
#' @param strat_var Stratification variable. Accepts bare names or strings.
#'    Default is `NULL`. When `NULL`, the value is read from
#'    `compute_out$config$strat_var` so output of [df_vpcstats()] is
#'    handled automatically.
#' @param bin_var String. Binning variable name. Default is `"BIN_MID"`.
#'
#' @family vpc
#' @return A `ggplot2` object.
#' @export plot_build_vpc
#'
#' @examples
#' model <- model_mread_load(model = "pkmodel")
#' simout <- df_mrgsim_replicate(data = data_sad, model = model, replicates = 5,
#'                               dv_var = ODV, carry_out = "FOOD")
#' out <- df_vpcstats(simout, strat_var = FOOD)
#' p <- plot_build_vpc(out, pcvpc = FALSE)

plot_build_vpc <- function(compute_out,
                           type = c("cont", "cens"),
                           min_bin_count = 1,
                           show_rep = TRUE,
                           shown = NULL,
                           theme = NULL,
                           pcvpc = FALSE,
                           loq = NULL,
                           strat_var = NULL,
                           bin_var = BIN_MID_VAR) {

  type <- match.arg(type)

  validate_vpc_stats(compute_out)

  if (type == "cens" && isTRUE(pcvpc)) {
    rlang::abort(
      "`pcvpc = TRUE` is not supported for `type = \"cens\"`; LOQ has no meaning on the prediction-corrected scale."
    )
  }
  if (type == "cens" && !"sim_prop_blq_med" %in% colnames(compute_out$stats)) {
    rlang::abort(
      "`type = \"cens\"` requires `sim_prop_blq_*` columns in `compute_out$stats`; call `df_vpcstats()` with a LOQ source (`loq` arg or `LLOQ` column)."
    )
  }

  ## Strat var dispatch: explicit user input takes precedence; otherwise
  ## inherit from the container's config slot (set by df_vpccompute).
  strat_var_str <- resolve_var(rlang::enquo(strat_var), nullable = TRUE)
  if (is.null(strat_var_str)) {
    strat_var_str <- compute_out$config$strat_var
  }

  ## LOQ dispatch: missing() distinguishes "not passed" (inherit from config),
  ## "explicit NULL" (suppress ref line), and "explicit value" (override). The
  ## inherited flag is preserved so the per-facet column-based rendering below
  ## is reserved for the inherited case; an explicit `loq` always draws a
  ## global hline.
  loq_inherited <- missing(loq)
  if (loq_inherited) loq <- compute_out$config$loq

  ## min_bin_count predicate diverges by type. For cont, only quantifiable
  ## obs count (BLQ-encoded records excluded). For cens, BLQ-heavy bins are
  ## the most informative so the threshold is applied to total obs.
  vpcstats <- if (type == "cens") {
    dplyr::filter(compute_out$stats, .data$obs_n >= min_bin_count)
  } else {
    dplyr::filter(compute_out$stats,
                  (.data$obs_n - .data$obs_n_blq) >= min_bin_count)
  }
  obs <- compute_out$obs

  shown <- merge_element(shown, plot_vpc_shown())
  vpctheme <- merge_theme(theme, plot_vpc_theme())

  ## Column-name dispatch: std flavor uses unprefixed stats columns and
  ## OBSDV in the obs frame; pc flavor uses pc_* stats columns and PC_OBSDV
  ## in the obs frame.
  prefix <- if (isTRUE(pcvpc)) "pc_" else ""
  col <- function(x) paste0(prefix, x)
  obs_y_col <- if (isTRUE(pcvpc)) "PC_OBSDV" else "OBSDV"

  ## LOQ ref line is meaningless on the pc scale; force off.
  if (isTRUE(pcvpc)) loq <- NULL

  plot <- ggplot2::ggplot(vpcstats, ggplot2::aes(x = .data[[bin_var]]))

  if (type == "cont") {

  if (isTRUE(shown$sim_pi_area)) {
    plot <- plot +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data[[col("sim_low_med")]],
                     ymax = .data[[col("sim_hi_med")]]),
        fill = vpctheme$sim_pi_area$fill,
        alpha = vpctheme$sim_pi_area$alpha
      )
  }

  if (isTRUE(shown$sim_pi_ci)) {
    plot <- plot +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data[[col("sim_low_low")]],
                     ymax = .data[[col("sim_low_hi")]]),
        fill = vpctheme$sim_pi_ci$fill,
        alpha = vpctheme$sim_pi_ci$alpha
      ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data[[col("sim_hi_low")]],
                     ymax = .data[[col("sim_hi_hi")]]),
        fill = vpctheme$sim_pi_ci$fill,
        alpha = vpctheme$sim_pi_ci$alpha
      )
  }

  if (isTRUE(shown$sim_pi_line)) {
    plot <- plot +
      ggplot2::geom_line(
        ggplot2::aes(y = .data[[col("sim_low_med")]]),
        color = vpctheme$sim_pi_line$color,
        linetype = vpctheme$sim_pi_line$linetype,
        linewidth = vpctheme$sim_pi_line$linewidth
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = .data[[col("sim_hi_med")]]),
        color = vpctheme$sim_pi_line$color,
        linetype = vpctheme$sim_pi_line$linetype,
        linewidth = vpctheme$sim_pi_line$linewidth
      )
  }

  if (isTRUE(shown$sim_median_ci)) {
    plot <- plot +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data[[col("sim_med_low")]],
                     ymax = .data[[col("sim_med_hi")]]),
        fill = vpctheme$sim_median_ci$fill,
        alpha = vpctheme$sim_median_ci$alpha
      )
  }

  if (isTRUE(shown$sim_median_line)) {
    plot <- plot +
      ggplot2::geom_line(
        ggplot2::aes(y = .data[[col("sim_med_med")]]),
        color = vpctheme$sim_median_line$color,
        linetype = vpctheme$sim_median_line$linetype,
        linewidth = vpctheme$sim_median_line$linewidth
      )
  }

  if (isTRUE(shown$obs_median_line)) {
    plot <- plot +
      ggplot2::geom_line(
        ggplot2::aes(x = .data[[bin_var]], y = .data[[col("obs_med")]]),
        inherit.aes = FALSE,
        color = vpctheme$obs_median_line$color,
        linetype = vpctheme$obs_median_line$linetype,
        linewidth = vpctheme$obs_median_line$linewidth
      )
  }

  if (isTRUE(shown$obs_pi_line)) {
    plot <- plot +
      ggplot2::geom_line(
        ggplot2::aes(x = .data[[bin_var]], y = .data[[col("obs_low")]]),
        inherit.aes = FALSE,
        color = vpctheme$obs_pi_line$color,
        linetype = vpctheme$obs_pi_line$linetype,
        linewidth = vpctheme$obs_pi_line$linewidth
      ) +
      ggplot2::geom_line(
        ggplot2::aes(x = .data[[bin_var]], y = .data[[col("obs_hi")]]),
        inherit.aes = FALSE,
        color = vpctheme$obs_pi_line$color,
        linetype = vpctheme$obs_pi_line$linetype,
        linewidth = vpctheme$obs_pi_line$linewidth
      )
  }

  } else if (type == "cens") {

    ## Cens layers map to a subset of the shown/theme keys used by cont:
    ##   sim_median_ci    -> ribbon of (sim_prop_blq_low, sim_prop_blq_hi)
    ##   sim_median_line  -> line at sim_prop_blq_med
    ##   obs_median_line  -> line at obs_prop_blq
    ##   obs_point        -> per-bin points at obs_prop_blq (from stats, not obs)
    if (isTRUE(shown$sim_median_ci)) {
      plot <- plot +
        ggplot2::geom_ribbon(
          ggplot2::aes(ymin = .data[["sim_prop_blq_low"]],
                       ymax = .data[["sim_prop_blq_hi"]]),
          fill = vpctheme$sim_median_ci$fill,
          alpha = vpctheme$sim_median_ci$alpha
        )
    }

    if (isTRUE(shown$sim_median_line)) {
      plot <- plot +
        ggplot2::geom_line(
          ggplot2::aes(y = .data[["sim_prop_blq_med"]]),
          color = vpctheme$sim_median_line$color,
          linetype = vpctheme$sim_median_line$linetype,
          linewidth = vpctheme$sim_median_line$linewidth
        )
    }

    if (isTRUE(shown$obs_median_line)) {
      plot <- plot +
        ggplot2::geom_line(
          ggplot2::aes(y = .data[["obs_prop_blq"]]),
          color = vpctheme$obs_median_line$color,
          linetype = vpctheme$obs_median_line$linetype,
          linewidth = vpctheme$obs_median_line$linewidth
        )
    }

    if (isTRUE(shown$obs_point)) {
      ## Cens obs points represent the same per-bin statistic as the obs
      ## line, so inherit the line color (obs_median_line) for color
      ## coherence. The obs_point theme key still controls shape/alpha/size.
      plot <- plot +
        ggplot2::geom_point(
          ggplot2::aes(y = .data[["obs_prop_blq"]]),
          shape = vpctheme$obs_point$shape,
          alpha = vpctheme$obs_point$alpha,
          size  = vpctheme$obs_point$size,
          color = vpctheme$obs_median_line$color
        )
    }

    ## Cens proportions are bounded [0, 1]. Use coord_cartesian (not
    ## scale_y_continuous limits) so boundary values aren't dropped; users
    ## can still override by adding their own coord_*() / scale_y_*() layer.
    plot <- plot + ggplot2::coord_cartesian(ylim = c(0, 1))
  }

  ## LOQ ref line: cont only (LOQ is meaningless on the pc scale and not
  ## drawn on cens plots, which already represent the BLQ proportion).
  if (type == "cont" && !is.null(loq)) {
    if (loq_inherited && !is.null(strat_var_str) && "LOQ" %in% colnames(compute_out$obs)) {
      loq_facet_df <- unique(
        compute_out$obs[!is.na(compute_out$obs$LOQ),
                        c(strat_var_str, "LOQ"),
                        drop = FALSE]
      )
      plot <- plot +
        ggplot2::geom_hline(
          data = loq_facet_df,
          mapping = ggplot2::aes(yintercept = .data[["LOQ"]]),
          color = vpctheme$loq_line$color,
          linetype = vpctheme$loq_line$linetype,
          linewidth = vpctheme$loq_line$linewidth,
          inherit.aes = FALSE
        )
    } else {
      plot <- plot +
        ggplot2::geom_hline(
          yintercept = loq,
          color = vpctheme$loq_line$color,
          linetype = vpctheme$loq_line$linetype,
          linewidth = vpctheme$loq_line$linewidth
        )
    }
  }

  if (!is.null(strat_var_str)) {
    plot <- plot +
      ggplot2::facet_wrap(ggplot2::vars(.data[[strat_var_str]]))
  }

  ## Obs scatter: cont only. Cens obs points are drawn from the per-bin
  ## stats frame inside the cens layer block above.
  if (type == "cont" && isTRUE(shown$obs_point) && nrow(obs) > 0) {
    plot <- plot +
      ggplot2::geom_point(ggplot2::aes(y = .data[[obs_y_col]], x = TIME),
                          data = obs, inherit.aes = FALSE,
                          shape = vpctheme$obs_point$shape,
                          alpha = vpctheme$obs_point$alpha,
                          size = vpctheme$obs_point$size,
                          color = vpctheme$obs_point$color)
  }

  if (isTRUE(show_rep)) {
    plot <- plot +
      ggplot2::labs(caption = paste0(
        "Replicates = ", compute_out$config$n_replicates))
  }

  p <- apply_panel_theme(plot, white_panel = TRUE)
  class(p) <- c("pmx_vpc_plot", class(p))
  p
}
