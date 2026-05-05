#' Build a VPC ggplot from compute output
#'
#' @description
#' `plot_build_vpc()` constructs a ggplot2 VPC plot from the output of
#' [df_vpccompute()] / [df_vpcstats()]. It applies the `min_bin_count` filter,
#' draws the simulated and observed quantile layers, overlays the observation
#' scatter, draws the LOQ reference line, applies stratification facets, and
#' adds the replicates caption and panel theme. The `pcvpc` argument selects
#' the standard or prediction-corrected column set; both are present in the
#' compute output.
#'
#' @param compute_out List with two data.frames as returned by
#'    [df_vpccompute()]: `stats` (summary statistics with both std and pc
#'    columns; carries `n_replicates`, `loq`, `strat_var` attributes) and
#'    `obs` (first-replicate observations carrying both `OBSDV` and
#'    `PC_OBSDV`).
#' @param min_bin_count Minimum number of quantifiable observations
#'    (`obs_n - obs_n_blq`) required for inclusion in binned plot layers.
#'    BLQ-encoded records do not count toward this threshold.
#' @param show_rep Logical. Display replicate count as a plot caption.
#' @param shown Named list of logicals specifying which layers to include.
#' @param theme Named list of aesthetic parameters (colors, sizes, etc.).
#' @param pcvpc Logical. When `TRUE`, plot the prediction-corrected columns
#'    (`pc_*` for stats, `PC_OBSDV` for the obs scatter) and suppress the
#'    LOQ reference line. Default is `FALSE` (standard VPC).
#' @param loq Numeric value for LOQ reference line, or `NULL` to suppress.
#'    Forced to `NULL` when `pcvpc = TRUE` (LOQ has no meaning on the
#'    prediction-corrected scale).
#' @param strat_var_str String or `NULL`. Stratification variable name.
#' @param bin_var String. Binning variable name. Default is `"BIN_MID"`.
#'
#' @return A `ggplot2` object.
#' @keywords internal

plot_build_vpc <- function(compute_out,
                           min_bin_count = 1,
                           show_rep = TRUE,
                           shown = NULL,
                           theme = NULL,
                           pcvpc = FALSE,
                           loq = NULL,
                           strat_var_str = NULL,
                           bin_var = "BIN_MID") {

  vpcstats <- dplyr::filter(compute_out$stats,
                            (.data$obs_n - .data$obs_n_blq) >= min_bin_count)
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

  if (!is.null(loq)) {
    plot <- plot +
      ggplot2::geom_hline(
        yintercept = loq,
        color = vpctheme$loq_line$color,
        linetype = vpctheme$loq_line$linetype,
        linewidth = vpctheme$loq_line$linewidth
      )
  }

  if (!is.null(strat_var_str)) {
    plot <- plot +
      ggplot2::facet_wrap(ggplot2::vars(.data[[strat_var_str]]))
  }

  if (isTRUE(shown$obs_point) && nrow(obs) > 0) {
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
        "Replicates = ", attr(compute_out$stats, "n_replicates")))
  }

  plot +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                            linewidth = 0.5,
                                                            color = "black"),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank())
}
