#' Build a VPC ggplot from a `vpc_stats` object
#'
#' @description
#' Constructs a ggplot2 VPC plot from a [df_vpcstats()] result (or any object
#' satisfying the `vpc_stats` contract). Applies the `min_bin_count` filter,
#' draws the simulated and observed quantile layers, overlays the observation
#' scatter, draws the LOQ reference line, applies stratification facets, and
#' adds the replicates caption and panel theme. The `pcvpc` argument selects
#' the standard or prediction-corrected column set; both are present in the
#' compute output.
#'
#' Most users will reach this function indirectly via [plot_vpc_cont()].
#' Call `plot_build_vpc()` directly when working from a manually-constructed
#' or cached `vpc_stats` object — for example, plotting a precomputed result
#' from disk or a custom pipeline that produces compatible columns.
#'
#' @param compute_out A `vpc_stats` object (typically the output of
#'    [df_vpcstats()]). Must contain `stats` and `obs` data.frames with the
#'    columns documented in [df_vpcstats()]. Validated by
#'    `validate_vpc_stats()` at entry.
#' @param min_bin_count Minimum number of quantifiable observations
#'    (`obs_n - obs_n_blq`) required for inclusion in binned plot layers.
#'    BLQ-encoded records do not count toward this threshold.
#' @param show_rep Logical. Display replicate count as a plot caption.
#' @param shown Layer visibility settings created by [plot_vpc_shown()].
#' @param theme Aesthetic parameters created by [plot_vpc_theme()].
#' @param pcvpc Logical. When `TRUE`, plot the prediction-corrected columns
#'    (`pc_*` for stats, `PC_OBSDV` for the obs scatter) and suppress the
#'    LOQ reference line. Default is `FALSE` (standard VPC).
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
#'                               dv_var = ODV)
#' out <- df_vpcstats(simout, strat_var = FOOD)
#' p <- plot_build_vpc(out, pcvpc = FALSE)

plot_build_vpc <- function(compute_out,
                           min_bin_count = 1,
                           show_rep = TRUE,
                           shown = NULL,
                           theme = NULL,
                           pcvpc = FALSE,
                           loq = NULL,
                           strat_var = NULL,
                           bin_var = BIN_MID_VAR) {

  validate_vpc_stats(compute_out)

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
        "Replicates = ", compute_out$config$n_replicates))
  }

  p <- apply_panel_theme(plot, white_panel = TRUE)
  class(p) <- c("pmx_vpc_plot", class(p))
  p
}
