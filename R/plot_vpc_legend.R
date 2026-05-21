
#' Plot a legend for a visual predictive check (VPC)
#'
#' @param ci Numeric confidence level for simulation intervals (e.g., `0.90` for 90% CI).
#'    Should match argument passed to [plot_vpc_cont()]. Default is `0.90`.
#' @param pi prediction intervals plotted. Should match argument passed to [plot_vpc_cont()]. Default is c(0.05, 0.95).
#'    Ignored when `type = "cens"` (cens VPCs do not have prediction intervals).
#' @param lloq Numeric scalar or vector of LLOQ values to label in the legend,
#'    or `NULL` to omit. Each unique value becomes one legend entry rendered
#'    with the theme's `loq_line` linetype. Pass `compute_out$config$loq` from
#'    a [df_vpcstats()] result to mirror the reference lines drawn by
#'    [plot_build_vpc()].
#' @param theme Named list of aesthetic parameters for the plot created by [plot_vpc_theme()].
#'    Defaults can be viewed by running `plot_vpc_theme()` with no arguments.
#' @param type One of `"cont"` (default) or `"cens"`. Selects the labels and
#'    layer set the legend describes. Under `"cont"`, the central-tendency
#'    entries are labeled `"Obs Med"`, `"Sim Med"`, and `"Sim <ci>% CI Med"`
#'    and the pi entries (`"Obs <p1>th and <p2>th"`, `"Sim <p1>th - <p2>th"`,
#'    `"Sim <ci>% CI <p1>th and <p2>th"`) are included when their `shown`
#'    keys are on. Under `"cens"`, the three central-tendency labels are
#'    relabeled to `"Obs Prop BLQ"`, `"Sim Prop BLQ"`, `"Sim <ci>% CI Prop BLQ"`
#'    and all pi-related entries are suppressed regardless of `shown` (cens
#'    VPCs have no prediction interval).
#' @param ... Other arguments passed to [ggplot2::theme()].
#'
#' @inheritParams plot_vpc_cont
#' @family vpc
#' @return a ggplot2 object
#' @export plot_vpc_legend
#'
#' @examples
#'plot_vpc_legend()
#'plot_vpc_legend(
#'pi = c(0.025, 0.975),
#'ci = 0.95,
#'  shown = plot_vpc_shown(obs_point = FALSE, obs_pi_line = TRUE,
#'  sim_pi_line = FALSE, sim_pi_area = FALSE, sim_pi_ci = TRUE,
#'  obs_median_line = TRUE,
#'  sim_median_line = FALSE, sim_median_ci = TRUE))
#'## Cens VPC legend
#'plot_vpc_legend(type = "cens", lloq = 1)

plot_vpc_legend <- function(ci = 0.90,
                        pi = c(0.05, 0.95),
                        shown = NULL,
                        lloq = NULL,
                        theme = NULL,
                        type = c("cont", "cens"),
                        ...){

  type <- match.arg(type)

  #aesthetics for legend based on settings in plot_vpc_theme
  plist <- merge_theme(theme, plot_vpc_theme())

  #shown elements for legend based on settings in plot_vpc_cont
  nlist <- merge_element(shown, plot_vpc_shown())

  ## Cens invariant: no prediction intervals on a cens VPC, so the four
  ## *_pi_* keys are forced off regardless of the user's `shown`. This
  ## lets the 8 downstream pi-key sites read `nlist$xxx` directly without
  ## also re-checking `type`.
  if (type == "cens") {
    nlist$obs_pi_line <- FALSE
    nlist$sim_pi_line <- FALSE
    nlist$sim_pi_ci   <- FALSE
    nlist$sim_pi_area <- FALSE
  }

  if (!is.null(lloq)) check_numeric_strict(lloq, "lloq")
  lloq_lab <- paste0("LLOQ = ", lloq)
  df_lloq <- if (!is.null(lloq)) {
    data.frame(x = rep(NA_real_, length(lloq)),
               y = rep(NA_real_, length(lloq)),
               lloq_lab = factor(lloq_lab, levels = lloq_lab))
  } else NULL
  obs <- "Obs"
  sim_cilab <- paste0("Sim ", ci * 100, "% CI")
  if (type == "cens") {
    obs_cent       <- "Obs Prop BLQ"
    sim_cent       <- "Sim Prop BLQ"
    sim_cilab_cent <- paste0(sim_cilab, " Prop BLQ")
    obs_pilab      <- NULL
    sim_pilab      <- NULL
    sim_cilab_pi   <- NULL
  } else {
    obs_cent       <- "Obs Med"
    sim_cent       <- "Sim Med"
    sim_cilab_cent <- paste0(sim_cilab, " Med")
    obs_pilab      <- paste0("Obs ", pi[1]*100,"th", " and ", pi[2]*100, "th")
    sim_pilab      <- paste0("Sim ", pi[1]*100,"th - ", pi[2]*100, "th")
    sim_cilab_pi   <- paste0(sim_cilab, " ", pi[1]*100,"th", " and ", pi[2]*100, "th")
  }

  df <- data.frame(x=NA_real_, y=NA_real_)
  plot_blank <- ggplot2::ggplot(data = df, ggplot2::aes(.data$x, .data$y))

  ## Named-vector construction for the manual scales: pairs each legend label
  ## (the data value mapped in the geom aes()) with its aesthetic value. Names
  ## must be set with stats::setNames() because most labels are held in
  ## variables (e.g. `obs_pilab`), not literal tokens. Passing named values to
  ## scale_*_manual() avoids the positional coupling between `breaks` and
  ## `values` that the previous assign()-based construction relied on.
  shape_vals <- if (isTRUE(nlist$obs_point)) {
    c("Obs" = plist$obs_point$shape)
  } else NULL

  linetype_vals <- c(
    if (isTRUE(nlist$obs_median_line))  stats::setNames(plist$obs_median_line$linetype, obs_cent),
    if (isTRUE(nlist$obs_pi_line))      stats::setNames(plist$obs_pi_line$linetype,     obs_pilab),
    if (isTRUE(nlist$sim_median_line))  stats::setNames(plist$sim_median_line$linetype, sim_cent),
    if (isTRUE(nlist$sim_pi_line))      stats::setNames(plist$sim_pi_line$linetype,     sim_pilab),
    if (!is.null(lloq))                 stats::setNames(rep(plist$loq_line$linetype, length(lloq_lab)), lloq_lab)
  )

  fill_vals <- c(
    if (isTRUE(nlist$sim_median_ci))    stats::setNames(plist$sim_median_ci$fill, sim_cilab_cent),
    if (isTRUE(nlist$sim_pi_ci))        stats::setNames(plist$sim_pi_ci$fill,     sim_cilab_pi),
    if (isTRUE(nlist$sim_pi_area))      stats::setNames(plist$sim_pi_area$fill,   sim_pilab)
  )

  plot <- plot_blank +
    {if(nlist$obs_point == TRUE) ggplot2::geom_point(ggplot2::aes(shape = obs),
                                                  color = plist$obs_point$color,
                                                  size = plist$obs_point$size, na.rm= TRUE)} +
    {if(!is.null(lloq)) ggplot2::geom_line(data = df_lloq,
                                           ggplot2::aes(linetype = lloq_lab),
                                           color = plist$loq_line$color,
                                           linewidth = 1, na.rm= TRUE)} +
    {if(nlist$obs_median_line == TRUE) ggplot2::geom_line(ggplot2::aes(linetype = obs_cent),
                                                     color = plist$obs_median_line$color,
                                                     linewidth = plist$obs_median_line$linewidth, na.rm= TRUE)} +
    {if(nlist$obs_pi_line == TRUE) ggplot2::geom_line(ggplot2::aes(linetype = obs_pilab),
                                                 color = plist$obs_pi_line$color,
                                                 linewidth = plist$obs_pi_line$linewidth, na.rm= TRUE)} +
    {if(nlist$sim_median_line == TRUE) ggplot2::geom_line(ggplot2::aes(linetype = sim_cent),
                                                     color = plist$sim_median_line$color,
                                                     linewidth = plist$sim_median_line$linewidth, na.rm= TRUE)} +
    {if(nlist$sim_pi_line == TRUE) ggplot2::geom_line(ggplot2::aes(linetype = sim_pilab),
                                             color = plist$sim_pi_line$color,
                                             linewidth = plist$sim_pi_line$linewidth, na.rm= TRUE)} +
    {if(nlist$sim_median_ci == TRUE) ggplot2::geom_rect(ggplot2::aes(xmin = .data$x, ymin = .data$y, xmax = .data$x, ymax = .data$y,
                                                                     fill = sim_cilab_cent),
                                                        alpha = plist$sim_median_ci$alpha, na.rm= TRUE)}+
    {if(nlist$sim_pi_ci == TRUE) ggplot2::geom_rect(ggplot2::aes(xmin = .data$x, ymin = .data$y, xmax = .data$x, ymax = .data$y,
                                                             fill = sim_cilab_pi),
                                                alpha = plist$sim_pi_ci$alpha, na.rm= TRUE)}+
    {if(nlist$sim_pi_area == TRUE) ggplot2::geom_rect(ggplot2::aes(xmin = .data$x, ymin = .data$y, xmax = .data$x, ymax = .data$y,
                                                                  fill = sim_pilab),
                                                     alpha = plist$sim_pi_area$alpha, na.rm= TRUE)}+
    {if (isTRUE(nlist$obs_point)) ggplot2::scale_shape_manual(name = "Points",
                                                              breaks = names(shape_vals),
                                                              values = shape_vals)} +
    ggplot2::scale_linetype_manual(name = "Lines",
                                   breaks = names(linetype_vals),
                                   values = linetype_vals) +
    ggplot2::scale_fill_manual(name = "Intervals",
                               breaks = names(fill_vals),
                               values = fill_vals) +
    ggplot2::theme_void()+
    ggplot2::theme(legend.position = "inside",
                   legend.box = "horizontal",
                   legend.title = ggplot2::element_text(size = 10),
                   legend.text = ggplot2::element_text(size = 8),
                   ...)+
    ggplot2::guides(shape = ggplot2::guide_legend(order=1),
                    linetype = ggplot2::guide_legend(order=2),
                    fill = ggplot2::guide_legend(order=3))

  return(plot)
}


