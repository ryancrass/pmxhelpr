
#' Plot a legend for a visual predictive check (VPC)
#'
#' @param ci Numeric confidence level for simulation intervals (e.g., `0.90` for 90% CI).
#'    Should match argument passed to [plot_vpc_cont()]. Default is `0.90`.
#' @param pi prediction intervals plotted. Should match argument passed to [plot_vpc_cont()]. Default is c(0.05, 0.95).
#' @param lloq label for lower limit of quantification in the plot legend.
#' @param update list containing the plot elements to be updated. Default is set by [plot_vpc_theme()].
#' @param ... Other arguments passed to [ggplot2::theme()].
#'
#' @inheritParams plot_vpc_cont
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

plot_vpc_legend <- function(ci = 0.90,
                        pi = c(0.05, 0.95),
                        shown = NULL,
                        lloq = NULL,
                        update = NULL,
                        ...){

  #aesthetics for legend based on settings in plot_vpc_theme
  plist <- merge_theme(update, plot_vpc_theme())

  #shown elements for legend based on settings in plot_vpc_cont
  nlist <- merge_element(shown, plot_vpc_shown())
  lloq_lab <- as.character(lloq)
  obs <- "Obs"
  obs_cent <- "Obs Med"
  sim_cent <- "Sim Med"
  sim_cilab <- paste0("Sim ", ci * 100, "% CI")
  obs_pilab <- paste0("Obs ", pi[1]*100,"th", " and ", pi[2]*100, "th")
  sim_pilab <- paste0("Sim ", pi[1]*100,"th - ", pi[2]*100, "th")
  sim_cilab_cent <- paste0(sim_cilab, " Med")
  sim_cilab_pi <- paste0(sim_cilab, " ", pi[1]*100,"th", " and ", pi[2]*100, "th")

  df <- data.frame(x=NA_real_, y=NA_real_)
  plot_blank <- ggplot2::ggplot(data = df, ggplot2::aes(x,y))

  plot <- plot_blank +
    {if(nlist$obs_point == TRUE) ggplot2::geom_point(ggplot2::aes(shape = obs),
                                                  color = plist$obs_point$color,
                                                  size = plist$obs_point$size, na.rm= TRUE)} +
    {if(!is.null(lloq)) ggplot2::geom_line(ggplot2::aes(linetype = lloq_lab),
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
    {if(nlist$sim_median_ci == TRUE) ggplot2::geom_rect(ggplot2::aes(xmin = x, ymin = y, xmax = x, ymax = y,
                                                                     fill = sim_cilab_cent),
                                                        alpha = plist$sim_median_ci$alpha, na.rm= TRUE)}+
    {if(nlist$sim_pi_ci == TRUE) ggplot2::geom_rect(ggplot2::aes(xmin = x, ymin = y, xmax = x, ymax = y,
                                                             fill = sim_cilab_pi),
                                                alpha = plist$sim_pi_ci$alpha, na.rm= TRUE)}+
    {if(nlist$sim_pi_area == TRUE) ggplot2::geom_rect(ggplot2::aes(xmin = x, ymin = y, xmax = x, ymax = y,
                                                                  fill = sim_pilab),
                                                     alpha = plist$sim_pi_area$alpha, na.rm= TRUE)}+
    {if(nlist$obs_point == TRUE) ggplot2::scale_shape_manual(name = "Points",
                       breaks = obs,
                       values = assign(obs, plist$obs_point$shape))}+
    ggplot2::scale_linetype_manual(name = "Lines",
                          breaks = c(
                            {if(nlist$obs_median_line == TRUE) obs_cent},
                            {if(nlist$obs_pi_line == TRUE) obs_pilab},
                            {if(nlist$sim_median_line == TRUE) sim_cent},
                            {if(nlist$sim_pi_line == TRUE) sim_pilab},
                            {if(!is.null(lloq)) lloq_lab}
                            ),
                          values = c(
                            {if(nlist$obs_median_line == TRUE) assign(obs_cent, plist$obs_median_line$linetype)},
                            {if(nlist$obs_pi_line == TRUE) assign(obs_pilab, plist$obs_pi_line$linetype)},
                            {if(nlist$sim_median_line == TRUE) assign(sim_cent, plist$sim_median_line$linetype)},
                            {if(nlist$sim_pi_line == TRUE) assign(sim_pilab, plist$sim_pi_line$linetype)},
                            {if(!is.null(lloq)) assign(lloq_lab, "solid")}
                            )) +
    ggplot2::scale_fill_manual(name = "Intervals",
                      breaks = c(
                        {if(nlist$sim_median_ci == TRUE) sim_cilab_cent},
                        {if(nlist$sim_pi_ci == TRUE) sim_cilab_pi},
                        {if(nlist$sim_pi_area == TRUE) sim_pilab}
                        ),
                      values = c(
                        {if(nlist$sim_median_ci == TRUE) assign(sim_cilab_cent, plist$sim_median_ci$fill)},
                        {if(nlist$sim_pi_ci == TRUE)assign(sim_cilab_pi, plist$sim_pi_ci$fill)},
                        {if(nlist$sim_pi_area == TRUE)assign(sim_pilab, plist$sim_pi_area$fill)}
                        ))+
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


