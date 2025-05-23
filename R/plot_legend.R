
#' Plot a legend for a visual predictive check (VPC)
#'
#' @param ci simulated confidence interval plotted. Should match argument passed to [`vpc::vpc()`]. Default is c(0.05, 0.95).
#' @param pi prediction intervals plotted. Should match argument passed to [`vpc::vpc()`]. Default is c(0.05, 0.95).
#' @param lloq label for lower limit of quantification in the plot legend.
#' @param update list containing the plot elements to be updated. Default is set by [pmxhelpr_vpc_theme()].
#' @param ... Other arguments passed to [ggplot2::theme()].
#'
#' @inheritParams plot_vpc_exactbins
#' @return a ggplot2 object
#' @export plot_legend
#'
#' @examples
#'plot_legend()
#'plot_legend(
#'pi = c(0.025, 0.975),
#'ci = c(0.025, 0.925),
#'  shown = list(obs_dv = FALSE, obs_ci = TRUE,
#'  pi = FALSE, pi_as_area = FALSE, pi_ci = TRUE,
#'  obs_median = TRUE,
#'  sim_median =FALSE, sim_median_ci = TRUE))

plot_legend <- function(ci = c(0.05, 0.95),
                        pi = c(0.05, 0.95),
                        shown = NULL,
                        lloq = NULL,
                        update = NULL,
                        ...){

  #aesthetics for legend based on settings in vpc::new_vpc_theme
  new_vpc_theme_list <- pmxhelpr_vpc_theme()
  attr(new_vpc_theme_list, "class") <- NULL
  plist <- list_update(update,new_vpc_theme_list)

  #shown elements for legend based on settings in vpc::vpc
  nlist <- list_update(shown,
                       list(obs_dv = TRUE, obs_ci = TRUE,
                            pi = FALSE, pi_as_area = FALSE, pi_ci = TRUE,
                            obs_median = TRUE, sim_median =FALSE, sim_median_ci = TRUE))
  lloq_lab <- as.character(lloq)
  obs <- "Obs"
  obs_cent <- "Obs Med"
  sim_cent <- "Sim Med"
  sim_cilab <- paste0("Sim ", (max(ci)-min(ci))*100, "% CI")
  obs_pilab <- paste0("Obs ", pi[1]*100,"th", " and ", pi[2]*100, "th")
  sim_pilab <- paste0("Sim ", pi[1]*100,"th - ", pi[2]*100, "th")
  sim_cilab_cent <- paste0(sim_cilab, " Med")
  sim_cilab_pi <- paste0(sim_cilab, " ", pi[1]*100,"th", " and ", pi[2]*100, "th")

  df <- data.frame(x=NA_real_, y=NA_real_)
  plot_blank <- ggplot2::ggplot(data = df, ggplot2::aes(x,y), na.rm= TRUE)

  plot <- plot_blank +
    {if(nlist$obs_dv == TRUE) ggplot2::geom_point(ggplot2::aes(shape = obs),
                                                  color = plist$obs_color,
                                                  size = plist$obs_size, na.rm= TRUE)} +
    {if(!is.null(lloq)) ggplot2::geom_line(ggplot2::aes(linetype = lloq_lab),
                                           color = plist$loq_color,
                                           linewidth = 1, na.rm= TRUE)} +
    {if(nlist$obs_median == TRUE) ggplot2::geom_line(ggplot2::aes(linetype = obs_cent),
                                                     color = plist$obs_median_color,
                                                     linewidth = plist$obs_median_size, na.rm= TRUE)} +
    {if(nlist$obs_ci == TRUE) ggplot2::geom_line(ggplot2::aes(linetype = obs_pilab),
                                                 color = plist$obs_ci_color,
                                                 linewidth = plist$obs_median_size, na.rm= TRUE)} +
    {if(nlist$sim_median == TRUE) ggplot2::geom_line(ggplot2::aes(linetype = sim_cent),
                                                     color = plist$sim_median_color,
                                                     linewdith = plist$sim_median_size, na.rm= TRUE)} +
    {if(nlist$pi == TRUE) ggplot2::geom_line(ggplot2::aes(linetype = sim_pilab),
                                             color = plist$sim_pi_color,
                                             linewidth = plist$sim_pi_size, na.rm= TRUE)} +
    {if(nlist$sim_median_ci == TRUE) ggplot2::geom_rect(ggplot2::aes(xmin = x, ymin = y, xmax = x, ymax = y,
                                                                     fill = sim_cilab_cent),
                                                        alpha = plist$sim_median_alpha, na.rm= TRUE)}+
    {if(nlist$pi_ci == TRUE) ggplot2::geom_rect(ggplot2::aes(xmin = x, ymin = y, xmax = x, ymax = y,
                                                             fill = sim_cilab_pi),
                                                alpha = plist$sim_pi_alpha, na.rm= TRUE)}+
    {if(nlist$pi_as_area == TRUE) ggplot2::geom_rect(ggplot2::aes(xmin = x, ymin = y, xmax = x, ymax = y,
                                                                  fill = sim_pilab),
                                                     sim_pi_alpha = plist$sim_pi_alpha, na.rm= TRUE)}+
    {if(nlist$obs_dv == TRUE) ggplot2::scale_shape_manual(name = "Points",
                       breaks = obs,
                       values = assign(obs,plist$obs_shape))}+
    ggplot2::scale_linetype_manual(name = "Lines",
                          breaks = c(
                            {if(nlist$obs_median == TRUE) obs_cent},
                            {if(nlist$obs_ci == TRUE) obs_pilab},
                            {if(nlist$sim_median == TRUE) sim_cent},
                            {if(nlist$pi == TRUE) sim_pilab},
                            {if(!is.null(lloq)) lloq_lab}
                            ),
                          values = c(
                            {if(nlist$obs_median == TRUE) assign(obs_cent, plist$obs_median_linetype)},
                            {if(nlist$obs_ci == TRUE) assign(obs_pilab, plist$obs_ci_linetype)},
                            {if(nlist$sim_median == TRUE) assign(sim_cent, plist$sim_median_linetype)},
                            {if(nlist$pi == TRUE) assign(sim_pilab, plist$sim_pi_linetype)},
                            {if(!is.null(lloq)) assign(lloq_lab, "solid")}
                            )) +
    ggplot2::scale_fill_manual(name = "Intervals",
                      breaks = c(
                        {if(nlist$sim_median_ci == TRUE) sim_cilab_cent},
                        {if(nlist$pi_ci == TRUE) sim_cilab_pi},
                        {if(nlist$pi_as_area == TRUE) sim_pilab}
                        ),
                      values = c(
                        {if(nlist$sim_median_ci == TRUE) assign(sim_cilab_cent, plist$sim_median_fill)},
                        {if(nlist$pi_ci == TRUE)assign(sim_cilab_pi, plist$sim_pi_fill)},
                        {if(nlist$pi_as_area == TRUE)assign(sim_pilab,plist$sim_pi_fill)}
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


