
#' Plot a legend for a visual predictive check (VPC)
#'
#' @param ci simulated confidence interval plotted. Should match argument passed to [`vpc::vpc()`]. Default is c(0.05, 0.95).
#' @param pi prediction intervals for plotted. Should match argument passed to [`vpc::vpc()`]. Default is c(0.05, 0.95),
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
                            shown = list(obs_dv = TRUE, obs_ci = TRUE,
                                         pi = FALSE, pi_as_area = FALSE, pi_ci = TRUE,
                                         obs_median = TRUE,
                                         sim_median =FALSE, sim_median_ci = TRUE)){

  obs <- "Obs"
  obs_cent <- "Obs Med"
  sim_cent <- "Sim Med"
  sim_cilab <- paste0("Sim ", (max(ci)-min(ci))*100, "% CI")
  obs_pilab <- paste0("Obs ", pi[1]*100,"th", ",", pi[2]*100, "th")
  sim_pilab <- paste0("Sim ", pi[1]*100,"th-", pi[2]*100, "th")
  sim_cilab_cent <- paste0(sim_cilab, " Med")
  sim_cilab_pi <- paste0(sim_cilab, " ", pi[1]*100,"th", ",", pi[2]*100, "th")

  plot <- ggplot2::ggplot(data = data.frame(x=NA_real_, y= NA_real_), ggplot2::aes(x,y))

  plot <- plot +
    {if(shown$obs_dv == TRUE) ggplot2::geom_point(ggplot2::aes(shape = obs))} +
    {if(shown$obs_median == TRUE) ggplot2::geom_line(ggplot2::aes(linetype = obs_cent), linewidth = 1)} +
    {if(shown$obs_ci == TRUE) ggplot2::geom_line(ggplot2::aes(linetype = obs_pilab), linewidth = 0.5)} +
    {if(shown$sim_median == TRUE) ggplot2::geom_line(ggplot2::aes(linetype = sim_cent), linewidth = 1)} +
    {if(shown$pi == TRUE) ggplot2::geom_line(ggplot2::aes(linetype = sim_pilab), linewidth = 1)} +
    {if(shown$sim_median_ci == TRUE) ggplot2::geom_rect(ggplot2::aes(xmin = x, ymin = y, xmax = x, ymax = y, fill = sim_cilab_cent), alpha = 0.3)}+
    {if(shown$pi_ci == TRUE) ggplot2::geom_rect(ggplot2::aes(xmin = x, ymin = y, xmax = x, ymax = y, fill = sim_cilab_pi), alpha = 0.15)}+
    {if(shown$pi_as_area == TRUE) ggplot2::geom_rect(ggplot2::aes(xmin = x, ymin = y, xmax = x, ymax = y, fill = sim_pilab), alpha = 0.15)}+
    {if(shown$obs_dv == TRUE) ggplot2::scale_shape_manual(name = "Points",
                       breaks = obs,
                       values = assign(obs,1))}+
    ggplot2::scale_linetype_manual(name = "Lines",
                          breaks = c(
                            {if(shown$obs_median == TRUE) obs_cent},
                            {if(shown$obs_ci == TRUE) obs_pilab},
                            {if(shown$sim_median == TRUE) sim_cent},
                            {if(shown$pi == TRUE) sim_pilab}
                            ),
                          values = c(
                            {if(shown$obs_median == TRUE) assign(obs_cent, "solid")},
                            {if(shown$obs_ci == TRUE) assign(obs_pilab, "dashed")},
                            {if(shown$sim_median == TRUE) assign(sim_cent, "dashed")},
                            {if(shown$pi == TRUE) assign(sim_pilab, "dotted")}
                            )) +
    ggplot2::scale_fill_manual(name = "Intervals",
                      breaks = c(
                        {if(shown$sim_median_ci == TRUE) sim_cilab_cent},
                        {if(shown$pi_ci == TRUE) sim_cilab_pi},
                        {if(shown$pi_as_area == TRUE) sim_pilab}
                        ),
                      values = c(
                        {if(shown$sim_median_ci == TRUE) assign(sim_cilab_cent, "#3388cc")},
                        {if(shown$pi_ci == TRUE)assign(sim_cilab_pi, "#3388cc")},
                        {if(shown$pi_as_area == TRUE)assign(sim_pilab, "#3388cc")}
                        ))+
    ggplot2::theme_void()+
    ggplot2::theme(legend.position = "inside",
                   legend.title = ggplot2::element_text(size = 10),
                   legend.text = ggplot2::element_text(size = 8))+
    ggplot2::guides(shape = ggplot2::guide_legend(order=1),
                    linetype = ggplot2::guide_legend(order=2),
                    fill = ggplot2::guide_legend(order=3))

  return(plot)
}


