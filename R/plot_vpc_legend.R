
#' Plot a legend for a visual predictive check (VPC)
#'
#' @param ci simulated confidence interval plotted. Should match argument passed to [`vpc::vpc()`]. Default is c(0.05, 0.95).
#' @param pi prediction intervals for plotted. Should match argument passed to [`vpc::vpc()`]. Default is c(0.05, 0.95),
#'
#' @return a ggplot2 object
#' @export plot_vpc_legend
#'
#' @examples
#'plot_vpc_legend()
#'plot_vpc_legend(pi = c(0.05, 0.95), ci = c(0.025, 0.975))

plot_vpc_legend <- function(ci = c(0.05, 0.95),
                            pi = c(0.05, 0.95)){

  obs <- "Observations"
  cent <- "Median (50th)"
  cilab <- paste0("Simulation ", (max(ci)-min(ci))*100, "% CI")
  pilab <- paste0(pi[1]*100,"th", ", ", pi[2]*100, "th")

  plot <- ggplot2::ggplot(data = data.frame(x=NA_real_, y= NA_real_), ggplot2::aes(x,y))+
    ggplot2::geom_point(ggplot2::aes(shape = obs)) +
    ggplot2::geom_line(ggplot2::aes(linetype = cent)) +
    ggplot2::geom_line(ggplot2::aes(linetype = pilab)) +
    ggplot2::geom_rect(ggplot2::aes(xmin = x, ymin = y, xmax = x, ymax = y, fill = cent), alpha = 0.3)+
    ggplot2::geom_rect(ggplot2::aes(xmin = x, ymin = y, xmax = x, ymax = y, fill = pilab), alpha = 0.15)+
    ggplot2::scale_shape_manual(name = "Observed",
                       breaks = obs,
                       values = assign(obs,1))+
    ggplot2::scale_linetype_manual(name = "Observed Quantiles",
                          breaks = c(cent, pilab),
                          values = c(assign(cent, "solid"),
                                     assign(pilab, "dashed"))) +
    ggplot2::scale_fill_manual(name = cilab,
                      breaks = c(cent, pilab),
                      values = c(assign(cent, "#3388cc"),
                                 assign(pilab, "#3388cc")))+
    ggplot2::theme_void()+
    ggplot2::theme(legend.position = "inside")

  return(plot)
}

plot_vpc_legend()


