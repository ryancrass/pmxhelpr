#' Plot population overlay goodness-of-fit (GOF) plots
#'
#' @param output_colors Colors for model outputs. Must be named character vector.
#'
#'    Defaults are:
#'    + `PRED= "red"`
#'    + `IPRED`=`"green"`,
#'    + `DV`=`"blue"`.
#'    + `OBS`=`"darkgrey"`
#'
#' @inheritParams df_mrgsim_replicate
#' @inheritParams plot_dvtime
#' @param theme Theme object created by [plot_popgof_theme()].
#'    Defaults can be viewed by running `plot_popgof_theme()` with no arguments.
#'    Default error bar width is 2.5% of maximum `NTIME`.
#'
#' @return A `ggplot2` plot object
#'
#' @export plot_popgof
#'
#' @examples
#'plot_popgof(data_sad_pkfit, output_vars = c(DV = "ODV"), dosenorm = TRUE, ylab = "Dose-norm Conc.")
#'

plot_popgof <- function(data,
                        time_vars = c(TIME = "TIME",
                                      NTIME = "NTIME"),
                        output_vars = c(PRED = "PRED",
                                        IPRED = "IPRED",
                                        DV = "DV"),
                        output_colors = c(PRED = "red",
                                          IPRED = "green",
                                          DV = "blue",
                                          OBS = "darkgrey"),
                        timeu = "hours",
                        grp_var = ID,
                        dose_var = DOSE,
                        loq = NULL,
                        loq_method = 0,
                        cent = "mean",
                        obs_dv = TRUE,
                        grp_dv = FALSE,
                        dosenorm = FALSE,
                        cfb = FALSE,
                        cfb_base = 0,
                        ylab = "Concentration",
                        log_y = FALSE,
                        show_caption = TRUE,
                        n_breaks = 8,
                        theme = NULL){


  grp_var_str  <- resolve_var(rlang::enquo(grp_var))
  dose_var_str <- resolve_var(rlang::enquo(dose_var))

  ##Update Defaults to output_vars
  output_vars <- list_update(output_vars, c(PRED = "PRED",
                                            IPRED = "IPRED",
                                            DV = "DV"))
  output_colors <- list_update(output_colors, c(PRED = "red",
                                                IPRED = "green",
                                                DV = "blue",
                                                OBS = "darkgrey"))

  prep <- df_prep_dvtime(
    data, time_vars,
    output_vars = output_vars,
    timeu = timeu, loq = loq, loq_method = loq_method,
    dose_var_str = if (dosenorm) dose_var_str,
    grp_dv = grp_dv, grp_var_str = grp_var_str,
    dosenorm = dosenorm,
    cfb = cfb, cfb_base = cfb_base
  )
  data <- prep$data
  lloq <- prep$lloq

  env <- prep_plot_env(data, cent, log_y, obs_dv, grp_dv,
                       timeu, n_breaks, theme, plot_popgof_theme)
  caption   <- env$caption
  xbreaks   <- env$xbreaks
  plottheme <- env$plottheme
  width     <- env$width


###Plot

  #Initialize Plot Aesthetics
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = TIME, y=DV)) +
    ggplot2::labs(x=paste0("Time (", timeu, ")"), y=ylab, color = "Legend") +
    ggplot2::scale_x_continuous(breaks = xbreaks) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank())

  #Reference Lines: Y=0 (cfb = TRUE) or Y=LLOQ (loq_method = 1,2)
  if(cfb == TRUE) plot <- plot + ggplot2::geom_hline(yintercept = as.numeric(cfb_base),
                                                     linewidth = plottheme$ref$linewidth,
                                                     linetype = plottheme$ref$linetype,
                                                     alpha = plottheme$ref$alpha)

  blq <- add_blq_layers(plot, caption, loq_method, loq = lloq, dosenorm, plottheme, show_legend = FALSE)
  plot <- blq$plot
  caption <- blq$caption

  #Show Observed Data Points / Connect within Group
  plot <- add_obs_layers(plot, obs_dv, grp_dv, grp_var_str, plottheme, color_aes = "OBS")

  #Plot Central Tendency (points, lines, error bars)
  plot <- add_cent_layers(plot, cent, "DV",    plottheme, width, color_aes = "DV",    line_element = plottheme$dv_line)
  plot <- add_cent_layers(plot, cent, "IPRED", plottheme, width, color_aes = "IPRED", show_errorbars = FALSE)
  plot <- add_cent_layers(plot, cent, "PRED",  plottheme, width, color_aes = "PRED",  show_errorbars = FALSE)

  #Log Transform
  if(log_y == TRUE) plot <- plot + ggplot2::scale_y_log10(guide = "axis_logticks")

  #Define Manual Legend
  plot <- plot + ggplot2::scale_color_manual(values = output_colors, limits = c("OBS", "DV", "IPRED", "PRED"))

  #Caption
  if(show_caption == TRUE) plot <- plot + ggplot2::labs(caption = caption)

  return(plot)
}


