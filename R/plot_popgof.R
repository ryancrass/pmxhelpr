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
#' @param dv_var Column containing the dependent variable (DV).
#'    Accepts bare names or strings. Default is `DV`.
#' @param pred_var Column containing population predictions (PRED).
#'    Accepts bare names or strings. Default is `PRED`.
#' @param ipred_var Column containing individual predictions (IPRED).
#'    Accepts bare names or strings. Default is `IPRED`.
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
#'plot_popgof(data_sad_pkfit, dv_var = ODV, dosenorm = TRUE, ylab = "Dose-norm Conc.")
#'

plot_popgof <- function(data,
                        dv_var = DV,
                        pred_var = PRED,
                        ipred_var = IPRED,
                        time_var = TIME,
                        ntime_var = NTIME,
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

  dv_var_str    <- resolve_var(rlang::enquo(dv_var))
  pred_var_str  <- resolve_var(rlang::enquo(pred_var))
  ipred_var_str <- resolve_var(rlang::enquo(ipred_var))
  time_var_str  <- resolve_var(rlang::enquo(time_var))
  ntime_var_str <- resolve_var(rlang::enquo(ntime_var))
  grp_var_str   <- resolve_var(rlang::enquo(grp_var))
  dose_var_str  <- resolve_var(rlang::enquo(dose_var))

  output_colors <- merge_element(output_colors, c(PRED = "red",
                                                  IPRED = "green",
                                                  DV = "blue",
                                                  OBS = "darkgrey"))

  prep <- df_prep_dvtime(
    data, time_var_str, ntime_var_str,
    dv_var_str = dv_var_str,
    pred_var_str = pred_var_str,
    ipred_var_str = ipred_var_str,
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

  #Reference Lines: Y=cfb_base (cfb = TRUE) or Y=LLOQ (loq_method = 1,2)
  plot <- add_cfb_layers(plot, cfb, cfb_base, plottheme)

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
  plot <- plot +
    ggplot2::scale_color_manual(values = output_colors, limits = c("OBS", "DV", "IPRED", "PRED"))

  #Caption
  if(show_caption == TRUE) plot <- plot + ggplot2::labs(caption = caption)

  return(plot)
}


