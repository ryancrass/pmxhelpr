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
#' @param theme Named list of aesthetic parameters to be supplied to the plot.
#'    Defaults can be viewed by running `plot_popgof_theme()` with no arguments.
#'    Default `width_errorbar` is 2.5% of maximum `NTIME`.
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

  ##Update Defaults to time_vars and output_vars
  time_vars <- init_time_vars(time_vars)
  output_vars <- list_update(output_vars, c(PRED = "PRED",
                                            IPRED = "IPRED",
                                            DV = "DV"))
  output_colors <- list_update(output_colors, c(PRED = "red",
                                                IPRED = "green",
                                                DV = "blue",
                                                OBS = "darkgrey"))

  #Checks
  check_df(data, "data")
  check_varsindf(data, time_vars[["TIME"]], "data", "time_vars")
  check_varsindf(data, time_vars[["NTIME"]], "data", "time_vars")
  check_varsindf(data, output_vars[["DV"]], "data", "output_vars")
  check_varsindf(data, output_vars[["IPRED"]], "data", "output_vars")
  check_varsindf(data, output_vars[["PRED"]], "data", "output_vars")
  check_varsindf(data, "MDV", "data", "MDV")
  check_timeu(timeu)
  if(grp_dv == TRUE) {check_varsindf(data, grp_var_str, "data", "grp_var")}
  if(dosenorm == TRUE){check_varsindf(data, dose_var_str, "data", "dose_var")}
  check_loq_method(loq, loq_method, data)
  if(cfb==TRUE)check_numeric(cfb_base, "cfb_base")

  #Handle Output and Time Variables
  data <- rename_time_vars(data, time_vars, output_vars)

  if(dosenorm==TRUE) {data <- dplyr::rename(data, dplyr::any_of(c(DOSE = dose_var_str)))}

  ##BLQ Handling
  data <- apply_blq(data, loq, loq_method, extra_vars = c("IPRED", "PRED"))

  lloq <- ifelse("LOQ" %in% colnames(data), unique(data$LOQ), NA_real_)

  #Dose-normalize if requested
  if(dosenorm == TRUE) {
    data <- dplyr::mutate(data,
                          DV    = var_dosenorm(DV, DOSE),
                          IPRED = var_dosenorm(IPRED, DOSE),
                          PRED  = var_dosenorm(PRED, DOSE))
  }

  #Determine Caption
  caption <- dvtime_caption(cent, log_y, obs_dv, grp_dv)

  #Determine Breaks
  xbreaks <- breaks_time(x = sort(unique(data$NTIME)), unit = timeu, n = n_breaks)

  #Determine aesthetics
  plottheme <- list_update(theme, plot_popgof_theme())

  #Determine Error Bar Cap Width
  width <- errorbar_width(plottheme, data)


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
                                                     linewidth = plottheme$linewidth_ref,
                                                     linetype = plottheme$linetype_ref,
                                                     alpha = plottheme$alpha_line_ref)

  if(loq_method %in% c(1,2) & dosenorm==FALSE) plot <- plot + ggplot2::geom_hline(yintercept = lloq,
                                                                                  linewidth = plottheme$linewidth_ref,
                                                                                  linetype = plottheme$linetype_ref,
                                                                                  alpha = plottheme$alpha_line_ref)

  #Show Observed Data Points
  if(obs_dv == TRUE) plot <- plot +  ggplot2::geom_point(ggplot2::aes(color = "OBS"),
                                                         size = plottheme$size_point_obs,
                                                         shape = plottheme$shape_point_obs,
                                                         alpha = plottheme$alpha_point_obs)
  #Connect Observed Data Points within Group
  if(grp_dv == TRUE) plot <- plot + ggplot2::geom_line(ggplot2::aes(x = TIME, y = DV, color = "OBS",
                                                                    group = .data[[grp_var_str]]),
                                                       linewidth = plottheme$linewidth_obs,
                                                       linetype = plottheme$linetype_obs,
                                                       alpha = plottheme$alpha_line_obs)

  #Plot Central Tendency (points, lines, error bars)
  plot <- add_cent_layers(plot, cent, "DV",    plottheme, width, color_aes = "DV",    line_prefix = "dv")
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


#' Customized population overlay goodness-of-fit (GOF) theme with pmxhelpr default aesthetics
#'
#' @param update list containing the plot elements to be updated.
#'    Run `plot_popgof_theme()` with no arguments to view defaults.
#' @return a named list `list`
#' @export plot_popgof_theme
#'
#' @examples
#' plot_popgof_theme()
#' new_theme <- plot_popgof_theme(update = list(linewidth_ref = 1))


plot_popgof_theme <- function(update = NULL) list_update(update, .popgof_defaults)
