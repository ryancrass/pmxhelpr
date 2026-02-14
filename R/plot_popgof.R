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
                        grp_var = "ID",
                        dose_var = "DOSE",
                        loq = NULL,
                        loq_method = 0,
                        cent = "mean",
                        obs_dv = TRUE,
                        grp_dv = FALSE,
                        dosenorm = FALSE,
                        cfb = FALSE,
                        ylab = "Concentration",
                        log_y = FALSE,
                        show_caption = TRUE,
                        n_breaks = 8,
                        theme = NULL){


  ##Update Defaults to time_vars and output_vars
  time_vars <- list_update(time_vars, c(TIME = "TIME",
                                        NTIME = "NTIME"))
  output_vars <- list_update(output_vars, c(PRED = "PRED",
                                            IPRED = "IPRED",
                                            DV = "DV"))
  output_colors <- list_update(output_colors, c(PRED = "red",
                                                IPRED = "green",
                                                DV = "blue",
                                                OBS = "darkgrey"))

  #Checks
  check_df(data)
  check_varsindf(data, time_vars[["TIME"]])
  check_varsindf(data, time_vars[["NTIME"]])
  check_varsindf(data, output_vars[["DV"]])
  check_varsindf(data, output_vars[["IPRED"]])
  check_varsindf(data, output_vars[["PRED"]])
  check_varsindf(data, "MDV")
  check_timeu(timeu)
  if(grp_dv == TRUE) {check_varsindf(data, grp_var)}
  if(dosenorm == TRUE){check_varsindf(data, dose_var)}
  check_loq_method(loq, loq_method, data)

  #Handle Output and Time Variables
  if(length(unique(c(time_vars[[1]], time_vars[[2]]))) == 2) {
    data <- dplyr::rename(data, dplyr::any_of(c(time_vars, output_vars)))
  } else {
    data <- data |>
      dplyr::rename(dplyr::any_of(c(c(NTIME = time_vars[["NTIME"]]),
                                  output_vars))) |>
      dplyr::mutate(TIME = NTIME)
  }

  if(dosenorm==TRUE) {data <- dplyr::rename(data, dplyr::any_of(c(DOSE = dose_var)))}

  ##BLQ Handling
  if(loq_method==1) {
    data <- data |>
      dplyr::mutate(LOQ = ifelse(is.null(loq), LLOQ, loq)) |>
      dplyr::mutate(DV = dplyr::case_when(EVID != 0 ~ NA_real_,
                                          MDV == 0 ~ DV,
                                          TIME <= 0 ~ 0,
                                          TIME > 0 ~ 0.5*LOQ),
                    IPRED = dplyr::case_when(EVID != 0 ~ NA_real_,
                                             TIME <= 0 ~ 0,
                                             IPRED >= LOQ ~ IPRED,
                                             IPRED < LOQ ~ 0.5*LOQ),
                    PRED = dplyr::case_when(EVID != 0 ~ NA_real_,
                                            TIME <= 0 ~ 0,
                                            PRED >= LOQ ~ PRED,
                                            PRED < LOQ ~ 0.5*LOQ))
  }

  if(loq_method==2) {
    data <- data |>
      dplyr::mutate(LOQ = ifelse(is.null(loq), LLOQ, loq)) |>
      dplyr::mutate(DV = dplyr::case_when(EVID != 0 ~ NA_real_,
                                          MDV == 0 ~ DV,
                                          MDV == 1 ~ 0.5*LOQ),
                    IPRED = dplyr::case_when(EVID != 0 ~ NA_real_,
                                             IPRED >= LOQ ~ IPRED,
                                             IPRED < LOQ ~ 0.5*LOQ),
                    PRED = dplyr::case_when(EVID != 0 ~ NA_real_,
                                           PRED >= LOQ ~ PRED,
                                           PRED < LOQ ~ 0.5*LOQ))
  }

  lloq <- ifelse("LOQ" %in% colnames(data), unique(data$LOQ), NA_real_)

  #Dose-normalize if requested
  if(dosenorm == TRUE) {
    data <- data |>
      dplyr::mutate(DV = DV/DOSE,
                    IPRED = IPRED/DOSE,
                    PRED = PRED/DOSE)
  }

  #Determine Caption
  caption <- dvtime_caption(cent, log_y, obs_dv, grp_dv)

  #Determine Breaks
  xbreaks <- breaks_time(x = sort(unique(data$NTIME)), unit = timeu, n = n_breaks)

  #Determine aesthetics
  plottheme <- list_update(theme, plot_popgof_theme())

  #Determine Error Bar Cap Width
  if(is.numeric(plottheme$width_errorbar)) {
    width <- plottheme$width_errorbar
  } else {
    width <- max(data$NTIME, na.rm = TRUE)*0.025
  }


###Plot

  #Initialize Plot Aesthetics
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = TIME, y=DV)) +
    ggplot2::labs(x=paste0("Time (", timeu, ")"), y=ylab, color = "Legend") +
    ggplot2::scale_x_continuous(breaks = xbreaks) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank())

  #Reference Lines: Y=0 (cfb = TRUE) or Y=LLOQ (loq_method = 1,2)
  if(cfb == TRUE) plot <- plot + ggplot2::geom_hline(yintercept = 0,
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
                                                                    group = !!dplyr::sym(grp_var)),
                                                       linewidth = plottheme$linewidth_obs,
                                                       linetype = plottheme$linetype_obs,
                                                       alpha = plottheme$alpha_line_obs)

  #Plot Points
  if(cent %in% c("mean", "mean_sdl", "mean_sdl_upper")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=DV,color = "DV"),
                                                                           fun = "mean", geom = "point",
                                                                           size = plottheme$size_point_cent,
                                                                           shape = plottheme$shape_point_cent,
                                                                           alpha = plottheme$alpha_point_cent)
  if(cent %in% c("median", "median_iqr")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=DV, color = "DV"),
                                                                               fun = "median", geom = "point",
                                                                               size = plottheme$size_point_cent,
                                                                               shape = plottheme$shape_point_cent,
                                                                               alpha = plottheme$alpha_point_cent)
  if(cent %in% c("mean", "mean_sdl", "mean_sdl_upper")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=IPRED, color = "IPRED"),
                                                                                             fun = "mean", geom = "point",
                                                                                             size = plottheme$size_point_cent,
                                                                                             shape = plottheme$shape_point_cent,
                                                                                             alpha = plottheme$alpha_point_cent)
  if(cent %in% c("median", "median_iqr")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=IPRED,color = "IPRED"),
                                                                               fun = "median", geom = "point",
                                                                               size = plottheme$size_point_cent,
                                                                               shape = plottheme$shape_point_cent,
                                                                               alpha = plottheme$alpha_point_cent)
  if(cent %in% c("mean", "mean_sdl", "mean_sdl_upper")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=PRED,color = "PRED"),
                                                                           fun = "mean", geom = "point",
                                                                           size = plottheme$size_point_cent,
                                                                           shape = plottheme$shape_point_cent,
                                                                           alpha = plottheme$alpha_point_cent)
  if(cent %in% c("median", "median_iqr")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=PRED,color = "PRED"),
                                                                               fun = "median", geom = "point",
                                                                               size = plottheme$size_point_cent,
                                                                               shape = plottheme$shape_point_cent,
                                                                               alpha = plottheme$alpha_point_cent)

  #Plot Observed Central Tendency
  if(cent %in% c("mean", "mean_sdl", "mean_sdl_upper")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=DV,color = "DV"),
                                                                           fun = "mean", geom = "line",
                                                                           linewidth = plottheme$linewidth_obs,
                                                                           linetype = plottheme$linetype_obs,
                                                                           alpha = plottheme$alpha_line_obs)
  if(cent %in% c("median", "median_iqr")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=DV,color = "DV"),
                                                                               fun = "median", geom = "line",
                                                                               linewidth = plottheme$linewidth_obs,
                                                                               linetype = plottheme$linetype_obs,
                                                                               alpha = plottheme$alpha_line_obs)
  #Plot Observed Central Tendency and Error Bars
  if(cent == "mean_sdl") plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=DV,color = "DV"),
                                                              fun.data = "mean_sdl", fun.args = list(mult=1),
                                                              geom = "errorbar",
                                                              linewidth = plottheme$linewidth_errorbar,
                                                              linetype = plottheme$linetype_errorbar,
                                                              alpha = plottheme$alpha_errorbar,
                                                              width = width)
  if(cent == "mean_sdl_upper") plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=DV, color = "DV"),
                                                                    fun.max = function(x){mean(x)+stats::sd(x)},
                                                                    fun.min = function(x){NA_real_},
                                                                    geom = "errorbar",
                                                                    linewidth = plottheme$linewidth_errorbar,
                                                                    linetype = plottheme$linetype_errorbar,
                                                                    alpha = plottheme$alpha_errorbar,
                                                                    width = width) +
                                              ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=DV, color = "DV"),
                                                                    fun.max = function(x){mean(x)+stats::sd(x)},
                                                                    fun.min = function(x){mean(x)},
                                                                    geom = "linerange",show.legend = FALSE,
                                                                    linewidth = plottheme$linewidth_errorbar,
                                                                    linetype = plottheme$linetype_errorbar,
                                                                    alpha = plottheme$alpha_errorbar,
                                                                    width = width)
  if(cent == "median_iqr") plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=DV,color = "DV"),
                                                              fun.max = function(x){stats::quantile(x,0.75)},
                                                              fun.min = function(x){stats::quantile(x,0.25)},
                                                              geom = "errorbar",
                                                              linewidth = plottheme$linewidth_errorbar,
                                                              linetype = plottheme$linetype_errorbar,
                                                              alpha = plottheme$alpha_errorbar)

  #Plot Individual Model Predictions
  if(cent %in% c("mean", "mean_sdl", "mean_sdl_upper")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=IPRED,color = "IPRED"),
                                                                           fun = "mean", geom = "line",
                                                                           linewidth = plottheme$linewidth_cent,
                                                                           linetype = plottheme$linetype_cent,
                                                                           alpha = plottheme$alpha_line_cent) +
  if(cent %in% c("median", "median_iqr")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=IPRED,color = "IPRED"),
                                                                               fun = "median", geom = "line",
                                                                               linewidth = plottheme$linewidth_cent,
                                                                               linetype = plottheme$linetype_cent,
                                                                               alpha = plottheme$alpha_line_cent)

  #Plot Population Model Predictions
  if(cent %in% c("mean", "mean_sdl", "mean_sdl_upper")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=PRED,color = "PRED"),
                                                                           fun = "mean", geom = "line",
                                                                           linewidth = plottheme$linewidth_cent,
                                                                           linetype = plottheme$linetype_cent,
                                                                           alpha = plottheme$alpha_line_cent)
  if(cent %in% c("median", "median_iqr")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=PRED,color = "PRED"),
                                                                               fun = "median", geom = "line",
                                                                               linewidth = plottheme$linewidth_cent,
                                                                               linetype = plottheme$linetype_cent,
                                                                               alpha = plottheme$alpha_line_cent)

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


plot_popgof_theme <- function(update = NULL){
  defaults_list <- list(
    linewidth_ref = 0.5,
    linetype_ref = 2,
    alpha_line_ref = 1,

    shape_point_obs = 1.25,
    size_point_obs = 0.75,
    alpha_point_obs = 0.5,
    linewidth_obs = 1,
    linetype_obs = 1,
    alpha_line_obs = 1,

    shape_point_cent = 1,
    size_point_cent = 1.25,
    alpha_point_cent = 1,
    linewidth_cent = 0.75,
    linetype_cent = 1,
    alpha_line_cent = 1,

    linewidth_errorbar = 0.75,
    linetype_errorbar = 1,
    alpha_errorbar = 1,
    width_errorbar = NULL
  )

  default_theme <- defaults_list
  theme <- list_update(update, default_theme)
  return(theme)
}
