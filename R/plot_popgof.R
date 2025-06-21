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
                        n_breaks = 8){


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

  ##Data Rename
  data <- data |>
    dplyr::rename(dplyr::any_of(c(output_vars, time_vars, c(DOSE = dose_var))))

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


###Plot

  #Initialize Plot Aesthetics
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = TIME, y=DV)) +
    ggplot2::labs(x=paste0("Time (", timeu, ")"), y=ylab, color = "Legend") +
    ggplot2::scale_x_continuous(breaks = xbreaks) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank())

  #Reference Lines: Y=0 (cfb = TRUE) or Y=LLOQ (loq_method = 1,2)
  if(cfb == TRUE) plot <- plot + ggplot2::geom_hline(yintercept = 0, linewidth = 1, linetype = "dashed")
  if(loq_method %in% c(1,2) & dosenorm==FALSE) plot <- plot + ggplot2::geom_hline(yintercept = lloq, linewidth = 0.5, linetype = "dashed")

  #Show Observed Data Points
  if(obs_dv == TRUE) plot <- plot +  ggplot2::geom_point(ggplot2::aes(color = "OBS"), shape=1, size=1, alpha = 0.5)
  #Connect Observed Data Points within Group
  if(grp_dv == TRUE) plot <- plot + ggplot2::geom_line(ggplot2::aes(x = TIME, y = DV, color = "OBS",
                                                                    group = !!dplyr::sym(grp_var)),
                                                       linewidth = 0.25, alpha = 0.25)

  #Plot Points
  if(cent %in% c("mean", "mean_sdl")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=DV,color = "DV"),
                                                                           size = 1.5,
                                                                           fun = "mean", geom = "point")
  if(cent %in% c("median", "median_iqr")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=DV, color = "DV"),
                                                                               size = 1.5,
                                                             fun = "median", geom = "point")
  if(cent %in% c("mean", "mean_sdl")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=IPRED,color = "IPRED"),
                                                                           size = 1.5,
                                                                           fun = "mean", geom = "point")
  if(cent %in% c("median", "median_iqr")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=IPRED,color = "IPRED"),
                                                                               size = 1.5,
                                                                               fun = "median", geom = "point")
  if(cent %in% c("mean", "mean_sdl")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=PRED,color = "PRED"),
                                                                           size = 1.5,
                                                                           fun = "mean", geom = "point",)
  if(cent %in% c("median", "median_iqr")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=PRED,color = "PRED"),
                                                                               size = 1.5,
                                                                               fun = "median", geom = "point")

  #Plot Observed Central Tendency
  if(cent %in% c("mean", "mean_sdl")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=DV,color = "DV"),
                                                                           fun = "mean", geom = "line",
                                                                           linewidth = 0.75)
  if(cent %in% c("median", "median_iqr")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=DV,color = "DV"),
                                                                               fun = "median", geom = "line",
                                                                               linewidth = 0.75)
  #Plot Observed Central Tendency and Error Bars
  if(cent == "mean_sdl") plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=DV,color = "DV"),
                                                              fun.data = "mean_sdl", geom = "errorbar")
  if(cent == "median_iqr") plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=DV,color = "DV"),
                                                              fun.max = function(x){stats::quantile(x,0.75)},
                                                              fun.min = function(x){stats::quantile(x,0.25)},
                                                              geom = "errorbar")

  #Plot Individual Model Predictions

  if(cent %in% c("mean", "mean_sdl")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=IPRED,color = "IPRED"),
                                                                           fun = "mean", geom = "line",
                                                                           linewidth = 0.75) +
  if(cent %in% c("median", "median_iqr")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=IPRED,color = "IPRED"),
                                                                               fun = "median", geom = "line",
                                                                               linewidth = 0.75)

  #Plot Population Model Predictions
  if(cent %in% c("mean", "mean_sdl")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=PRED,color = "PRED"),
                                                                           fun = "mean", geom = "line",
                                                                           linewidth = 0.75)
  if(cent %in% c("median", "median_iqr")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=PRED,color = "PRED"),
                                                                               fun = "median", geom = "line",
                                                                               linewidth = 0.75)

  #Log Transform
  if(log_y == TRUE) plot <- plot + ggplot2::scale_y_log10(guide = "axis_logticks")

  #Define Manual Legend
  plot <- plot + ggplot2::scale_color_manual(values = output_colors, limits = c("OBS", "DV", "IPRED", "PRED"))

  #Caption
  if(show_caption == TRUE) plot <- plot + ggplot2::labs(caption = caption)

  return(plot)
}
