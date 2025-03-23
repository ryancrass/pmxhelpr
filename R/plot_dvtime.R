

#Pooled GOF Plots
#Purpose: post-process nlmixr2 output to derive pooled overlay GOF plots
#Arguments
#fit = nlmixr2 fit object
#strat_var = character, variable to stratify plots by (e.g., "DOSE")
#units_dv = character, units of the dependent variable (e.g., "mg/L")
#units_time = character, units of the time variable (e.g., "hours")
#log_y = logical, log-scale y-axis (default = FALSE)

#' Plot a dependent variable versus time
#'
#' @param data Input dataset. Must contain the variables: `"ID"`, `"DV"` `"MDV"`.
#' @param dv_var Character string of the dependent variable. Default is `"DV"`.
#' @param col_var Character string of the name of the variable to map to the color aesthetic.
#' @param loq_method Method for handling data below the lower limit of quantification (BLQ) in the plot.
#'   Options are:
#'     + `0` : No handling. Plot input dataset `DV` vs `TIME` as is. (default)
#'     + `1` : Impute all BLQ data at `TIME` <= 0 to 0 and all BLQ data at `TIME` > 0 to 1/2 x `loq`.
#'        Useful for plotting concentration-time data with some data BLQ on the linear scale
#'     + `2` : Impute all BLQ data at `TIME` <= 0 to 1/2 x `loq` and all BLQ data at `TIME` > 0 to 1/2 x `loq`.
#'        Useful for plotting concentration-time data with some data BLQ on the log scale where 0 cannot be displayed
#' @param cent Character string specifying the central tendency measure to plot.
#'  Options are:
#'    + Mean only: `"mean"` (default)
#'    + Mean +/- Standard Deviation: `"mean_sdl"`
#'    + Median only: `"median"`
#'    + None: `"none"`
#' @param obs_dv Logical indicating if observed data points should be shown. Default is `TRUE`.
#' @param ind_dv Logical indiciating if observed data points shoudld be connected within an individual (i.e., spaghetti plot).
#'  Defaut is `FALSE`.
#' @param cfb Logical indicating if dependent variable is a change from baseline.
#'    Plots a reference line at y = 0. Default is `FALSE`.
#' @param ylab Character string specifing the y-axis label: Default is `"Concentration"`.
#' @param xlab  Character string specfing the x-axis label: Default is `"Time"`.
#' @param log_y Logical indicator for log10 transformation of the y-axis.
#' @param show_caption Logical indicating if a caption should be show describing the data plotted
#' @inheritParams df_mrgsim_replicate
#' @inheritParams plot_vpc_exactbins
#'
#' @return A ggplot2 plot object
#'
#' @export plot_dvtime
#'
#' @examples
#'data <- dplyr::mutate(data_sad, Dose = factor(DOSE))
#'plot <- plot_dvtime(data, dv_var = c(DV = "ODV"), cent = "median", col_var = "Dose")
#'plot
#'
#'


plot_dvtime <- function(data,
                        dv_var = c(DV = "DV"),
                        time_vars = c(TIME = "TIME",
                                      NTIME = "NTIME"),
                        col_var = NULL,
                        loq = NULL,
                        loq_method = 0,
                        cent = "mean",
                        obs_dv = TRUE,
                        ind_dv = FALSE,
                        cfb = FALSE,
                        ylab = "Concentration",
                        xlab = "Time",
                        log_y = FALSE,
                        show_caption = TRUE){

  #Checks
  check_df(data)
  check_varsindf(data, dv_var[["DV"]])
  check_varsindf(data, time_vars[["TIME"]])
  check_varsindf(data, time_vars[["NTIME"]])
  check_varsindf(data, "MDV")
  check_varsindf(data, col_var)
  if(!is.null(col_var)) {check_factor(data, col_var)}
  check_loq_method(loq, loq_method, data)

  ##Data Rename
  data <- data |>
    dplyr::rename((dplyr::any_of(c(dv_var, time_vars))))

  ##BLQ Handling
  if(loq_method==1) {
    data <- data |>
      dplyr::mutate(LOQ = ifelse(is.null(loq), LLOQ, loq)) |>
      dplyr::mutate(DV = dplyr::case_when(EVID != 0 ~ NA_real_,
                                          MDV == 0 ~ DV,
                                          TIME <= 0 ~ 0,
                                          TIME > 0 ~ 0.5*LOQ))
  }

  if(loq_method==2) {
    data <- data |>
      dplyr::mutate(LOQ = ifelse(is.null(loq), LLOQ, loq)) |>
      dplyr::mutate(DV = dplyr::case_when(EVID != 0 ~ NA_real_,
                                          MDV == 0 ~ DV,
                                          MDV == 1 ~ 0.5*LOQ))
  }

  lloq <- ifelse("LOQ" %in% colnames(data), unique(data$LOQ), NA_real_)

  #Determine Caption
  capdf <- data.frame("cent" = rep(c("mean", "mean_sdl", "median", "none"),
                                   2),
                      "cap" = c(c("mean","mean + SD error bars","median", ""),
                                c("geometric mean","geo. mean + geo. SD error bars","median", "")),
                      "log_y" = c(rep(FALSE, 4),
                                  rep(TRUE, 4))
  )

  cap1 <- capdf$cap[capdf$cent==cent&capdf$log_y==log_y]

  cap2_dv <- "Open circles are observations"
  cap2_ind <- "Thin lines connect observations within an individual"
  cap2_inddv <- "Thin lines connect observations (open circles) within an individual"

  caption <- if(cent == "none" & ind_dv == FALSE & obs_dv == TRUE) {
    paste0(cap2_dv)
  } else if (cent == "none" & ind_dv == TRUE & obs_dv == FALSE) {
    paste0(cap2_ind)
  } else if (cent == "none" & ind_dv == TRUE & obs_dv == TRUE) {
    paste0(cap2_inddv)
  } else if(ind_dv==FALSE & obs_dv==FALSE) {
    paste0("Solid circles and thick lines are the ", cap1)
  } else if(ind_dv==FALSE & obs_dv==TRUE){
    paste0("Solid circles and thick lines are the ", cap1, "\n", cap2_dv)
  } else if(ind_dv==TRUE & obs_dv==FALSE){
    paste0("Solid circles and thick lines are the ", cap1, "\n", cap2_ind)
  } else if(ind_dv==TRUE & obs_dv==TRUE){
    paste0("Solid circles and thick lines are the ", cap1, "\n", cap2_inddv)
  }



###Plot

  #Initialize Plot and Primary Aesthetics
  if(is.null(col_var)) {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = TIME, y=DV)) +
      ggplot2::labs(x=xlab, y=ylab)
  } else {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = TIME, y=DV, color = !!as.symbol(col_var))) +
      ggplot2::labs(x=xlab, y=ylab)
  }

  #Reference Lines: Y=0 (cfb = TRUE) or Y=LLOQ (loq_method = 1,2)
  if(cfb == TRUE) plot <- plot + ggplot2::geom_hline(yintercept = 0, linewidth = 1, linetype = "dashed")
  if(loq_method %in% c(1,2)) plot <- plot + ggplot2::geom_hline(yintercept = lloq, linewidth = 0.5, linetype = "dashed")

  #Show Observed Data Points
  if(obs_dv == TRUE) plot <- plot +  ggplot2::geom_point(shape=1, size=0.75, alpha = 0.5)
  #Connect Observed Data Points within an Individual
  if(ind_dv == TRUE) plot <- plot + ggplot2::geom_line(ggplot2::aes(x = TIME, y = DV, group = ID),
                                                       linewidth = 0.5, alpha = 0.5)

  #Plot Central Tendency Points
  if(cent %in% c("mean", "mean_sdl")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=DV), size = 1.25,
                                                                           fun.y = "mean", geom = "point")
  if(cent == "median") plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=DV), size = 1.25,
                                                             fun.y = "median", geom = "point")

  #Plot Central Lines
  if(cent %in% c("mean", "mean_sdl")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=DV), linewidth = 1,
                                                                           fun.y = "mean", geom = "line")
  if(cent == "median") plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=DV), linewidth = 1,
                                                            fun.y = "median", geom = "line")

  #Plot Error Bars
  if(cent == "mean_sdl") plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=DV),
                                                              fun.data = "mean_sdl", geom = "errorbar")

  #Log Transform
  if(log_y == TRUE) plot <- plot + ggplot2::scale_y_log10()

  #Caption
  if(show_caption == TRUE) plot <- plot + ggplot2::labs(caption = caption)

  return(plot)
}
