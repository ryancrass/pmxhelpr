#' Plot a dependent variable versus time
#'
#' @param data Input dataset. Must contain the variables: `"ID"`, `"DV"` `"MDV"`.
#' @param dv_var Character string of the dependent variable. Default is `"DV"`.
#' @param timeu Character string specifying units for the time variable.
#'    Passed to `breaks_time` and assigned to default x-axis label.
#'    Options include:
#'    + "hours" (default)
#'    + "days"
#'    + "weeks"
#'    + "months"
#' @param n_breaks Number of breaks requested for x-axis. Default is 5.
#' @param col_var Character string of the name of the variable to map to the color aesthetic.
#' @param grp_var Character string of the variable to map to the group aesthetic. Default is `"ID"`
#' @param loq Numeric value of the lower limit of quantification (LLOQ) for the assay.
#'  Must be coercible to a numeric if specified. Can be `NULL` if variable `LLOQ` is present in `data`
#'  Specifying this argument implies that `DV`is missing in `data` where < LLOQ.
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
#'    + Median +/- Interquartile Range: `median_iqr`
#'    + None: `"none"`
#' @param obs_dv Logical indicating if observed data points should be shown. Default is `TRUE`.
#' @param grp_dv Logical indicating if observed data points should be connected within a group (i.e., spaghetti plot).
#'    Default is `FALSE`.
#' @param dosenorm logical indicating if observed data points should be dose normalized. Default is `FALSE`,
#'    Requires variable `DOSE` to be present in `data`
#' @param cfb Logical indicating if dependent variable is a change from baseline.
#'    Plots a reference line at y = 0. Default is `FALSE`.
#' @param ylab Character string specifing the y-axis label: Default is `"Concentration"`.
#' @param log_y Logical indicator for log10 transformation of the y-axis.
#' @param show_caption Logical indicating if a caption should be show describing the data plotted
#' @inheritParams df_mrgsim_replicate
#'
#' @return A `ggplot2` plot object
#'
#' @export plot_dvtime
#'
#' @examples
#'data <- dplyr::mutate(data_sad, Dose = factor(DOSE))
#'plot_dvtime(data, dv_var = c(DV = "ODV"), cent = "median", col_var = "Dose")
#'

plot_dvtime <- function(data,
                        dv_var = c(DV = "DV"),
                        time_vars = c(TIME = "TIME",
                                      NTIME = "NTIME"),
                        timeu = "hours",
                        col_var = NULL,
                        grp_var = "ID",
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

  #Checks
  check_df(data)
  check_varsindf(data, dv_var[["DV"]])
  check_varsindf(data, time_vars[["TIME"]])
  check_varsindf(data, time_vars[["NTIME"]])
  check_varsindf(data, "MDV")
  check_timeu(timeu)
  check_varsindf(data, col_var)
  if(!is.null(col_var)) {check_factor(data, col_var)}
  if(dosenorm == TRUE){check_varsindf(data, "DOSE")}
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

  #Dose-normalize if requested
  if(dosenorm == TRUE) {
    data <- data |>
      dplyr::mutate(DV = DV/DOSE)
  }

  #Determine Caption
  capdf <- data.frame("cent" = rep(c("mean", "mean_sdl",
                                     "median", "median_iqr",
                                     "none"),
                                   2),
                      "cap" = c(c("mean","mean + SD error bars",
                                  "median", "median + IQR error bars", ""),
                                c("geometric mean","geo. mean + geo. SD error bars",
                                  "median", "median + IQR error bars", "")),
                      "log_y" = c(rep(FALSE, 5),
                                  rep(TRUE, 5))
  )

  cap1 <- capdf$cap[capdf$cent==cent&capdf$log_y==log_y]

  cap2_dv <- "Open circles are observations"
  cap2_ind <- "Thin lines connect observations within an individual"
  cap2_inddv <- "Thin lines connect observations (open circles) within an individual"

  caption <- if(cent == "none" & grp_dv == FALSE & obs_dv == TRUE) {
    paste0(cap2_dv)
  } else if (cent == "none" & grp_dv == TRUE & obs_dv == FALSE) {
    paste0(cap2_ind)
  } else if (cent == "none" & grp_dv == TRUE & obs_dv == TRUE) {
    paste0(cap2_inddv)
  } else if(grp_dv==FALSE & obs_dv==FALSE) {
    paste0("Solid circles and thick lines are the ", cap1)
  } else if(grp_dv==FALSE & obs_dv==TRUE){
    paste0("Solid circles and thick lines are the ", cap1, "\n", cap2_dv)
  } else if(grp_dv==TRUE & obs_dv==FALSE){
    paste0("Solid circles and thick lines are the ", cap1, "\n", cap2_ind)
  } else if(grp_dv==TRUE & obs_dv==TRUE){
    paste0("Solid circles and thick lines are the ", cap1, "\n", cap2_inddv)
  }

  #Determine breaks
  xbreaks <- breaks_time(x = sort(unique(data$NTIME)), unit = timeu, n = n_breaks)


###Plot

  #Initialize Plot and Primary Aesthetics
  if(is.null(col_var)) {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = TIME, y=DV)) +
      ggplot2::labs(x=paste0("Time (", timeu, ")"), y=ylab) +
      ggplot2::scale_x_continuous(breaks = xbreaks) +
      ggplot2::theme_bw()
  } else {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = TIME, y=DV, color = !!dplyr::sym(col_var))) +
      ggplot2::labs(x=paste0("Time (", timeu, ")"), y=ylab) +
      ggplot2::scale_x_continuous(breaks = xbreaks) +
      ggplot2::theme_bw()
  }

  #Reference Lines: Y=0 (cfb = TRUE) or Y=LLOQ (loq_method = 1,2)
  if(cfb == TRUE) plot <- plot + ggplot2::geom_hline(yintercept = 0, linewidth = 1, linetype = "dashed")
  if(loq_method %in% c(1,2) & dosenorm==FALSE) plot <- plot + ggplot2::geom_hline(yintercept = lloq, linewidth = 0.5, linetype = "dashed")

  #Show Observed Data Points
  if(obs_dv == TRUE) plot <- plot +  ggplot2::geom_point(shape=1, size=0.75, alpha = 0.5)
  #Connect Observed Data Points within an Individual
  if(grp_dv == TRUE) plot <- plot + ggplot2::geom_line(ggplot2::aes(x = TIME, y = DV, group = !!dplyr::sym(grp_var)),
                                                       linewidth = 0.5, alpha = 0.5)

  #Plot Central Tendency Points
  if(cent %in% c("mean", "mean_sdl")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=DV), size = 1.25,
                                                                           fun.y = "mean", geom = "point")
  if(cent == "median") plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=DV), size = 1.25,
                                                             fun.y = "median", geom = "point")

  #Plot Central Lines
  if(cent %in% c("mean", "mean_sdl")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=DV), linewidth = 1,
                                                                           fun.y = "mean", geom = "line")
  if(cent %in% c("median", "median_iqr")) plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=DV), linewidth = 1,
                                                            fun.y = "median", geom = "line")

  #Plot Error Bars
  if(cent == "mean_sdl") plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=DV),
                                                              fun.data = "mean_sdl", geom = "errorbar")
  if(cent == "median_iqr") plot <- plot + ggplot2::stat_summary(ggplot2::aes(x=NTIME, y=DV),
                                                              fun.max = function(x){stats::quantile(x,0.75)},
                                                              fun.min = function(x){stats::quantile(x,0.25)},
                                                              geom = "errorbar")

  #Log Transform
  if(log_y == TRUE) plot <- plot + ggplot2::scale_y_log10(guide = "axis_logticks") +
    ggplot2::theme(panel.grid.minor.yl = ggplot2::element_blank())

  #Caption
  if(show_caption == TRUE) plot <- plot + ggplot2::labs(caption = caption)

  return(plot)
}






#' Determine axis breaks automatically for time variables
#'
#' @param x Numeric vector of times from which to determine breaks
#' @param unit Character string for time units.
#'    Options include:
#'    + "hours" (default)
#'    + "days"
#'    + "weeks"
#'    + "months"
#' @param n Ideal number of axis breaks requested (default = 5). Passed to `labeling::extended()`
#'
#' @return A numeric vector of breaks
#'
#' @export breaks_time
#'
#' @examples
#'ntimes <- sort(unique(data_sad$NTIME))
#'breaks <- breaks_time(ntimes)
#'
#'
breaks_time <- function(x, unit="hours", n=8) {
  check_timeu(unit)
  rng <- range(x, na.rm = TRUE)

  if (unit == "hours") {
    scale <- 24
  } else if (unit == "days") {
    scale <- 7
  } else if (unit == "weeks") {
    scale <- 1
  } else if (unit == "months") {
    scale <- 1
  }

  rng <- rng / scale

  if(max(rng, na.rm = TRUE)<=1) {
    if(unit == "hours") Ql <- c(4/24, 8/24, 12/24, 1)
    if(unit == "days") Ql <- c(1/7, 1)
    if(unit %in% c("weeks", "months")) Ql <- c(0.5, 1)

    breaks <- labeling::extended(
      rng[1], rng[2], n,
      Q = Ql,
      only.loose = FALSE)*scale

    breaks <- breaks[breaks<=max(x, na.rm=TRUE)]
  } else {
    breaks <- labeling::extended(
      rng[1], rng[2], n,
      Q = c(1, 2, 4, 7),
      only.loose = FALSE)*scale

    breaks <- breaks[breaks<=max(x, na.rm = TRUE)]
  }

  return(breaks)
}
