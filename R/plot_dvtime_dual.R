#' Wrapper function for plot_dvtime to plot two dependent variables paneled together
#'
#' @param dvid_var Variable to identify unique types of values specified in the dependent variable
#' @param dvid_values Named vector of dependent variable types to plot as DV1 and DV2.
#' @param ylab1 Character string specifying the y-axis label for DV1: Default is `"Concentration"`.
#' @param ylab2 Character string specifying the y-axis label for DV2: Default is `"Response"`.
#' @param log_y1 Logical indicator for log10 transformation of the y-axis for DV1. Default is `FALSE`.
#' @param log_y2 Logical indicator for log10 transformation of the y-axis. Default is `FALSE`
#' @inheritParams plot_dvtime
#'
#' @return A `ggplot2` plot object
#'
#' @export plot_dvtime_dual
#'
#' @examples
#'data <- df_addn(data_sad_pd, id_var = "ID", grp_var = "DOSE", sep = "mg")
#'plot_dvtime_dual(data, dv_var = "ODV", col_var = "DOSE")
#'

plot_dvtime_dual <- function(data,
                        dv_var = "DV",
                        dvid_var = "CMT",
                        dvid_values = c(DV1 = 2, DV2 = 3),
                        time_vars = c(TIME = "TIME",
                                      NTIME = "NTIME"),
                        timeu = "hours",
                        col_var = NULL,
                        grp_var = "ID",
                        dose_var = "DOSE",
                        loq = NULL,
                        loq_method = 0,
                        cent = "mean",
                        obs_dv = TRUE,
                        grp_dv = FALSE,
                        dosenorm = FALSE,
                        cfb = FALSE,
                        ylab1 = "Concentration",
                        ylab2 = "Response",
                        log_y1 = FALSE,
                        log_y2 = FALSE,
                        show_caption = TRUE,
                        n_breaks = 8,
                        theme = NULL){

  plot_dv1 <- pmxhelpr::plot_dvtime(
    data = dplyr::filter(data, !!dplyr::sym(dvid_var)==dvid_values[["DV1"]]),
    dv_var = dv_var,
    time_vars = time_vars,
    timeu = timeu,
    col_var = col_var,
    grp_var = grp_var,
    dose_var = dose_var,
    loq = loq,
    loq_method = loq_method,
    cent = cent,
    obs_dv = obs_dv,
    grp_dv = grp_dv,
    dosenorm = dosenorm,
    cfb = FALSE,
    ylab = ylab1,
    log_y = log_y1,
    show_caption = show_caption,
    n_breaks = n_breaks,
    theme = theme
  )

  plot_dv2 <- pmxhelpr::plot_dvtime(
    data = dplyr::filter(data, !!dplyr::sym(dvid_var)==dvid_values[["DV2"]]),
    dv_var = dv_var,
    time_vars = time_vars,
    timeu = timeu,
    col_var = col_var,
    grp_var = grp_var,
    dose_var = dose_var,
    loq_method = 0,
    cent = cent,
    obs_dv = obs_dv,
    grp_dv = grp_dv,
    dosenorm = FALSE,
    cfb = cfb,
    ylab = ylab2,
    log_y = log_y2,
    show_caption = show_caption,
    n_breaks = n_breaks,
    theme = theme
  )

  plot <- patchwork::wrap_plots(plot_dv1, plot_dv2) + patchwork::plot_layout(ncol = 1, nrow = 2)
  return(plot)
}
