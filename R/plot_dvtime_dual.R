#' Wrapper function for plot_dvtime to plot two dependent variables paneled together
#'
#' @param dv_var1 Character string specifying the variable containing observations for the top panel (DV1).
#'    Default is "DV".
#' @param dv_var2 Character string specifying the variable containing observations for the bottom panel (DV2).
#'    Default is "DV".
#' @param dvid_var Character string specifying the variable to identify each observation type.
#' @param dvid_val1 Value of variable specified in `dvid_var` to identify observations for top panel (DV1).
#' @param dvid_val2 Value of variable specified in `dvid_var` to identify observations for bottom panel (DV2).
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
#'plot_dvtime_dual(data, dv_var1 = "ODV", dv_var2 = "ODV")
#'

plot_dvtime_dual <- function(data,
                        dv_var1 = "DV",
                        dv_var2 = "DV",
                        dvid_var = "CMT",
                        dvid_val1 = 2,
                        dvid_val2 = 3,
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
                        cfb_base=0,
                        ylab1 = "Concentration",
                        ylab2 = "Response",
                        log_y1 = FALSE,
                        log_y2 = FALSE,
                        show_caption = TRUE,
                        n_breaks = 8,
                        theme = NULL){

  plot_dv1 <- pmxhelpr::plot_dvtime(
    data = dplyr::filter(data, !!dplyr::sym(dvid_var)==dvid_val1),
    dv_var = dv_var1,
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
    data = dplyr::filter(data, !!dplyr::sym(dvid_var)==dvid_val2),
    dv_var = dv_var2,
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
    cfb_base = cfb_base,
    ylab = ylab2,
    log_y = log_y2,
    show_caption = show_caption,
    n_breaks = n_breaks,
    theme = theme
  )

  plot <- patchwork::wrap_plots(plot_dv1, plot_dv2) + patchwork::plot_layout(ncol = 1, nrow = 2)
  return(plot)
}
