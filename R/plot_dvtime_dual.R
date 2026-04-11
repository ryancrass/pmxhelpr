#' Wrapper function for plot_dvtime to plot two dependent variables paneled together
#'
#' @param dv_var1 Column containing observations for the top panel (DV1). Accepts bare names or strings.
#'    Default is `DV`.
#' @param dv_var2 Column containing observations for the bottom panel (DV2). Accepts bare names or strings.
#'    Default is `DV`.
#' @param dvid_var Column to identify each observation type. Accepts bare names or strings.
#' @param dvid_val1 Value of variable specified in `dvid_var` to identify observations for the top panel (DV1).
#' @param dvid_val2 Value of variable specified in `dvid_var` to identify observations for the bottom panel (DV2).
#' @param ylab1 Character string specifying the y-axis label for the top panel (DV1): Default is `"Concentration"`.
#' @param ylab2 Character string specifying the y-axis label for the bottom panel (DV2): Default is `"Response"`.
#' @param log_y1 Logical indicator for log10 transformation of the y-axis for the top panel (DV1). Default is `FALSE`.
#' @param log_y2 Logical indicator for log10 transformation of the y-axis for the bottom panel (DV2). Default is `FALSE`.
#' @param show_caption1 Logical indicating if a caption should be show describing the data plotted for the top panel (DV1).
#' @param show_caption2 Logical indicating if a caption should be show describing the data plotted for the bottom panel (DV2).
#' @param onelegend Logical indicator if the plot legends should be collected into a single legend. Default is `FALSE`.
#' @inheritParams plot_dvtime
#'
#' @return A `ggplot2` plot object
#'
#' @export plot_dvtime_dual
#'
#' @examples
#'data <- df_addn(dplyr::mutate(data_sad, Dose = DOSE), grp_var = Dose, sep = "mg")
#'plot_dvtime_dual(data, dv_var1 = ODV, dv_var2 = ODV, col_var = Dose)
#'

plot_dvtime_dual <- function(data,
                        dv_var1 = DV,
                        dv_var2 = DV,
                        dvid_var = CMT,
                        dvid_val1 = 2,
                        dvid_val2 = 3,
                        time_vars = c(TIME = "TIME",
                                      NTIME = "NTIME"),
                        timeu = "hours",
                        col_var = NULL,
                        grp_var = ID,
                        dose_var = DOSE,
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
                        show_caption1 = TRUE,
                        show_caption2 = TRUE,
                        n_breaks = 8,
                        onelegend = TRUE,
                        theme = NULL){

  dv_var1_str  <- rlang::as_name(rlang::ensym(dv_var1))
  dv_var2_str  <- rlang::as_name(rlang::ensym(dv_var2))
  dvid_var_str <- rlang::as_name(rlang::ensym(dvid_var))
  grp_var_str  <- rlang::as_name(rlang::ensym(grp_var))
  dose_var_str <- rlang::as_name(rlang::ensym(dose_var))
  col_var_str  <- capture_col(rlang::enquo(col_var))

  plot_dv1 <- rlang::inject(pmxhelpr::plot_dvtime(
    data = dplyr::filter(data, .data[[dvid_var_str]]==dvid_val1),
    dv_var = !!dv_var1_str,
    time_vars = time_vars,
    timeu = timeu,
    col_var = !!col_var_str,
    grp_var = !!grp_var_str,
    dose_var = !!dose_var_str,
    loq = loq,
    loq_method = loq_method,
    cent = cent,
    obs_dv = obs_dv,
    grp_dv = grp_dv,
    dosenorm = dosenorm,
    cfb = FALSE,
    ylab = ylab1,
    log_y = log_y1,
    show_caption = show_caption1,
    n_breaks = n_breaks,
    theme = theme
  ))

  plot_dv2 <- rlang::inject(pmxhelpr::plot_dvtime(
    data = dplyr::filter(data, .data[[dvid_var_str]]==dvid_val2),
    dv_var = !!dv_var2_str,
    time_vars = time_vars,
    timeu = timeu,
    col_var = !!col_var_str,
    grp_var = !!grp_var_str,
    dose_var = !!dose_var_str,
    loq_method = 0,
    cent = cent,
    obs_dv = obs_dv,
    grp_dv = grp_dv,
    dosenorm = FALSE,
    cfb = cfb,
    cfb_base = cfb_base,
    ylab = ylab2,
    log_y = log_y2,
    show_caption = show_caption2,
    n_breaks = n_breaks,
    theme = theme
  ))

  plot <- patchwork::wrap_plots(plot_dv1, plot_dv2) + patchwork::plot_layout(ncol = 1, nrow = 2)
  if(onelegend == TRUE) {plot <- plot + patchwork::plot_layout(guides = "collect")}
  return(plot)
}
