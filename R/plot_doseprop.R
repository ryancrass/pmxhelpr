
#' Perform a log-log Regression
#'
#' @param data Input dataset for log-log regression.
#'    Default expected format is output from `PKNCA::pk.nca()` (i.e., SDTM PP formatting)
#' @param exp_var Character string specifying the variable in `data` containing the exposure metric (dependent variable)
#'    Default is "PPORRES".
#' @param dose_var Character string specifying the variable in `data` containing the dose (independent variable)
#'    Default is "DOSE".
#'
#' @return `lm` object
#' @export mod_loglog
#'
#' @examples
#' mod_auc <- mod_loglog(dplyr::filter(data_sad_nca, PPTESTCD == "aucinf.obs"))
#' summary(mod_auc)
#'
#' mod_cmax <- mod_loglog(dplyr::filter(data_sad_nca, PPTESTCD == "cmax"))
#' summary(mod_cmax)
#'
mod_loglog <- function(data,
                       exp_var = "PPORRES",
                       dose_var="DOSE") {

  form <- stats::as.formula(paste(paste0("log(",exp_var,")"),"~",paste0("log(",dose_var,")")))
  fit <- stats::lm(form, data)
  return(fit)

}


#' Compute estimate table for log-log regression
#'
#' @param fit `lm` model object for the log-log regression
#' @param method character string specifying the distribution to be used to derived the confidence interval.
#'    Options are "normal" (default) and "tdist"
#' @param ci confidence interval to be calculated.
#'    Options are 0.95 (default) and 0.90
#' @param sigdigits number of significant digits for rounding
#'
#' @return `data.frame`
#' @export df_loglog
#'
#' @examples
#' mod_auc <- mod_loglog(dplyr::filter(data_sad_nca, PPTESTCD == "aucinf.obs"))
#' df_loglog(mod_auc)
#'
#' mod_cmax <- mod_loglog(dplyr::filter(data_sad_nca, PPTESTCD == "cmax"))
#' df_loglog(mod_cmax)

df_loglog <- function(fit,
                      method = "normal",
                      ci = 0.95,
                      sigdigits = 3) {

  if(!method %in% c("normal", "tdist"))
    stop("method must be 'normal' or 'tdist'")

  int <- stats::coef(fit)[[1]]
  est <- stats::coef(fit)[[2]]
  se <- sqrt(diag(stats::vcov(fit)))[[2]]
  z <- stats::qt((1 + ci)/2, (length(fit$residuals)-1))

  tab <- data.frame(
    Intercept = signif(int, digits=sigdigits),
    StandardError = signif(se, digits=sigdigits),
    CI = paste0(ci*100, "%"),
    Power = signif(est, digits = sigdigits),
    LCL = dplyr::case_when(method == "normal" & ci == 0.95 ~ signif(est - 1.96*se, digits=sigdigits),
                           method == "normal" & ci == 0.90 ~ signif(est - 1.64*se, digits=sigdigits),
                           method == "tdist" ~ signif(est - z*se, digits=sigdigits),
                           .default = NA_real_),
    UCL = dplyr::case_when(method == "normal" & ci == 0.95 ~ signif(est + 1.96*se, digits=sigdigits),
                           method == "normal" & ci == 0.90 ~ signif(est + 1.64*se, digits=sigdigits),
                           method == "tdist" ~ signif(est + z*se, digits=sigdigits),
                           .default = NA_real_)) |>
    dplyr::mutate(Proportional = dplyr::case_when(LCL < 1 & UCL < 1 ~ FALSE,
                                        LCL > 1 & UCL > 1 ~ FALSE,
                                        .default = TRUE),
                  PowerCI = paste0("Power: ", Power, " (", CI," CI ",LCL ,"-",UCL,")"),
                  Interpretation = dplyr::case_when(LCL < 1 & UCL < 1 ~ "Less than dose-proportional",
                                      LCL > 1 & UCL > 1 ~ "Greater than dose-proportional",
                                      .default = "Dose-proportional"))

  return(tab)
}


#' Compute and tabulate estimates for log-log regression
#'
#' @param metrics character vector of exposure metrics in `data` to plot
#' @param metric_var character string of variable in `data` containing the values provided in `metrics`.
#'    Default is "PPTESTCD".
#' @inheritParams mod_loglog
#' @inheritParams df_loglog
#'
#' @return `data.frame`
#' @export df_doseprop
#'
#' @examples
#' df_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"))

df_doseprop <- function(data,
                        metrics,
                        metric_var="PPTESTCD",
                        exp_var = "PPORRES",
                        dose_var = "DOSE",
                        method = "normal",
                        ci = 0.95,
                        sigdigits=3) {

  fit_list <- list()
   for(i in 1:length(metrics)) {
     fit_list[[i]] <- mod_loglog(dplyr::filter(data, !!dplyr::sym(metric_var) == metrics[i]), exp_var, dose_var)
   }

   tab_list <- list()
   for(i in 1:length(fit_list)) {
     tab_list[[i]] <- df_loglog(fit_list[[i]], method, ci, sigdigits) |>
       dplyr::mutate(!!metric_var := metrics[i])
   }

   tab <- do.call(rbind.data.frame, tab_list)
   return(tab)
}



#' Plot a dose-proportionality assessment via power law (log-log) regression
#'
#' @param se logical to display confidence interval around regression. Default is `TRUE`.
#' @inheritParams mod_loglog
#' @inheritParams df_loglog
#' @inheritParams df_doseprop
#'
#' @return a `ggplot` plot object
#' @export plot_doseprop
#'
#' @examples
#' plot_doseprop(dplyr::filter(data_sad_nca, PART == "Part 1-SAD"), metrics = c("aucinf.obs", "cmax"))

plot_doseprop <- function(data,
                          metrics,
                          metric_var="PPTESTCD",
                          exp_var = "PPORRES",
                          dose_var = "DOSE",
                          method = "normal",
                          ci = 0.95,
                          sigdigits=3,
                          se = TRUE) {

  dat <- dplyr::filter(data, !!dplyr::sym(metric_var) %in% metrics)
  tab <- df_doseprop(data, metrics, metric_var, exp_var, dose_var, method, ci) |>
    dplyr::mutate(label = paste0(!!dplyr::sym(metric_var), " | ", PowerCI))

  plot_data <- dplyr::left_join(dat, tab, by = metric_var)

  plot <-
  ggplot2::ggplot(data = plot_data, ggplot2::aes(x = !!dplyr::sym(dose_var), y = !!dplyr::sym(exp_var))) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm", formula = y~x, se = se, level = ci) +
    ggplot2::labs(x = "Dose", y = "Exposure")+
    ggplot2::scale_x_log10(guide = "axis_logticks") +
    ggplot2::scale_y_log10(guide = "axis_logticks") +
    ggplot2::facet_wrap(~label, scales = "free")+
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  return(plot)
}
