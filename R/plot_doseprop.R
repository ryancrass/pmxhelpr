
#' Perform a log-log Regression
#'
#' @param data Input dataset for log-log regression.
#'    Default expected format is output from `PKNCA::pk.nca()` (i.e., SDTM PP formatting)
#' @param exp_var Column in `data` containing the exposure metric (dependent variable).
#'    Accepts bare names or strings. Default is `PPORRES`.
#' @param dose_var Column in `data` containing the dose (independent variable).
#'    Accepts bare names or strings. Default is `DOSE`.
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
                       exp_var = PPORRES,
                       dose_var = DOSE) {

  exp_var_str  <- rlang::as_name(rlang::ensym(exp_var))
  dose_var_str <- rlang::as_name(rlang::ensym(dose_var))

  check_df(data, "data")
  check_varsindf(data, exp_var_str, "data", "exp_var")
  check_varsindf(data, dose_var_str, "data", "dose_var")
  if(nrow(data) < 2) {rlang::abort(message = "argument `data` must contain at least 2 rows for log-log regression")}

  form <- stats::as.formula(paste(paste0("log(",exp_var_str,")"),"~",paste0("log(",dose_var_str,")")))
  fit <- stats::lm(form, data)
  return(fit)

}


#' Compute estimate table for log-log regression
#'
#' @param fit `lm` model object for the log-log regression
#' @param method character string specifying the distribution to be used to derived the confidence interval.
#'    Options are "normal" (default) and "tdist"
#' @param ci confidence interval to be calculated.
#'    Options 0.90 (default) and 0.95
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
                      ci = 0.9,
                      sigdigits = 3) {

  check_lm(fit, "fit")
  check_loglog_args(method, ci, sigdigits)

  int <- stats::coef(fit)[[1]]
  est <- stats::coef(fit)[[2]]
  se <- sqrt(diag(stats::vcov(fit)))[[2]]
  z <- stats::qt((1 + ci)/2, (length(fit$residuals)-1))

  tab <- data.frame(
    Intercept = signif(int, digits=sigdigits),
    StandardError = signif(se, digits=sigdigits),
    CI = paste0(ci*100, "%"),
    Power = signif(est, digits = sigdigits),
    LCL = dplyr::case_when(method == "normal" ~ signif(est - stats::qnorm((1 + ci)/2)*se, digits=sigdigits),
                           method == "tdist" ~ signif(est - z*se, digits=sigdigits),
                           .default = NA_real_),
    UCL = dplyr::case_when(method == "normal" ~ signif(est + stats::qnorm((1 + ci)/2)*se, digits=sigdigits),
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
#' @param metric_var Column in `data` containing the values provided in `metrics`.
#'    Accepts bare names or strings. Default is `PPTESTCD`.
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
                        metric_var = PPTESTCD,
                        exp_var = PPORRES,
                        dose_var = DOSE,
                        method = "normal",
                        ci = 0.90,
                        sigdigits=3) {

  metric_var_str <- rlang::as_name(rlang::ensym(metric_var))
  exp_var_str    <- rlang::as_name(rlang::ensym(exp_var))
  dose_var_str   <- rlang::as_name(rlang::ensym(dose_var))

  check_df(data, "data")
  check_varsindf(data, metric_var_str, "data", "metric_var")
  check_varsindf(data, exp_var_str, "data", "exp_var")
  check_varsindf(data, dose_var_str, "data", "dose_var")
  check_loglog_args(method, ci, sigdigits)

  tab_list <- lapply(metrics, function(m) {
    dat_m <- dplyr::filter(data, .data[[metric_var_str]] == m)
    fit   <- rlang::inject(mod_loglog(dat_m, exp_var = !!exp_var_str, dose_var = !!dose_var_str))
    tab   <- df_loglog(fit, method, ci, sigdigits)
    tab[[metric_var_str]] <- m
    tab
  })

  do.call(rbind.data.frame, tab_list)
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
                          metric_var = PPTESTCD,
                          exp_var = PPORRES,
                          dose_var = DOSE,
                          method = "normal",
                          ci = 0.90,
                          sigdigits=3,
                          se = TRUE) {

  metric_var_str <- rlang::as_name(rlang::ensym(metric_var))
  exp_var_str    <- rlang::as_name(rlang::ensym(exp_var))
  dose_var_str   <- rlang::as_name(rlang::ensym(dose_var))

  check_df(data, "data")
  check_varsindf(data, metric_var_str, "data", "metric_var")
  check_varsindf(data, dose_var_str, "data", "dose_var")
  check_levelsinvar(data, metric_var_str, metrics, "metric_var", "metrics")
  check_loglog_args(method, ci, sigdigits)

  dat <- dplyr::filter(data, .data[[metric_var_str]] %in% metrics)
  tab <- rlang::inject(df_doseprop(data, metrics,
                     metric_var = !!metric_var_str, exp_var = !!exp_var_str,
                     dose_var = !!dose_var_str, method, ci, sigdigits)) |>
    dplyr::mutate(label = paste0(.data[[metric_var_str]], " | ", PowerCI))

  plot_data <- dplyr::left_join(dat, tab, by = metric_var_str)

  plot <-
  ggplot2::ggplot(data = plot_data, ggplot2::aes(x = !!rlang::sym(dose_var_str), y = !!rlang::sym(exp_var_str))) +
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
