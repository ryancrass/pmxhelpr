
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
#' #example needed
#'
mod_loglog <- function(data, exp_var = "PPORRES", dose_var="DOSE"){
  form <- stats::as.formula(paste(paste0("log(",exp_var,")"),"~",paste0("log(",dose_var,")")))
  fit <- stats::lm(form, data)
  return(fit)
}


#' Compute estimate table for log-log regression
#'
#' @param fit `lm` model object for the log-log regression
#' @param method character string specifying the distrubtion to be used to derived the confidence interval.
#'    Options are "normal" (default) and "tdist"
#' @param ci confidence interval to be calculated.
#'    Options are 0.95 (default) and 0.90
#'
#' @return `data.frame`
#' @export df_loglog
#'
#' @examples
#' #example needed
df_loglog <- function(fit, method = "normal", ci = 0.95){
  if(!method %in% c("normal", "tdist"))
    stop("method must be 'normal' or 'tdist'")

  tab <- data.frame(
    Intercept = stats::coef(fit)[[1]],
    Power = stats::coef(fit)[[2]],
    StandardError = sqrt(diag(stats::vcov(fit)))[[2]],
    CI = paste0(ci*100, "%"),
    LCL = dplyr::case_when(method == "normal" & ci == 0.95 ~ stats::coef(fit)[[2]] - 1.96*sqrt(diag(stats::vcov(fit)))[[2]],
                           method == "normal" & ci == 0.90 ~ stats::coef(fit)[[2]] - 1.64*sqrt(diag(stats::vcov(fit)))[[2]],
                           method == "tdist" ~ stats::coef(fit)[[2]] -
                                   stats::qt((1 + ci)/2, (length(fit$residuals)-1))*sqrt(diag(stats::vcov(fit)))[[2]],
                                 .default = NA_real_),
    UCL = dplyr::case_when(method == "normal" & ci == 0.95 ~ stats::coef(fit)[[2]] + 1.96*sqrt(diag(stats::vcov(fit)))[[2]],
                           method == "normal" & ci == 0.90 ~ stats::coef(fit)[[2]] + 1.64*sqrt(diag(stats::vcov(fit)))[[2]],
                           method == "tdist" ~ stats::coef(fit)[[2]] +
                                   stats::qt((1 + ci)/2, (length(fit$residuals)-1))*sqrt(diag(stats::vcov(fit)))[[2]],
                                 .default = NA_real_))

  return(tab)
}

plot_doseprop <- function(){
  plot <- ggplot(data = data_plot, aes(x = DOSE, y = PPORRES)) +
    geom_point() +
    labs(x = "Dose", y = "Concentration") +
    geom_smooth(method = "lm", formula = y~x) +
    scale_x_log10() +
    scale_y_log10() +
    theme_bw()
}
