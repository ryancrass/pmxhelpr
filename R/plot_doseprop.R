
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
#'
#' @return `data.frame`
#' @export df_loglog
#'
#' @examples
#' #example needed
df_loglog <- function(fit, method = "normal"){
  if(!method %in% c("normal", "tdist"))
    stop("method must be 'normal' or 'tdist'")

  tab <- data.frame(
    Intercept = stats::coef(fit)[[1]],
    Power = stats::coef(fit)[[2]],
    StandardError = sqrt(diag(stats::vcov(fit)))[[2]],
    LCL95 = dplyr::case_when(method == "normal" ~ stats::coef(fit)[[2]] - 1.96*sqrt(diag(stats::vcov(fit)))[[2]],
                                 method == "tdist" ~ stats::coef(fit)[[2]] -
                                   stats::qt((1 + 0.95)/2, (length(fit$residuals)-1))*sqrt(diag(stats::vcov(fit)))[[2]],
                                 .default = NA_real_),
    UCL95 = dplyr::case_when(method == "normal" ~ stats::coef(fit)[[2]] + 1.96*sqrt(diag(stats::vcov(fit)))[[2]],
                                 method == "tdist" ~ stats::coef(fit)[[2]] +
                                   stats::qt((1 + 0.95)/2, (length(fit$residuals)-1))*sqrt(diag(stats::vcov(fit)))[[2]],
                                 .default = NA_real_))

  return(tab)
}

