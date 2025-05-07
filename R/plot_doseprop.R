
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
#'
#'
mod_loglog <- function(data, exp_var = "PPORRES", dose_var="DOSE"){
  form <- as.formula(paste(paste0("log(",exp_var,")"),"~",paste0("log(",dose_var,")")))
  fit <- lm(form, data)
  return(fit)
}


#' Compute estimate table for log-log regression
#'
#' @param fit `lm` model object for the log-log regression
#'
#' @return `data.frame`
#' @export df_loglog
#'
#' @examples
df_loglog <- function(fit){
  tab <- data.frame(
    Intercept = coef(fit)[[1]],
    Power = coef(fit)[[2]],
    StandardError = sqrt(diag(vcov(fit)))[[2]]
  ) |>
   dplyr::mutate(`95% LCL` = Power + qt((1 - 0.95)/2, (length(fit$residuals)-1))*StandardError,
                `95% UCL` = Power + qt((1 + 0.95)/2, (length(fit$residuals)-1))*StandardError)
  return(tab)
}

