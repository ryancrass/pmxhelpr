#' Internal helper: Apply BLQ censoring rules to data
#'
#' @param data data.frame containing data to censor
#' @param pred_vars Character vector of prediction variable names (e.g., `"PRED"`, `"IPRED"`)
#'    to apply threshold-based BLQ imputation. Default is `NULL`.
#' @inheritParams plot_dvtime
#'
#' @return A data.frame with BLQ censoring applied
#' @keywords internal
#' @examples
#' data <- dplyr::rename(dplyr::filter(data_sad, CMT %in% c(1,2)), DV = ODV)
#' data_loq1 <- pmxhelpr:::df_prep_blq(data, loq = 10, loq_method = 1)
#' data_loq2 <- pmxhelpr:::df_prep_blq(data, loq = 10, loq_method = 2)
#'
df_prep_blq <- function(data, loq, loq_method, pred_vars = NULL) {
  if(!loq_method %in% c(1, 2)) return(data)

  data <- resolve_loq(data, loq)

  if(loq_method == 1) {
    data <- data |>
      dplyr::mutate(DV = dplyr::case_when(.data$EVID != 0 ~ NA_real_,
                                           .data$MDV == 0 ~ .data$DV,
                                           .data$TIME <= 0 ~ 0,
                                           .data$TIME > 0 ~ 0.5 * .data$LOQ))
    for(v in pred_vars) {
      data <- data |>
        dplyr::mutate("{v}" := dplyr::case_when(
          .data$EVID != 0 ~ NA_real_,
          .data$TIME <= 0 ~ 0,
          .data[[v]] >= .data$LOQ ~ .data[[v]],
          .data[[v]] < .data$LOQ ~ 0.5 * .data$LOQ))
    }
  }

  if(loq_method == 2) {
    data <- data |>
      dplyr::mutate(DV = dplyr::case_when(.data$EVID != 0 ~ NA_real_,
                                           .data$MDV == 0 ~ .data$DV,
                                           .data$MDV == 1 ~ 0.5 * .data$LOQ))
    for(v in pred_vars) {
      data <- data |>
        dplyr::mutate("{v}" := dplyr::case_when(
          .data$EVID != 0 ~ NA_real_,
          .data[[v]] >= .data$LOQ ~ .data[[v]],
          .data[[v]] < .data$LOQ ~ 0.5 * .data$LOQ))
    }
  }

  return(data)
}


#' Internal helper: Prepare data for plotting of a dependent variable versus time
#'
#' Validates inputs, renames time and output variables to internal standards,
#' applies BLQ imputation, and optionally applies dose normalization.
#'
#' @param data Input data.frame containing pharmacometric data.
#' @param time_var_str String name of the actual time column in `data`.
#' @param ntime_var_str String name of the nominal time column in `data`.
#' @param dv_var_str String name of the dependent variable column in `data`.
#' @param pred_var_str String name of the population prediction column in `data`, or `NULL`.
#' @param ipred_var_str String name of the individual prediction column in `data`, or `NULL`.
#' @param loq Numeric value of LLOQ, or `NULL`.
#' @param loq_method Integer (0, 1, or 2) specifying BLQ handling method.
#' @param dose_var_str String specifying the dose column name, or `NULL`.
#' @param col_var_str String specifying the color variable column name, or `NULL`.
#' @param id_var_str String specifying the group column name for spaghetti lines, or `NULL`.
#' @param dosenorm Logical indicating if dose normalization should be applied.
#' @param ref Numeric y-intercept for a horizontal reference line, or `NULL` for none.
#' @param blq_mode One of `"obs"` (default) or `"all"`. Controls which columns
#'    receive BLQ imputation: `"obs"` imputes the renamed `DV` only; `"all"`
#'    additionally imputes any `PRED` / `IPRED` columns that were renamed via
#'    `pred_var_str` / `ipred_var_str`. Has no effect when `loq_method = 0`.
#'
#' @return A named list with elements `data` (processed data.frame) and `lloq`
#'    (the LLOQ line value: the highest LLOQ in the data, or `NA`).
#' @keywords internal
#' @examples
#' data <- dplyr::rename(dplyr::filter(data_sad, CMT %in% c(1,2)), DV = ODV)
#' prep <- pmxhelpr:::df_prep_dvtime(data, time_var_str = "TIME", ntime_var_str = "NTIME")
#' head(prep$data)
#'
df_prep_dvtime <- function(data,
                           time_var_str = "TIME",
                           ntime_var_str = "NTIME",
                           dv_var_str = "DV",
                           pred_var_str = NULL,
                           ipred_var_str = NULL,
                           dose_var_str = NULL,
                           col_var_str = NULL,
                           id_var_str = NULL,
                           loq = NULL,
                           loq_method = 0,
                           dosenorm = FALSE,
                           ref = NULL,
                           blq_mode = c("obs", "all")) {

  blq_mode <- match.arg(blq_mode)

  check_df(data, "data")
  check_varsindf(data, time_var_str, "data", "time_var")
  check_varsindf(data, ntime_var_str, "data", "ntime_var")
  check_varsindf(data, dv_var_str, "data", "dv_var")
  if (!is.null(pred_var_str)) check_varsindf(data, pred_var_str, "data", "pred_var")
  if (!is.null(ipred_var_str)) check_varsindf(data, ipred_var_str, "data", "ipred_var")
  check_varsindf(data, "MDV", "data", "MDV")
  check_varsindf(data, "EVID", "data", "EVID")
  if (!is.null(col_var_str)) check_varsindf(data, col_var_str, "data", "col_var")
  if (!is.null(id_var_str)) check_varsindf(data, id_var_str, "data", "id_var")
  if (isTRUE(dosenorm)) check_varsindf(data, dose_var_str, "data", "dose_var")
  loq_method <- check_loq_method(loq, loq_method, data)
  if (!is.null(ref)) check_numeric_strict(ref, "ref")

  data <- df_prep_timevars(data, time_var_str, ntime_var_str)
  rename_vec <- c(DV = dv_var_str)
  if (!is.null(pred_var_str)) rename_vec <- c(rename_vec, PRED = pred_var_str)
  if (!is.null(ipred_var_str)) rename_vec <- c(rename_vec, IPRED = ipred_var_str)
  data <- dplyr::rename(data, dplyr::any_of(rename_vec))

  if (!is.null(dose_var_str) && dose_var_str != "DOSE") {
    data <- dplyr::rename(data, dplyr::any_of(c(DOSE = dose_var_str)))
  }

  if (!is.null(col_var_str)) {
    data[[col_var_str]] <- order_col_factor(data[[col_var_str]])
  }

  data <- dplyr::filter(data, .data$EVID == 0)
  check_single_cmt(data)

  pred_vars <- if (blq_mode == "all") {
    c(if (!is.null(pred_var_str)) "PRED",
      if (!is.null(ipred_var_str)) "IPRED")
  } else NULL
  data <- df_prep_blq(data, loq, loq_method, pred_vars = pred_vars)

  ## One LLOQ line: with several assays, the highest LLOQ (the most censoring).
  loq_vals <- if ("LOQ" %in% colnames(data)) data$LOQ[!is.na(data$LOQ)] else numeric(0)
  lloq <- if (length(loq_vals) > 0) max(loq_vals) else NA_real_

  if (isTRUE(dosenorm)) {
    data$DV <- var_dosenorm(data$DV, data$DOSE)
    if (!is.null(pred_var_str)) data$PRED <- var_dosenorm(data$PRED, data$DOSE)
    if (!is.null(ipred_var_str)) data$IPRED <- var_dosenorm(data$IPRED, data$DOSE)
  }

  list(data = data, lloq = lloq, loq_method = loq_method)
}


check_loq_method <- function(loq, loq_method, data) {
  aliases <- c(none = 0, postdose = 1, all = 2)
  if (is.character(loq_method) && loq_method %in% names(aliases)) {
    loq_method <- aliases[[loq_method]]
  }
  if(is.null(loq_method) || !loq_method %in% c(0, 1, 2)) {
    rlang::abort(message = 'argument `loq_method` must be 0, 1, 2, "none", "postdose", or "all"')
  }

  if(loq_method != 0) {
    if(is.null(loq)) {
      if(!"LLOQ" %in% colnames(data)) {
        rlang::abort(message = "argument `loq` must be numeric or variable `LLOQ` must be present in `data` when `loq_method` is 1 or 2")
      }
    } else {
      num <- suppressWarnings(as.numeric(loq))
      if(is.na(num) || !is.numeric(num)) {
        rlang::abort(message = "argument `loq` must be numeric or variable `LLOQ` must be present in `data` when `loq_method` is 1 or 2")
      }
    }
  } else if (!is.null(loq)) {
    rlang::inform(paste0(
      "`loq` has no effect with `loq_method = 0`; set `loq_method` to 1 or 2 ",
      "to impute BLQ values and draw the LLOQ line."
    ))
  }
  loq_method
}


#' Internal: shared setup for `plot_dvtime()` and `plot_gof()`
#'
#' Warns when `dose_var` is given without `dosenorm`, prepares the data with
#' [df_prep_dvtime()], resolves the style (family preset default, `log_y`
#' falling back to the style's `logy`, error bar cap width defaulted from the
#' data), and builds the central tendency caption line.
#'
#' @param data Input data.
#' @param var_strs Named list of resolved column names: `time`, `ntime`, `dv`,
#'    `dose`, and optionally `pred`, `ipred`, `id`, `col`.
#' @param dose_var_given Logical; whether the caller passed `dose_var`.
#' @param dosenorm,loq,loq_method,ref,cent,log_y,style As in the caller.
#' @param blq_mode As in [plot_gof()]; `"obs"` for [plot_dvtime()].
#' @param style_default The family preset function, e.g. [style_dvtime()].
#' @return `list(data, lloq, loq_method, style, log_y, caption)`; `caption` is
#'    a character vector of lines.
#' @noRd
prep_dvtime_plot <- function(data, var_strs, dose_var_given, dosenorm, loq,
                             loq_method, ref, blq_mode = "obs", style,
                             style_default, log_y, cent) {
  if (dose_var_given && !isTRUE(dosenorm)) {
    warning("`dose_var` is ignored when `dosenorm = FALSE`", call. = FALSE)
  }
  dose_var_str <- if (isTRUE(dosenorm)) var_strs$dose

  prep <- df_prep_dvtime(
    data, var_strs$time, var_strs$ntime,
    dv_var_str = var_strs$dv,
    pred_var_str = var_strs$pred,
    ipred_var_str = var_strs$ipred,
    loq = loq, loq_method = loq_method,
    dose_var_str = dose_var_str,
    col_var_str = var_strs$col,
    id_var_str = var_strs$id,
    dosenorm = dosenorm,
    ref = ref,
    blq_mode = blq_mode
  )

  # Resolve style: preset default, with log_y (falling back to the style's
  # logy) driving the y axis and caption, and the error bar cap width defaulted
  # from the data when the style leaves it unset.
  plotstyle <- resolve_style(style, style_default)
  log_y <- log_y %||% isTRUE(plotstyle$logy)
  check_boolean(log_y, "log_y")
  plotstyle <- ggstylekit::set_style(plotstyle, logy = log_y)
  plotstyle <- style_errorbar_width(plotstyle, prep$data)

  list(data = prep$data, lloq = prep$lloq, loq_method = prep$loq_method,
       style = plotstyle, log_y = log_y, caption = caption_dvtime(cent, log_y))
}
