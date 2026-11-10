#' Internal Helper: Apply dose-normalization to a variable
#'
#' @param dv_var Vector containing the dependent variable (DV)
#' @param dose_var Vector containing dose
#'
#' @family vectorized helpers
#' @return A numeric vector of dose-normalized values of `dv_var`
#' @export var_dosenorm
#' @examples
#' data <- dplyr::mutate(data_sad, DNDV = var_dosenorm(ODV, DOSE))

var_dosenorm <- function(dv_var,
                         dose_var) {
  if (!is.numeric(dv_var)) pmx_abort("{.arg dv_var} must be {.cls numeric}.", "bad_arg")
  if (!is.numeric(dose_var)) pmx_abort("{.arg dose_var} must be {.cls numeric}.", "bad_arg")
  if (length(dv_var) != length(dose_var)) {
    pmx_abort("{.arg dv_var} and {.arg dose_var} must have the same length.", "bad_arg")
  }
  zero_doses <- !is.na(dose_var) & dose_var == 0
  if (any(zero_doses)) {
    n_zero <- sum(zero_doses)
    pmx_warn("{.arg dose_var} contains {n_zero} zero value{?s}; returning NA for {?that position/those positions}.",
             "data")
    dose_var[zero_doses] <- NA_real_
  }
  dv_var / dose_var
}


#' Apply prediction correction
#'
#' @param dv_var Vector containing the dependent variable (DV)
#' @param pred_var Vector containing population predictions (PRED)
#' @param lower_bound Lower bound for prediction correction formula.
#'
#' @details The bin-median `predbin` is computed over the full `pred_var`
#'    vector passed in. Callers must group their data by a binning variable
#'    (e.g. nominal time, optionally stratified by compartment or covariates)
#'    before invoking `var_predcorr()` so that `predbin` is the median across
#'    the bin's observations, not the entire dataset.
#'
#' @family vectorized helpers
#' @return A numeric vector of prediction-corrected values of `dv_var`
#' @export var_predcorr
#' @examples
#' pkmodel <- model_mread_load(model = "pkmodel")
#' data <- df_mrgsim_addpred(
#'   data = dplyr::filter(data_sad, CMT != 3),
#'   model = pkmodel)
#' data <- data |>
#'   dplyr::filter(EVID == 0) |>
#'   dplyr::group_by(NTIME) |>
#'   dplyr::mutate(PCDV = var_predcorr(ODV, PRED)) |>
#'   dplyr::ungroup()
#'
var_predcorr <- function(dv_var,
                         pred_var,
                         lower_bound = 0) {
  if (!is.numeric(dv_var)) pmx_abort("{.arg dv_var} must be {.cls numeric}.", "bad_arg")
  if (!is.numeric(pred_var)) pmx_abort("{.arg pred_var} must be {.cls numeric}.", "bad_arg")
  if (length(dv_var) != length(pred_var)) {
    pmx_abort("{.arg dv_var} and {.arg pred_var} must have the same length.", "bad_arg")
  }
  if (all(is.na(pred_var))) {
    pmx_warn("{.arg pred_var} is all NA; returning NA for prediction-corrected values.", "data")
    return(rep(NA_real_, length(dv_var)))
  }
  predbin <- stats::median(pred_var, na.rm = TRUE)
  denom <- pred_var - lower_bound
  denom[denom == 0] <- NA_real_
  lower_bound + (dv_var - lower_bound) * ((predbin - lower_bound) / denom)
}


#' Append counts of unique identifiers to group labels
#'
#' @description `var_addn()` counts distinct values of `id_var` within each
#'    level of `grp_var` and returns a factor with labels like `"100 mg (n=6)"`.
#'    Factor levels follow the order in which values first appear in
#'    `grp_var`, so a pre-sorted input like `c(10, 200, 1000)` yields levels
#'    in numeric order rather than the alphabetic default of `factor()`.
#'
#' @param grp_var Vector of grouping variable values.
#' @param id_var Vector of identifier values to count distinct entries of.
#' @param sep Optional separator to add between values of `grp_var` and appended counts.
#'    Default is NULL.
#'
#' @family vectorized helpers
#' @return A factor vector with group labels appended with subject counts (e.g., `"100 mg (n=6)"`)
#' @export var_addn
#' @examples
#' data <- dplyr::filter(data_sad, CMT != 3)
#' var_addn(data$DOSE, data$ID, sep = "mg")
#'
var_addn <- function(grp_var,
                     id_var,
                     sep = NULL) {
  if (length(grp_var) != length(id_var)) {
    pmx_abort("{.arg grp_var} and {.arg id_var} must have the same length.", "bad_arg")
  }
  counts <- tapply(id_var, grp_var, dplyr::n_distinct)
  n <- unname(counts[as.character(grp_var)])
  parts <- if (is.null(sep)) paste(grp_var) else paste(grp_var, sep)
  labels <- paste(parts, paste0("(n=", n, ")"))
  ordered_grp <- unique(grp_var)
  ordered_parts <- if (is.null(sep)) paste(ordered_grp) else paste(ordered_grp, sep)
  ordered_counts <- unname(counts[as.character(ordered_grp)])
  ordered_levels <- paste(ordered_parts, paste0("(n=", ordered_counts, ")"))
  factor(labels, levels = ordered_levels)
}


#' Internal Helper: Encode below-the-limit-of-quantification (BLQ) values as -Inf
#'
#' @description
#' Vectorized helper used in VPC pre-processing. Replaces BLQ-flagged positions
#' of `x` with `-Inf`. A position is BLQ when any of the following hold:
#' `is.na(x)`, `!is.null(loq) && x < loq`, or `!is.null(mdv) && mdv == 1`.
#' Returns the modified vector — quantile computation is performed downstream.
#'
#' @param x Numeric vector to encode.
#' @param loq Numeric scalar or vector of length `length(x)` giving the lower
#'    limit of quantification, or `NULL` (no `< loq` trigger).
#' @param mdv Integer/logical vector of length `length(x)` flagging missing-DV
#'    rows (NONMEM convention `MDV == 1`), or `NULL` (no MDV trigger).
#' @param call The call reported in errors.
#'
#' @return Numeric vector the same length as `x`, with BLQ positions set to `-Inf`.
#' @keywords internal
#' @examples
#' pmxhelpr:::var_loqcens(c(1, 2, 5, NA, 10), loq = 3, mdv = c(0, 0, 0, 0, 1))
#' pmxhelpr:::var_loqcens(c(1, 2, 5), loq = NULL, mdv = c(0, 1, 0))

var_loqcens <- function(x, loq = NULL, mdv = NULL, call = rlang::caller_env()) {
  if (!is.numeric(x)) pmx_abort("{.arg x} must be {.cls numeric}.", "bad_arg", call = call)
  if (!is.null(loq)) {
    if (!is.numeric(loq)) pmx_abort("{.arg loq} must be {.cls numeric}.", "bad_arg", call = call)
    if (length(loq) != 1L && length(loq) != length(x)) {
      pmx_abort("{.arg loq} must be length 1 or the same length as {.arg x}.", "bad_arg",
                call = call)
    }
  }
  if (!is.null(mdv) && length(mdv) != length(x)) {
    pmx_abort("{.arg mdv} must be the same length as {.arg x}.", "bad_arg", call = call)
  }
  blq <- is.na(x)
  if (!is.null(loq)) blq <- blq | (x < loq)
  if (!is.null(mdv)) blq <- blq | (mdv == 1L)
  blq[is.na(blq)] <- FALSE
  x[blq] <- -Inf
  x
}


#' Internal Helper: Replace -Inf with NA_real_
#'
#' @description
#' Vectorized helper that converts `-Inf` entries (and only `-Inf`, not `+Inf`)
#' to `NA_real_`. Used to clean BLQ-encoded values in VPC summary statistics
#' and observation frames before plotting.
#'
#' @param x Numeric vector.
#' @return Numeric vector the same length as `x`, with `-Inf` replaced by `NA_real_`.
#' @keywords internal
#' @examples
#' pmxhelpr:::var_infna(c(1, -Inf, 3, NA))

var_infna <- function(x) {
  if (!is.numeric(x)) pmx_abort("{.arg x} must be {.cls numeric}.", "bad_arg")
  x[is.infinite(x) & x < 0] <- NA_real_
  x
}
