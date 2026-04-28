#' Internal helper: Resolve function input conflicts to standard internal string constants
#'
#' @param quo Function argument as a quosure (wrapped in rlang::ensym)
#' @param nullable Logical argument specifying if the argument may be `NULL`.
#'    Default is `FALSE`.
#'
#' @return A resolved string constant variable
#' @keywords internal
#' @examples
#' f <- function(x = DV) pmxhelpr:::resolve_var(rlang::enquo(x))
#' f()
#' f("ODV")
#' f(CONC)
#'
resolve_var <- function(quo, nullable = FALSE) {
  expr <- rlang::quo_get_expr(quo)
  if (is.character(expr)) return(expr)
  val <- tryCatch(
    list(value = rlang::eval_tidy(quo), ok = TRUE),
    error = function(e) list(value = NULL, ok = FALSE)
  )
  if (val$ok) {
    if (is.null(val$value) && nullable) return(NULL)
    if (is.character(val$value)) return(val$value)
  }
  if (is.symbol(expr)) return(as.character(expr))
  rlang::abort("Column specification must be a name or string")
}




#' Internal helper: Standardize time variable names in a data.frame
#'
#' Merges user-supplied `time_vars` mapping with defaults, then renames
#' columns in `data` to the standardized `TIME` and `NTIME` names.
#' When both time variables map to the same source column, `NTIME` is
#' renamed and `TIME` is created as a copy.
#'
#' @param data Input data.frame.
#' @param time_vars Named character vector mapping internal time variable names
#'    to column names in `data`. Default is `c(TIME = "TIME", NTIME = "NTIME")`.
#' @param other_vars Other named variables to include in renaming.
#'
#' @return A data.frame with standardized `TIME` and `NTIME` columns
#' @keywords internal
#' @examples
#' data <- dplyr::rename(data_sad, NTFD = NTIME)
#' c("TIME", "NTIME") %in% colnames(data)
#' data_std <- pmxhelpr:::df_prep_timevars(data, c(TIME = "TIME", NTIME = "NTFD"))
#' c("TIME", "NTIME") %in% colnames(data_std)
#'
df_prep_timevars <- function(data, time_vars, other_vars = NULL) {
  time_vars <- list_update(time_vars, c(TIME = "TIME", NTIME = "NTIME"))
  if(length(unique(c(time_vars[[1]], time_vars[[2]]))) == 2) {
    data <- dplyr::rename(data, dplyr::any_of(c(time_vars, other_vars)))
  } else {
    data <- data |>
      dplyr::rename(dplyr::any_of(c(c(NTIME = time_vars[["NTIME"]]), other_vars))) |>
      dplyr::mutate(TIME = NTIME)
  }
  return(data)
}




#' Internal helper: Apply BLQ censoring rules to data
#'
#' @param data data.frame containing data to censor
#' @param extra_vars Other variables to include in censoring.
#' @inheritParams plot_dvtime
#'
#' @return A data.frame with BLQ censoring applied
#' @keywords internal
#' @examples
#' data <- dplyr::rename(dplyr::filter(data_sad, CMT %in% c(1,2)), DV = ODV)
#' data_loq1 <- pmxhelpr:::df_prep_blq(data, loq = 10, loq_method = 1)
#' data_loq2 <- pmxhelpr:::df_prep_blq(data, loq = 10, loq_method = 2)
#'
df_prep_blq <- function(data, loq, loq_method, extra_vars = NULL) {
  if(!loq_method %in% c(1, 2)) return(data)

  data <- data |>
    dplyr::mutate(LOQ = ifelse(is.null(loq), LLOQ, loq))

  if(loq_method == 1) {
    data <- data |>
      dplyr::mutate(DV = dplyr::case_when(EVID != 0 ~ NA_real_,
                                           MDV == 0 ~ DV,
                                           TIME <= 0 ~ 0,
                                           TIME > 0 ~ 0.5 * LOQ))
    for(v in extra_vars) {
      data <- data |>
        dplyr::mutate("{v}" := dplyr::case_when(
          EVID != 0 ~ NA_real_,
          TIME <= 0 ~ 0,
          .data[[v]] >= LOQ ~ .data[[v]],
          .data[[v]] < LOQ ~ 0.5 * LOQ))
    }
  }

  if(loq_method == 2) {
    data <- data |>
      dplyr::mutate(DV = dplyr::case_when(EVID != 0 ~ NA_real_,
                                           MDV == 0 ~ DV,
                                           MDV == 1 ~ 0.5 * LOQ))
    for(v in extra_vars) {
      data <- data |>
        dplyr::mutate("{v}" := dplyr::case_when(
          EVID != 0 ~ NA_real_,
          .data[[v]] >= LOQ ~ .data[[v]],
          .data[[v]] < LOQ ~ 0.5 * LOQ))
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
#' @param time_vars Named character vector mapping internal time variable names
#'    to column names in `data`. Default is `c(TIME = "TIME", NTIME = "NTIME")`.
#' @param output_vars Named character vector mapping internal output variable names
#'    to column names in `data`. Default is `c(DV = "DV")`.
#' @param timeu Character string specifying time units. Passed to validation.
#' @param loq Numeric value of LLOQ, or `NULL`.
#' @param loq_method Integer (0, 1, or 2) specifying BLQ handling method.
#' @param dose_var_str String specifying the dose column name, or `NULL`.
#' @param col_var_str String specifying the color variable column name, or `NULL`.
#' @param grp_dv Logical indicating if group variable should be validated.
#' @param grp_var_str String specifying the group column name, or `NULL`.
#' @param dosenorm Logical indicating if dose normalization should be applied.
#' @param cfb Logical indicating if data is change from baseline.
#' @param cfb_base Numeric baseline value when `cfb = TRUE`.
#'
#' @return A named list with elements `data` (processed data.frame) and `lloq` (numeric LLOQ value).
#' @keywords internal
#' @examples
#' data <- dplyr::rename(dplyr::filter(data_sad, CMT %in% c(1,2)), DV = ODV)
#' prep <- pmxhelpr:::df_prep_dvtime(data, time_vars = c(TIME = "TIME", NTIME = "NTIME"))
#' head(prep$data)
#'
df_prep_dvtime <- function(data, time_vars, output_vars = c(DV = "DV"),
                            timeu = "hours", loq = NULL, loq_method = 0,
                            dose_var_str = NULL, col_var_str = NULL,
                            grp_dv = FALSE, grp_var_str = NULL,
                            dosenorm = FALSE,
                            cfb = FALSE, cfb_base = NULL) {

  check_df(data, "data")
  time_vars <- list_update(time_vars, c(TIME = "TIME", NTIME = "NTIME"))
  check_varsindf(data, time_vars[["TIME"]], "data", "time_vars")
  check_varsindf(data, time_vars[["NTIME"]], "data", "time_vars")
  for (i in seq_along(output_vars)) {
    check_varsindf(data, output_vars[[i]], "data", names(output_vars)[[i]])
  }
  check_varsindf(data, "MDV", "data", "MDV")
  check_timeu(timeu)
  if (!is.null(col_var_str)) {
    check_varsindf(data, col_var_str, "data", "col_var")
    check_factor(data, col_var_str, "col_var")
  }
  if (isTRUE(grp_dv)) check_varsindf(data, grp_var_str, "data", "grp_var")
  if (isTRUE(dosenorm)) check_varsindf(data, dose_var_str, "data", "dose_var")
  check_loq_method(loq, loq_method, data)
  if (isTRUE(cfb)) check_numeric(cfb_base, "cfb_base")

  data <- df_prep_timevars(data, time_vars, output_vars)

  if (!is.null(dose_var_str) && dose_var_str != "DOSE") {
    data <- dplyr::rename(data, dplyr::any_of(c(DOSE = dose_var_str)))
  }

  if (!is.null(col_var_str)) {
    data[[col_var_str]] <- factor(data[[col_var_str]])
  }

  extra_vars <- setdiff(names(output_vars), "DV")
  data <- df_prep_blq(data, loq, loq_method, extra_vars = extra_vars)

  lloq <- if ("LOQ" %in% colnames(data)) unique(data$LOQ[!is.na(data$LOQ)]) else NA_real_

  if (isTRUE(dosenorm)) {
    for (v in names(output_vars)) {
      data[[v]] <- var_dosenorm(data[[v]], data$DOSE)
    }
  }

  list(data = data, lloq = lloq)
}


#' Internal helper: Update elements of a list by name
#'
#' @param update Named list of values to update
#'    Default is `NULL`
#' @param src Named list of source values
#'
#' @return A named list
#' @keywords internal
#' @examples
#' src_list <- c(a = 1, b = 2, c = 3, d = 4)
#' new_list <- pmxhelpr:::list_update(c(a = 5), src_list)
#'
list_update <- function(update=NULL, src){

  update_list_name <-deparse(substitute(update))

  out <- src
  if(!is.null(update) & length(names(update))>0){
    for(i in seq(names(update))) {
      current_name <- names(update)[i]
      if(current_name %in% names(out)) {
        out[[current_name]] <- update[[current_name]]
      } else {
        warning(paste0("`", current_name,"` is not a valid element of ", update_list_name))
      }
    }
  }
  return(out)
}


#' Internal Helper: Apply dose-normalization to a variable
#'
#' @param dv_var Vector containing the dependent variable (DV)
#' @param dose_var Vector containing dose
#'
#' @return A numeric vector of dose-normalized values of `dv_var`
#' @keywords internal
#' @examples
#' data <- dplyr::mutate(data_sad, DNDV = pmxhelpr:::var_dosenorm(ODV, DOSE))

var_dosenorm <- function(dv_var, dose_var) {
  dv_var / dose_var
}


#' Internal Helper: Apply prediction correction
#'
#' @param dv_var Vector containing the dependent variable (DV)
#' @param pred_var Vector containing population predictions (PRED)
#' @param lower_bound Lower bound for prediction correction formula.
#'
#' @return A numeric vector of prediction-corrected values of `dv_var`
#' @keywords internal
#' @examples
#' pkmodel <- model_mread_load(model = "pkmodel")
#' data <- df_addpred(data = dplyr::filter(data_sad, CMT != 3), model = pkmodel)
#' data <- dplyr::mutate(data, PCDV = pmxhelpr:::var_pc(ODV, PRED))
#'
var_pc <- function(dv_var, pred_var, lower_bound = 0) {
  predbin <- stats::median(pred_var)
  lower_bound + (dv_var - lower_bound) * ((predbin - lower_bound) / (pred_var - lower_bound))
}


#' Internal Helper: Determine left-censoring for quantiles at the lower limit of quantification
#'
#' @param x Vector containing the variable to be censored
#' @param p Quantile for computation
#' @param loq Numeric value of the lower limit of quantification (LLOQ) for the assay
#'
#' @return Replaces values below loq (including NA) with -Inf, then computes
#   the quantile. Returns NA if the result is -Inf.
#' @keywords internal
#' @examples
#' data <- data_sad |>
#'   dplyr::group_by(CMT, NTIME, DOSE) |>
#'   dplyr::summarize(P05 = pmxhelpr:::var_loqcens(ODV, 0.05, loq = LLOQ),
#'                    P50 = pmxhelpr:::var_loqcens(ODV, 0.5, loq = LLOQ),
#'                    P95 = pmxhelpr:::var_loqcens(ODV, 0.05, loq = LLOQ), .groups = "drop")

var_loqcens <- function(x, p, loq) {
  x[is.na(x)] <- -Inf
  x[x < loq] <- -Inf
  q <- stats::quantile(x, probs = p, na.rm = TRUE)
  if (is.infinite(q) && q < 0) NA_real_ else as.numeric(q)
}




#' Append counts of unique identifiers to group labels
#'
#' @description `var_addn()` counts distinct values of `id_var` within each
#'    level of `grp_var` and returns a factor with labels like `"100 mg (n=6)"`.
#'
#' @param grp_var Vector of grouping variable values.
#' @param id_var Vector of identifier values to count distinct entries of.
#' @param sep Optional separator to add between values of `grp_var` and appended counts.
#'    Default is NULL.
#'
#' @return A factor vector with group labels appended with subject counts (e.g., `"100 mg (n=6)"`)
#' @export var_addn
#' @examples
#' data <- dplyr::filter(data_sad, CMT != 3)
#' var_addn(data$DOSE, data$ID, sep = "mg")
#'
var_addn <- function(grp_var, id_var, sep = NULL) {
  counts <- tapply(id_var, grp_var, dplyr::n_distinct)
  n <- unname(counts[as.character(grp_var)])
  parts <- if (is.null(sep)) paste(grp_var) else paste(grp_var, sep)
  factor(paste(parts, paste0("(n=", n, ")")))
}
