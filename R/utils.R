resolve_var <- function(quo, nullable = FALSE) {
  if (rlang::quo_is_null(quo)) return(NULL)
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

init_time_vars <- function(time_vars) {
  list_update(time_vars, c(TIME = "TIME", NTIME = "NTIME"))
}

rename_time_vars <- function(data, time_vars, other_vars = NULL) {
  if(length(unique(c(time_vars[[1]], time_vars[[2]]))) == 2) {
    data <- dplyr::rename(data, dplyr::any_of(c(time_vars, other_vars)))
  } else {
    data <- data |>
      dplyr::rename(dplyr::any_of(c(c(NTIME = time_vars[["NTIME"]]), other_vars))) |>
      dplyr::mutate(TIME = NTIME)
  }
  return(data)
}

apply_blq <- function(data, loq, loq_method, extra_vars = NULL) {
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

prep_dvtime_data <- function(data, time_vars, output_vars = c(DV = "DV"),
                            timeu = "hours", loq = NULL, loq_method = 0,
                            dose_var_str = NULL, col_var_str = NULL,
                            grp_dv = FALSE, grp_var_str = NULL,
                            dosenorm = FALSE,
                            cfb = FALSE, cfb_base = NULL) {

  time_vars <- init_time_vars(time_vars)

  check_df(data, "data")
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

  data <- rename_time_vars(data, time_vars, output_vars)

  if (!is.null(dose_var_str) && dose_var_str != "DOSE") {
    data <- dplyr::rename(data, dplyr::any_of(c(DOSE = dose_var_str)))
  }

  if (!is.null(col_var_str)) {
    data[[col_var_str]] <- factor(data[[col_var_str]])
  }

  extra_vars <- setdiff(names(output_vars), "DV")
  data <- apply_blq(data, loq, loq_method, extra_vars = extra_vars)

  lloq <- if ("LOQ" %in% colnames(data)) unique(data$LOQ[!is.na(data$LOQ)]) else NA_real_

  if (isTRUE(dosenorm)) {
    for (v in names(output_vars)) {
      data[[v]] <- var_dosenorm(data[[v]], data$DOSE)
    }
  }

  list(data = data, lloq = lloq)
}


prep_plot_env <- function(data, cent, log_y, obs_dv, grp_dv,
                          timeu, n_breaks, theme, theme_fn) {
  caption  <- dvtime_caption(cent, log_y, obs_dv, grp_dv)
  xbreaks  <- var_timebreaks(x = sort(unique(data$NTIME)), unit = timeu, n = n_breaks)
  plottheme <- list_update(theme, theme_fn())
  width    <- errorbar_width(plottheme, data)
  list(caption = caption, xbreaks = xbreaks, plottheme = plottheme, width = width)
}


errorbar_width <- function(plottheme, data) {
  if(is.numeric(plottheme$width_errorbar)) plottheme$width_errorbar
  else max(data$NTIME, na.rm = TRUE) * 0.025
}

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

