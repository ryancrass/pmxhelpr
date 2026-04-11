capture_col <- function(col_quo) {
  if (rlang::quo_is_null(col_quo)) return(NULL)
  rlang::as_name(col_quo)
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
        dplyr::mutate(!!rlang::sym(v) := dplyr::case_when(
          EVID != 0 ~ NA_real_,
          TIME <= 0 ~ 0,
          !!rlang::sym(v) >= LOQ ~ !!rlang::sym(v),
          !!rlang::sym(v) < LOQ ~ 0.5 * LOQ))
    }
  }

  if(loq_method == 2) {
    data <- data |>
      dplyr::mutate(DV = dplyr::case_when(EVID != 0 ~ NA_real_,
                                           MDV == 0 ~ DV,
                                           MDV == 1 ~ 0.5 * LOQ))
    for(v in extra_vars) {
      data <- data |>
        dplyr::mutate(!!rlang::sym(v) := dplyr::case_when(
          EVID != 0 ~ NA_real_,
          !!rlang::sym(v) >= LOQ ~ !!rlang::sym(v),
          !!rlang::sym(v) < LOQ ~ 0.5 * LOQ))
    }
  }

  return(data)
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

