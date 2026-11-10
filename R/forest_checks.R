check_ref <- function(ref) {
  if (is.null(ref) || identical(ref, "reference")) return(invisible(NULL))
  if (!is.numeric(ref) || length(ref) != 1L || is.na(ref)) {
    rlang::abort(message = "argument `ref` must be a single numeric value, `\"reference\"`, or `NULL`")
  }
  invisible(NULL)
}


check_ref_layout <- function(ref_layout) {
  if (is.null(ref_layout)) return(invisible(NULL))
  if (!is.character(ref_layout) || length(ref_layout) != 1L ||
      !ref_layout %in% c("distributed", "consolidated")) {
    rlang::abort(message = "argument `ref_layout` must be `\"distributed\"`, `\"consolidated\"`, or `NULL`")
  }
  invisible(NULL)
}


check_ref_band <- function(ref_band, ref_band_type = "absolute") {
  if (is.null(ref_band)) return(invisible(NULL))
  if (!is.numeric(ref_band) || length(ref_band) != 2L || any(is.na(ref_band))) {
    rlang::abort(message = "argument `ref_band` must be a length-2 numeric vector with no NAs")
  }
  if (ref_band[1] >= ref_band[2]) {
    rlang::abort(message = "argument `ref_band` must be ordered: ref_band[1] < ref_band[2]")
  }
  if (ref_band_type == "multiplicative" && any(ref_band <= 0)) {
    rlang::abort(message = "argument `ref_band` must be positive when `ref_band_type = \"multiplicative\"`")
  }
  invisible(NULL)
}


check_forest_metric <- function(metric, metric_values, metric_name_var_str) {
  if (is.null(metric)) {
    if (length(metric_values) != 1L) {
      rlang::abort(message = paste0(
        "argument `metric` is required when `stats$stats` contains multiple values of `",
        metric_name_var_str, "` (", paste(metric_values, collapse = ", "), "). ",
        "Pass `metric = <one of the listed values>` to pick which to render."
      ))
    }
    return(metric_values)
  }
  check_string(metric, "metric")
  if (!metric %in% metric_values) {
    rlang::abort(message = paste0(
      "argument `metric` (", metric, ") not found in `", metric_name_var_str,
      "`. Available: ", paste(metric_values, collapse = ", ")
    ))
  }
  metric
}


check_character_vars <- function(x, name) {
  if (!is.character(x) || length(x) == 0L) {
    rlang::abort(message = paste0(
      "argument `", name, "` must be a non-empty character vector of column names"
    ))
  }
  if (any(is.na(x)) || any(!nzchar(x))) {
    rlang::abort(message = paste0(
      "argument `", name, "` must not contain `NA` or empty strings"
    ))
  }
  if (anyDuplicated(x)) {
    rlang::abort(message = paste0(
      "argument `", name, "` has duplicate entries: ",
      paste(unique(x[duplicated(x)]), collapse = ", ")
    ))
  }
  invisible(NULL)
}


check_cov_val_ref <- function(cov_val_ref, cov_vars) {
  check_named_vector(cov_val_ref, "cov_val_ref",
                     allowed = cov_vars, allowed_name = "cov_vars")
  missing_cov <- setdiff(cov_vars, names(cov_val_ref))
  if (length(missing_cov) > 0) {
    rlang::abort(message = paste0(
      "argument `cov_val_ref` is missing a reference value for: ",
      paste(missing_cov, collapse = ", "),
      ". One entry is required per `cov_vars` column."
    ))
  }
  invisible(NULL)
}


check_cov_labels <- function(cov_labels, cov_vars) {
  if (is.null(cov_labels)) return(invisible(NULL))
  if (!is.list(cov_labels)) {
    rlang::abort(message = "argument `cov_labels` must be a named list (or `NULL`)")
  }
  check_names(cov_labels, "cov_labels", what = "named list",
              allowed = cov_vars, allowed_name = "cov_vars")
  for (cn in names(cov_labels)) {
    lab <- cov_labels[[cn]]
    if (is.function(lab)) next
    if (is.character(lab) && !is.null(names(lab)) &&
        !any(is.na(names(lab))) && all(nzchar(names(lab)))) {
      next
    }
    rlang::abort(message = paste0(
      "element `", cn, "` of argument `cov_labels` must be a function or a ",
      "named character vector keyed on the covariate values"
    ))
  }
  invisible(NULL)
}
