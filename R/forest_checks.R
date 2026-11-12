check_ref <- function(ref, call = rlang::caller_env()) {
  if (is.null(ref) || identical(ref, "reference")) return(invisible(NULL))
  if (!is.numeric(ref) || length(ref) != 1L || is.na(ref)) {
    pmx_abort("{.arg ref} must be a single numeric value, {.val reference}, or {.code NULL}.",
              "bad_arg", call = call)
  }
  invisible(NULL)
}


check_ref_layout <- function(ref_layout, call = rlang::caller_env()) {
  if (is.null(ref_layout)) return(invisible(NULL))
  if (!is.character(ref_layout) || length(ref_layout) != 1L ||
      !ref_layout %in% c("distributed", "consolidated")) {
    pmx_abort("{.arg ref_layout} must be {.val distributed}, {.val consolidated}, or {.code NULL}.",
              "bad_arg", call = call)
  }
  invisible(NULL)
}


check_ref_band <- function(ref_band, ref_band_type = "absolute", call = rlang::caller_env()) {
  if (is.null(ref_band)) return(invisible(NULL))
  if (!is.numeric(ref_band) || length(ref_band) != 2L || any(is.na(ref_band))) {
    pmx_abort("{.arg ref_band} must be a length-2 numeric vector with no missing values.",
              "bad_arg", call = call)
  }
  if (ref_band[1] >= ref_band[2]) {
    pmx_abort("{.arg ref_band} must be ordered: ref_band[1] < ref_band[2].", "bad_arg",
              call = call)
  }
  if (ref_band_type == "multiplicative" && any(ref_band <= 0)) {
    pmx_abort("{.arg ref_band} must be positive when {.code ref_band_type = \"multiplicative\"}.",
              "bad_arg", call = call)
  }
  invisible(NULL)
}


check_forest_metric <- function(metric, metric_values, metric_name_var_str,
                                call = rlang::caller_env()) {
  if (is.null(metric)) {
    if (length(metric_values) != 1L) {
      pmx_abort(c(
        "{.arg metric} is required when {.code stats$stats} contains multiple values of {.var {metric_name_var_str}}: {.val {metric_values}}.",
        "i" = "Pass {.code metric = <one of the listed values>} to pick which to render."
      ), "bad_arg", call = call)
    }
    return(metric_values)
  }
  check_string(metric, "metric", call = call)
  if (!metric %in% metric_values) {
    pmx_abort(c(
      "{.arg metric} ({.val {metric}}) not found in {.var {metric_name_var_str}}.",
      "i" = "Available: {.val {metric_values}}."
    ), "bad_arg", call = call)
  }
  metric
}


check_character_vars <- function(x, name, call = rlang::caller_env()) {
  if (!is.character(x) || length(x) == 0L) {
    pmx_abort("{.arg {name}} must be a non-empty character vector of column names.",
              "bad_arg", call = call)
  }
  if (any(is.na(x)) || any(!nzchar(x))) {
    pmx_abort("{.arg {name}} must not contain {.code NA} or empty strings.", "bad_arg",
              call = call)
  }
  if (anyDuplicated(x)) {
    dups <- unique(x[duplicated(x)])
    pmx_abort("{.arg {name}} has duplicate entries: {.val {dups}}.", "bad_arg", call = call)
  }
  invisible(NULL)
}


check_cov_val_ref <- function(cov_val_ref, cov_vars, call = rlang::caller_env()) {
  check_named_vector(cov_val_ref, "cov_val_ref",
                     allowed = cov_vars, allowed_name = "cov_vars", call = call)
  missing_cov <- setdiff(cov_vars, names(cov_val_ref))
  if (length(missing_cov) > 0) {
    pmx_abort(c(
      "{.arg cov_val_ref} is missing a reference value for {.var {missing_cov}}.",
      "i" = "One entry is required per {.arg cov_vars} column."
    ), "bad_arg", call = call)
  }
  invisible(NULL)
}


check_cov_labels <- function(cov_labels, cov_vars, call = rlang::caller_env()) {
  if (is.null(cov_labels)) return(invisible(NULL))
  if (!is.list(cov_labels)) {
    pmx_abort("{.arg cov_labels} must be a named list (or {.code NULL}).", "bad_arg",
              call = call)
  }
  check_names(cov_labels, "cov_labels", what = "named list",
              allowed = cov_vars, allowed_name = "cov_vars", call = call)
  for (cn in names(cov_labels)) {
    lab <- cov_labels[[cn]]
    if (is.function(lab)) next
    if (is.character(lab) && !is.null(names(lab)) &&
        !any(is.na(names(lab))) && all(nzchar(names(lab)))) {
      next
    }
    pmx_abort("Element {.var {cn}} of {.arg cov_labels} must be a function or a named character vector keyed on the covariate values.",
              "bad_arg", call = call)
  }
  invisible(NULL)
}
