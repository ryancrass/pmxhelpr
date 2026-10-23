#' Reshape Replicate Simulation Output for Covariate Forest Plots
#'
#' @description
#' Turns raw replicate simulation output into the long draws frame
#' [df_forest()] consumes. Input is one row per covariate scenario per
#' replicate with the exposure metrics as columns (an N replicates by M
#' scenarios table, the shape an `mrgsolve` forest simulation produces once
#' metrics are derived over the interval of interest). Output is one row per
#' metric per scenario per replicate, with the covariate name, the covariate
#' level label, and a logical reference flag.
#'
#' Scenarios are read from the covariate columns themselves: the reference
#' scenario is the one where every covariate equals its `cov_val_ref`
#' value, and every other scenario must differ from it in exactly one
#' covariate, which becomes that scenario's covariate name. No scenario
#' identifier column is needed.
#'
#' @details
#' Covariate values are matched to `cov_val_ref` on their character
#' representation, so a numeric column and a character reference value still
#' match. Covariate columns may not contain `NA`.
#'
#' With `ratio = TRUE` each metric is also divided by the same replicate's
#' reference-scenario value, producing a within-replicate test/reference
#' ratio named `<metric><ratio_suffix>`. Because the parameter draw is shared
#' across scenarios within a replicate, uncertainty common to both arms
#' cancels in the ratio, which is why ratio intervals are narrower than
#' absolute-exposure intervals.
#'
#' Reference rows carry `cov_name_ref` as both their covariate name and their
#' covariate level, so [df_forest()] renders them as a standalone reference
#' panel by default. To place the reference inside each covariate panel
#' instead, pass the per-covariate reference labels to [df_forest()] as
#' `cov_level_ref`; the mapping is returned on the `"cov_level_ref"`
#' attribute of the output and is not read automatically, since that would
#' change the default rendering from invisible metadata.
#'
#' @param data Input data.frame. One row per covariate scenario per
#'    replicate, with exposure metrics as columns.
#' @param metric_vars Character vector naming the exposure metric columns in
#'    `data` (e.g. `c("CMAX", "AUC")`). Must be numeric columns.
#' @param cov_vars Character vector naming the covariate columns in `data`
#'    that define the scenarios (e.g. `c("FOOD", "WTBL")`).
#' @param cov_val_ref Named vector giving the reference-scenario value of
#'    every `cov_vars` column (e.g. `c(FOOD = 0, WTBL = 70)`). Not to be
#'    confused with `cov_level_ref` in [df_forest()], which holds display
#'    labels rather than source values.
#' @param replicate_var Column in `data` identifying the replicate.
#'    Accepts bare names or strings. Default is `SIM`. Required only when
#'    `ratio = TRUE`, which forms ratios within a replicate.
#' @param ratio Logical. When `TRUE` (default), append a test/reference
#'    ratio metric for every entry of `metric_vars`. When `FALSE`, emit the
#'    absolute metrics only.
#' @param ratio_suffix Suffix appended to a metric name to name its ratio.
#'    Default `"RATIO"`, so `AUC` yields `AUCRATIO`.
#' @param cov_labels Optional named list of display labels, one entry per
#'    covariate. Each entry is either a named character vector keyed on the
#'    character form of the covariate value
#'    (`c("0" = "Fasted", "1" = "Fed")`) or a function applied to the
#'    covariate values (`function(x) paste(x, "kg")`). Values with no label
#'    fall back to their character form with a warning. Default `NULL`
#'    (every level labeled by its character form).
#' @param cov_name_ref Label written into the covariate name and covariate
#'    level columns for reference-scenario rows. Default `"Reference"`,
#'    matching the [df_forest()] argument of the same name.
#' @param metric_name_var Name of the output column of metric names.
#'    Accepts bare names or strings. Default `metric`.
#' @param metric_value_var Name of the output column of metric values.
#'    Accepts bare names or strings. Default `value`.
#' @param cov_name_var Name of the output column of covariate names.
#'    Accepts bare names or strings. Default `cov_var`.
#' @param cov_level_var Name of the output column of covariate level labels.
#'    Accepts bare names or strings. Default `cov_val`.
#' @param is_ref_var Name of the output logical column flagging
#'    reference-scenario rows. Accepts bare names or strings. Default
#'    `is_ref`. [df_forest()] prefers this column over `cov_name_ref` when
#'    it is present.
#'
#' @section Output columns:
#' Every column of `data` that is not a metric column passes through
#' unchanged and in its original order, followed by `metric_name_var`,
#' `metric_value_var`, `cov_name_var`, `cov_level_var`, and `is_ref_var`.
#' An output name that collides with a passthrough column replaces it, with
#' a warning.
#'
#' @family forest plot
#' @return A `data.frame` with one row per metric per scenario per
#'    replicate, carrying a `"cov_level_ref"` attribute: a named character
#'    vector mapping each covariate to its reference level label, ready to
#'    pass to [df_forest()] as `cov_level_ref`.
#' @export df_forest_preprocess
#'
#' @examples
#' # Raw replicate simulation output: one row per scenario per replicate
#' head(data_sad_pkforest_sim)
#'
#' draws <- df_forest_preprocess(
#'   data_sad_pkforest_sim,
#'   metric_vars   = c("CMAX", "AUC"),
#'   cov_vars      = c("FOOD", "WTBL"),
#'   cov_val_ref = c(FOOD = 0, WTBL = 70),
#'   cov_labels    = list(
#'     FOOD = c("0" = "Fasted", "1" = "Fed"),
#'     WTBL = function(x) paste(x, "kg")
#'   )
#' )
#' head(draws)
#'
#' # The per-covariate reference labels come back as an attribute
#' attr(draws, "cov_level_ref")
#'
#' # Feed straight into the forest pipeline
#' df_forest(draws)
#'
#' # ... or disperse the reference into each covariate panel
#' df_forest(draws, cov_level_ref = attr(draws, "cov_level_ref"))
#'
#' # Absolute metrics only
#' df_forest_preprocess(
#'   data_sad_pkforest_sim,
#'   metric_vars   = "AUC",
#'   cov_vars      = c("FOOD", "WTBL"),
#'   cov_val_ref = c(FOOD = 0, WTBL = 70),
#'   ratio         = FALSE
#' )

df_forest_preprocess <- function(
  data,
  metric_vars,
  cov_vars,
  cov_val_ref,
  replicate_var = "SIM",
  ratio = TRUE,
  ratio_suffix = "RATIO",
  cov_labels = NULL,
  cov_name_ref = "Reference",
  metric_name_var = "metric",
  metric_value_var = "value",
  cov_name_var = "cov_var",
  cov_level_var = "cov_val",
  is_ref_var = "is_ref"
) {
  replicate_var_str <- resolve_var(rlang::enquo(replicate_var))
  metric_name_var_str <- resolve_var(rlang::enquo(metric_name_var))
  metric_value_var_str <- resolve_var(rlang::enquo(metric_value_var))
  cov_name_var_str <- resolve_var(rlang::enquo(cov_name_var))
  cov_level_var_str <- resolve_var(rlang::enquo(cov_level_var))
  is_ref_var_str <- resolve_var(rlang::enquo(is_ref_var))

  check_df(data, "data")
  check_character_vars(metric_vars, "metric_vars")
  check_character_vars(cov_vars, "cov_vars")
  check_cov_val_ref(cov_val_ref, cov_vars)
  check_cov_labels(cov_labels, cov_vars)
  check_boolean(ratio, "ratio")
  if (!is.character(ratio_suffix) || length(ratio_suffix) != 1L ||
      is.na(ratio_suffix) || !nzchar(ratio_suffix)) {
    rlang::abort(message = "argument `ratio_suffix` must be a single non-empty character string")
  }
  if (!is.character(cov_name_ref) || length(cov_name_ref) != 1L ||
      is.na(cov_name_ref) || !nzchar(cov_name_ref)) {
    rlang::abort(message = "argument `cov_name_ref` must be a single non-empty character string")
  }
  check_varsindf(data, c(metric_vars, cov_vars), "data", "metric_vars/cov_vars")
  if (isTRUE(ratio)) {
    check_varsindf(data, replicate_var_str, "data", "replicate_var")
  }

  data <- as.data.frame(data)
  not_numeric <- metric_vars[!vapply(data[metric_vars], is.numeric, logical(1))]
  if (length(not_numeric) > 0) {
    rlang::abort(message = paste0(
      "argument `metric_vars` must name numeric columns in `data`; these are not: ",
      paste(not_numeric, collapse = ", ")
    ))
  }

  ## ---- Scenario derivation -----------------------------------------------
  ## A scenario is one combination of the `cov_vars` columns. Matching is on
  ## the character form so a numeric column and a character reference value
  ## still line up.
  cov_chr <- lapply(data[cov_vars], as.character)
  names(cov_chr) <- cov_vars
  has_na <- cov_vars[vapply(cov_chr, function(x) any(is.na(x)), logical(1))]
  if (length(has_na) > 0) {
    rlang::abort(message = paste0(
      "covariate columns must not contain `NA`; these do: ",
      paste(has_na, collapse = ", ")
    ))
  }
  ref_chr <- as.character(cov_val_ref[cov_vars])

  diff_mat <- matrix(
    unlist(Map(function(x, r) x != r, cov_chr, ref_chr)),
    nrow = nrow(data),
    ncol = length(cov_vars),
    dimnames = list(NULL, cov_vars)
  )
  n_diff <- rowSums(diff_mat)
  is_ref <- n_diff == 0L

  scenario_label <- do.call(
    paste,
    c(lapply(cov_vars, function(cn) paste0(cn, " = ", cov_chr[[cn]])), list(sep = ", "))
  )
  if (!any(is_ref)) {
    rlang::abort(message = paste0(
      "no reference scenario found: no rows in `data` match `cov_val_ref` (",
      paste0(cov_vars, " = ", ref_chr, collapse = ", "), "). ",
      "Scenarios present: ", paste(unique(scenario_label)[seq_len(min(10L, length(unique(scenario_label))))], collapse = "; ")
    ))
  }
  multi <- n_diff >= 2L
  if (any(multi)) {
    rlang::abort(message = paste0(
      "every non-reference scenario must differ from the reference in exactly one covariate; ",
      "these differ in more than one: ",
      paste(unique(scenario_label[multi])[seq_len(min(10L, length(unique(scenario_label[multi]))))], collapse = "; "),
      ". `df_forest_preprocess()` does not support factorial scenario designs."
    ))
  }

  ## Which covariate each non-reference scenario varies, and its raw value.
  which_cov <- rep(NA_character_, nrow(data))
  which_cov[!is_ref] <- cov_vars[
    max.col(diff_mat[!is_ref, , drop = FALSE], ties.method = "first")
  ]

  ## ---- Covariate level labels --------------------------------------------
  unlabeled <- character(0)
  label_vals <- function(cn, orig) {
    chr <- as.character(orig)
    lab <- cov_labels[[cn]]
    if (is.null(lab)) return(chr)
    if (is.function(lab)) return(as.character(lab(orig)))
    out <- unname(lab[chr])
    miss <- is.na(out)
    if (any(miss)) {
      unlabeled <<- c(unlabeled, paste0(cn, " = ", unique(chr[miss])))
      out[miss] <- chr[miss]
    }
    out
  }

  cov_level <- rep(NA_character_, nrow(data))
  for (cn in cov_vars) {
    sel <- which(!is_ref & which_cov == cn)
    if (length(sel) > 0) cov_level[sel] <- label_vals(cn, data[[cn]][sel])
  }

  ## The same labeller applied to each covariate's reference value gives the
  ## `cov_level_ref` mapping `df_forest()` takes for dispersed references.
  cov_level_ref <- vapply(
    cov_vars,
    function(cn) label_vals(cn, cov_val_ref[[cn]]),
    character(1)
  )
  names(cov_level_ref) <- cov_vars

  if (length(unlabeled) > 0) {
    rlang::warn(paste0(
      "`cov_labels` has no entry for these covariate values: ",
      paste(unique(unlabeled), collapse = "; "),
      ". They are labeled by their character value."
    ))
  }

  ## ---- Within-replicate test/reference ratios -----------------------------
  wide <- data
  ratio_vars <- character(0)
  if (isTRUE(ratio)) {
    rep_key <- as.character(data[[replicate_var_str]])
    if (any(is.na(rep_key))) {
      rlang::abort(message = paste0(
        "column `", replicate_var_str, "` must not contain `NA`."
      ))
    }
    reps <- unique(rep_key)
    n_ref_per <- tabulate(match(rep_key[is_ref], reps), nbins = length(reps))
    bad_rep <- n_ref_per != 1L
    if (any(bad_rep)) {
      rlang::abort(message = paste0(
        "each replicate must contain exactly one reference-scenario row; ",
        "these values of `", replicate_var_str, "` do not: ",
        paste(reps[bad_rep][seq_len(min(10L, sum(bad_rep)))], collapse = ", "),
        " (found ", paste(n_ref_per[bad_rep][seq_len(min(10L, sum(bad_rep)))], collapse = ", "), ")."
      ))
    }
    ref_idx <- which(is_ref)
    denom_idx <- ref_idx[match(rep_key, rep_key[ref_idx])]
    ratio_vars <- paste0(metric_vars, ratio_suffix)
    for (i in seq_along(metric_vars)) {
      wide[[ratio_vars[i]]] <- data[[metric_vars[i]]] / data[[metric_vars[i]]][denom_idx]
    }
  }

  ## ---- Assemble the long frame -------------------------------------------
  out_vars <- c(
    metric_name_var_str, metric_value_var_str,
    cov_name_var_str, cov_level_var_str, is_ref_var_str
  )
  passthrough <- setdiff(names(data), c(metric_vars, out_vars))
  clobbered <- intersect(names(data), out_vars)
  if (length(clobbered) > 0) {
    rlang::warn(paste0(
      "these columns of `data` are replaced by output columns of the same name: ",
      paste(clobbered, collapse = ", "),
      ". Rename them in `data`, or choose different output column names."
    ))
  }

  cov_name_col <- ifelse(is_ref, cov_name_ref, which_cov)
  cov_level_col <- ifelse(is_ref, cov_name_ref, cov_level)

  stacked_vars <- c(metric_vars, ratio_vars)
  blocks <- lapply(seq_along(stacked_vars), function(i) {
    b <- wide[, passthrough, drop = FALSE]
    b[[metric_name_var_str]] <- stacked_vars[i]
    b[[metric_value_var_str]] <- wide[[stacked_vars[i]]]
    b[[cov_name_var_str]] <- cov_name_col
    b[[cov_level_var_str]] <- cov_level_col
    b[[is_ref_var_str]] <- is_ref
    b$.src_row <- seq_len(nrow(wide))
    b$.src_metric <- i
    b
  })

  ## Order by source row then metric, so each input row's metrics stay
  ## together (the row-major order a wide-to-long pivot produces).
  out <- dplyr::bind_rows(blocks)
  out <- out[order(out$.src_row, out$.src_metric), , drop = FALSE]
  out$.src_row <- NULL
  out$.src_metric <- NULL
  rownames(out) <- NULL
  out <- as.data.frame(out)

  attr(out, "cov_level_ref") <- cov_level_ref
  out
}
