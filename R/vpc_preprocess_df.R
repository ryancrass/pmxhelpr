## ---- VPC pipeline conventions -------------------------------------------
## Internal registry of column-naming conventions and column groups produced
## by df_vpccompute(). Used by compute_one_flavor() (var_infna mutate),
## df_vpccompute() (relocate, pc select + rename), validate_vpc_stats(), and
## the public-API default values for `bin_var`. Keep these in sync if a new
## column or group is added.
##
##   BIN_MID_VAR              standardized internal name for the bin column
##   .vpc_count_cols          single, flavor-independent (counts of rows)
##   .vpc_blq_cols            std-only (LOQ has no meaning on the pc scale)
##   .vpc_obs_quantile_cols   mirrored under pc_*
##   .vpc_sim_quantile_cols   mirrored under pc_*
##   .vpc_quantile_cols       union of obs + sim quantile cols
##   .vpc_meta_cols           single, run-config (ci, pi_low, pi_hi)

BIN_MID_VAR <- "BIN_MID"

.vpc_count_cols <- c("obs_n", "obs_n_blq", "obs_prop_blq")

.vpc_blq_cols <- c("sim_prop_blq_low", "sim_prop_blq_med", "sim_prop_blq_hi")

.vpc_obs_quantile_cols <- c("obs_low", "obs_med", "obs_hi")

.vpc_sim_quantile_cols <- c("sim_low_low", "sim_low_med", "sim_low_hi",
                            "sim_med_low", "sim_med_med", "sim_med_hi",
                            "sim_hi_low",  "sim_hi_med",  "sim_hi_hi")

.vpc_quantile_cols <- c(.vpc_obs_quantile_cols, .vpc_sim_quantile_cols)

.vpc_meta_cols <- c("ci", "pi_low", "pi_hi")



#' Preprocess simulation data for VPC statistics
#'
#' @description
#' Internal function. Validates columns, resolves `loq` (inheritance from an
#' `LLOQ` column when not explicitly supplied), filters dose rows
#' (`EVID == 0`), renames simulation-output columns to standard internal
#' names, and applies BLQ encoding to `OBSDV`. Prediction-correction and the
#' (mode-aware) BLQ-encoding of `SIMDV` are handled per-flavor in
#' [df_vpccompute()].
#'
#' BLQ encoding (in this stage):
#' \itemize{
#'   \item `OBSDV` positions where `MDV == 1`, `is.na(OBSDV)`, or (when `loq`
#'     is provided) `OBSDV < loq` are encoded as `-Inf` via
#'     [`var_loqcens()`].
#' }
#'
#' @param data Simulated data from `df_mrgsim_replicate()` or equivalent.
#' @param time_var_str String name of the actual time column in `data`.
#' @param ntime_var_str String name of the nominal time column in `data`.
#' @param pred_var_str String name of the population prediction column in `data`.
#' @param sim_dv_var_str String name of the simulated DV column in `data`.
#' @param obs_dv_var_str String name of the observed DV column in `data`.
#' @param strat_var_str String or `NULL`. Stratification variable name.
#' @param irep_name_str String. Replicate identifier column name. Default is `"SIM"`.
#' @param loq Numeric value of the lower limit of quantification, or `NULL`.
#'    When `NULL` and column `LLOQ` is present in `data`, the value is inherited
#'    from the unique non-NA `LLOQ` value at replicate 1.
#'
#' @return A preprocessed data.frame with standardized columns. The resolved
#'    `loq` is attached as `attr(., "loq")`.
#' @keywords internal

df_vpcpreprocess <- function(data, time_var_str, ntime_var_str,
                             pred_var_str,
                             sim_dv_var_str, obs_dv_var_str,
                             strat_var_str = NULL, irep_name_str = "SIM",
                             loq = NULL) {

  check_df(data, "data")
  check_varsindf(data, time_var_str, "data", "time_var")
  check_varsindf(data, ntime_var_str, "data", "ntime_var")
  check_varsindf(data, pred_var_str, "data", "pred_var")
  check_varsindf(data, sim_dv_var_str, "data", "sim_dv_var")
  check_varsindf(data, obs_dv_var_str, "data", "obs_dv_var")
  check_varsindf(data, "MDV", "data", "MDV")
  check_varsindf(data, "EVID", "data", "EVID")
  check_varsindf(data, irep_name_str, "data", "irep_name")
  if (!is.null(strat_var_str)) check_varsindf(data, strat_var_str, "data", "strat_var")
  if (!is.null(strat_var_str)) check_factor(data, strat_var_str, "strat_var")

  if (!is.null(strat_var_str) && any(is.na(data[[strat_var_str]]))) {
    rlang::warn(paste0("`strat_var` ('", strat_var_str,
                       "') contains NA values; faceting will produce an `NA` facet"))
  }

  if (is.null(loq) && "LLOQ" %in% colnames(data)) {
    lloq_vals <- unique(data$LLOQ[data[[irep_name_str]] == 1 & !is.na(data$LLOQ)])
    if (length(lloq_vals) == 1L) {
      loq <- lloq_vals
      message("Inheriting `loq = ", loq, "` from `LLOQ` column in `data`.")
    } else if (length(lloq_vals) > 1L) {
      rlang::warn(paste0("`LLOQ` column in `data` has multiple unique values (",
                         paste(lloq_vals, collapse = ", "),
                         "); not inherited. Pass `loq` explicitly to enable BLQ handling."))
    }
  }

  if (!is.null(loq)) check_numeric_strict(loq, "loq")

  data <- df_prep_timevars(data, time_var_str, ntime_var_str)
  data <- dplyr::rename(data, dplyr::any_of(c(PRED = pred_var_str,
                                              SIMDV = sim_dv_var_str, OBSDV = obs_dv_var_str)))
  data <- dplyr::rename(data, !!BIN_MID_VAR := NTIME)
  data <- dplyr::filter(data, EVID == 0)

  data$OBSDV <- var_loqcens(data$OBSDV, loq = loq, mdv = data$MDV)

  attr(data, "loq") <- loq
  data
}



#' Internal helper: per-flavor VPC quantile aggregation
#'
#' @description
#' Computes the per-bin two-stage simulated quantile summary plus observed
#' quantiles for a single VPC flavor (standard or prediction-corrected).
#' Called twice by [df_vpccompute()] — once with std-mode data, once with
#' a prediction-corrected copy.
#'
#' @param data Preprocessed data with standardized columns. For the pc flavor,
#'    `OBSDV` and `SIMDV` are expected to already be prediction-corrected.
#' @param group_vars Character vector of grouping columns (`bin_var` and
#'    optional `strat_var`).
#' @param irep_name String. Replicate identifier column name.
#' @param pi Numeric vector of length 2 specifying prediction interval
#'    quantiles.
#' @param ci_bounds Numeric vector of length 2 specifying CI bounds derived
#'    from `ci`.
#' @param loq Numeric value of the lower limit of quantification, or `NULL`.
#'    When `NULL`, `sim_prop_blq_*` columns are not produced.
#' @param pcvpc Logical. Selects the BLQ-detection expression for
#'    `sim_prop_blq` in stage 1: in pcVPC, BLQ rows were masked to NA before
#'    prediction-correction, so detection uses `is.na(SIMDV)`; in std VPC,
#'    detection uses `(SIMDV < loq) | is.na(SIMDV)`.
#'
#' @return A `data.frame` with `group_vars`, `obs_n`, `obs_n_blq`,
#'    `obs_prop_blq`, the simulated quantile CIs (`sim_low_low/med/hi`,
#'    `sim_med_low/med/hi`, `sim_hi_low/med/hi`), the observed quantiles
#'    (`obs_low`, `obs_med`, `obs_hi`), and (when `loq` is supplied)
#'    `sim_prop_blq_low/med/hi`. Quantile columns are masked through
#'    [`var_infna()`].
#' @keywords internal

compute_one_flavor <- function(data, group_vars, irep_name, pi, ci_bounds,
                               loq, pcvpc) {
  stage1 <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_vars, irep_name)))) |>
    dplyr::summarise(
      obs_n     = dplyr::n(),
      obs_n_blq = sum(!is.finite(.data[["OBSDV"]])),
      sim_prop_blq = if (is.null(loq)) NA_real_
        else if (isTRUE(pcvpc)) mean(is.na(.data[["SIMDV"]]))
        else mean((.data[["SIMDV"]] < loq) | is.na(.data[["SIMDV"]])),
      sim_low      = stats::quantile(.data[["SIMDV"]], probs = pi[1], na.rm = TRUE),
      sim_med      = stats::quantile(.data[["SIMDV"]], probs = 0.5,   na.rm = TRUE),
      sim_hi       = stats::quantile(.data[["SIMDV"]], probs = pi[2], na.rm = TRUE),
      .groups = "drop"
    )

  sim_quant <- stage1 |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      obs_n     = dplyr::first(obs_n),
      obs_n_blq = dplyr::first(obs_n_blq),
      sim_low_low  = stats::quantile(sim_low, probs = ci_bounds[1], na.rm = TRUE),
      sim_low_med  = stats::quantile(sim_low, probs = 0.5,          na.rm = TRUE),
      sim_low_hi   = stats::quantile(sim_low, probs = ci_bounds[2], na.rm = TRUE),
      sim_med_low  = stats::quantile(sim_med, probs = ci_bounds[1], na.rm = TRUE),
      sim_med_med  = stats::quantile(sim_med, probs = 0.5,          na.rm = TRUE),
      sim_med_hi   = stats::quantile(sim_med, probs = ci_bounds[2], na.rm = TRUE),
      sim_hi_low   = stats::quantile(sim_hi,  probs = ci_bounds[1], na.rm = TRUE),
      sim_hi_med   = stats::quantile(sim_hi,  probs = 0.5,          na.rm = TRUE),
      sim_hi_hi    = stats::quantile(sim_hi,  probs = ci_bounds[2], na.rm = TRUE),
      sim_prop_blq_low = stats::quantile(sim_prop_blq, probs = ci_bounds[1], na.rm = TRUE),
      sim_prop_blq_med = stats::quantile(sim_prop_blq, probs = 0.5,          na.rm = TRUE),
      sim_prop_blq_hi  = stats::quantile(sim_prop_blq, probs = ci_bounds[2], na.rm = TRUE),
      .groups = "drop"
    )
  if (is.null(loq)) {
    sim_quant <- dplyr::select(sim_quant, -dplyr::any_of(c(
      "sim_prop_blq_low", "sim_prop_blq_med", "sim_prop_blq_hi")))
  }

  obs_quant <- data |>
    dplyr::filter(.data[[irep_name]] == 1) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      obs_prop_blq = sum(!is.finite(.data[["OBSDV"]])) / dplyr::n(),
      obs_low = stats::quantile(.data[["OBSDV"]], probs = pi[1], na.rm = TRUE),
      obs_med = stats::quantile(.data[["OBSDV"]], probs = 0.5,   na.rm = TRUE),
      obs_hi  = stats::quantile(.data[["OBSDV"]], probs = pi[2], na.rm = TRUE),
      .groups = "drop"
    )

  out <- dplyr::left_join(sim_quant, obs_quant, by = group_vars)

  out |>
    dplyr::mutate(dplyr::across(dplyr::all_of(.vpc_quantile_cols), var_infna))
}



#' Compute VPC quantile statistics from preprocessed data
#'
#' @description
#' Internal function. Always emits both standard and prediction-corrected
#' summary statistics: the std flavor uses raw `OBSDV`/`SIMDV`; the pc flavor
#' applies `var_loqcens` to `SIMDV` (when `loq` is supplied) and
#' [`var_predcorr()`] per (`bin_var` × `strat_var` × `CMT`) to both
#' `OBSDV` and `SIMDV`. The pc columns are returned with a `pc_` prefix.
#'
#' @param data Preprocessed simulation data, typically from [df_vpcpreprocess()].
#' @param pi Numeric vector of length 2 specifying prediction interval quantiles.
#'    Default is `c(0.05, 0.95)`.
#' @param ci Numeric scalar in `(0, 1)` specifying the simulation interval
#'    (e.g. `0.90` for a 90% CI). Default is `0.90`.
#' @param bin_var String. Binning variable name. Default is `"BIN_MID"`.
#' @param strat_var String or `NULL`. Stratification variable name. Default is `NULL`.
#' @param irep_name String. Replicate identifier column name. Default is `"SIM"`.
#' @param lower_bound Numeric. Forwarded to [`var_predcorr()`] in the pc
#'    flavor.
#' @param mode One of `"auto"` (default), `"rank"`, or `"drop"`. Controls how
#'    BLQ-encoded values flow into quantile aggregation per-flavor: `"auto"`
#'    uses `"rank"` for the std flavor and `"drop"` for the pc flavor (the
#'    historical defaults); `"rank"` and `"drop"` apply to both flavors.
#' @param loq Numeric value of the lower limit of quantification, or `NULL`.
#'    Defaults to `attr(data, "loq")`. When supplied, the std stats include
#'    `sim_prop_blq_low/med/hi`. The pc stats do *not* emit a
#'    `pc_sim_prop_blq_*` set — LOQ has no meaning on the prediction-corrected
#'    scale.
#'
#' @return A list of two data.frames with class `c("vpc_stats", "list")`:
#'    \describe{
#'      \item{`stats`}{Wide summary with both std and pc columns. Std columns
#'        are unprefixed (`obs_low/med/hi`, `sim_low_*` etc.); pc-flavor
#'        observed and simulated quantile columns carry a `pc_` prefix
#'        (`pc_obs_low`, `pc_sim_low_med`, etc.). `obs_n`, `obs_n_blq`,
#'        `obs_prop_blq`, `sim_prop_blq_*`, `ci`, `pi_low`, `pi_hi` are
#'        single (not duplicated). Carries attributes `n_replicates`,
#'        `loq`, and `strat_var`.}
#'      \item{`obs`}{First-replicate observation rows with `MDV == 0`,
#'        carrying both `OBSDV` (std) and `PC_OBSDV` (prediction-corrected).}
#'    }
#' @keywords internal

df_vpccompute <- function(data,
                          pi = c(0.05, 0.95),
                          ci = 0.90,
                          bin_var = BIN_MID_VAR,
                          strat_var = NULL,
                          irep_name = "SIM",
                          lower_bound = 0,
                          mode = c("auto", "rank", "drop"),
                          loq = attr(data, "loq")) {

  ## Force the lazy default before any data mutation: dplyr's group_by /
  ## mutate strips attributes, so reading attr(data, "loq") later would
  ## return NULL.
  force(loq)

  mode <- match.arg(mode)
  check_quantile_pair(pi, "pi")
  check_quantile_scalar(ci, "ci")
  ci_bounds <- c((1 - ci) / 2, 1 - (1 - ci) / 2)

  group_vars <- c(bin_var)
  if (!is.null(strat_var)) group_vars <- c(bin_var, strat_var)

  ## ----- Standard flavor -----
  std_mode <- if (mode == "auto") "rank" else mode
  data_std <- data
  if (std_mode == "drop") {
    data_std$OBSDV <- var_infna(data_std$OBSDV)
  }
  stats_std <- compute_one_flavor(data_std, group_vars, irep_name,
                                  pi, ci_bounds, loq, pcvpc = FALSE)

  ## ----- Prediction-corrected flavor -----
  pc_mode <- if (mode == "auto") "drop" else mode
  data_pc <- data
  ## SIMDV BLQ encoding is required for pc so that censoring is applied
  ## *before* prediction-correction. Std-VPC simulated quantiles intentionally
  ## leave SIMDV uncensored, so this encoding is pc-only.
  if (!is.null(loq)) {
    data_pc$SIMDV <- var_loqcens(data_pc$SIMDV, loq = loq)
  }
  if (pc_mode == "drop") {
    data_pc$OBSDV <- var_infna(data_pc$OBSDV)
    data_pc$SIMDV <- var_infna(data_pc$SIMDV)
  }
  pc_group_vars <- c(bin_var, strat_var)
  if ("CMT" %in% colnames(data_pc)) pc_group_vars <- c(bin_var, "CMT", strat_var)
  data_pc <- data_pc |>
    dplyr::group_by(dplyr::across(dplyr::all_of(pc_group_vars))) |>
    dplyr::mutate(OBSDV = var_predcorr(OBSDV, PRED, lower_bound),
                  SIMDV = var_predcorr(SIMDV, PRED, lower_bound)) |>
    dplyr::ungroup()
  ## Pass loq = NULL for pc so compute_one_flavor doesn't emit
  ## sim_prop_blq_* (LOQ has no meaning on the pc scale).
  stats_pc_full <- compute_one_flavor(data_pc, group_vars, irep_name,
                                      pi, ci_bounds, loq = NULL, pcvpc = TRUE)

  ## Keep only the quantile columns from the pc flavor; counts, BLQ
  ## proportions, and any sim_prop_blq carry-over are std-only.
  stats_pc <- stats_pc_full |>
    dplyr::select(dplyr::all_of(c(group_vars, .vpc_quantile_cols))) |>
    dplyr::rename_with(~ paste0("pc_", .x), -dplyr::all_of(group_vars))

  stats <- dplyr::left_join(stats_std, stats_pc, by = group_vars)

  stats <- stats |>
    dplyr::relocate(
      dplyr::all_of(group_vars),
      dplyr::all_of(.vpc_count_cols),
      dplyr::any_of(.vpc_blq_cols),
      dplyr::all_of(.vpc_quantile_cols),
      dplyr::all_of(paste0("pc_", .vpc_quantile_cols))
    )

  ## Tag rows with the configuration that drove the column scheme.
  stats[["ci"]]     <- ci
  stats[["pi_low"]] <- pi[1]
  stats[["pi_hi"]]  <- pi[2]

  ## Build obs frame with both std (raw) OBSDV and pc OBSDV columns. data_std
  ## and data_pc share row count + ordering (mutate doesn't reorder), so we
  ## can attach PC_OBSDV directly before filtering.
  data_std$OBSDV <- var_infna(data_std$OBSDV)
  data_pc$OBSDV  <- var_infna(data_pc$OBSDV)
  data_full <- data_std
  data_full[["PC_OBSDV"]] <- data_pc$OBSDV
  obs <- data_full |>
    dplyr::filter(.data[[irep_name]] == 1 & MDV == 0)

  pmx_stats(
    stats  = stats,
    obs    = obs,
    config = list(
      n_replicates = max(data[[irep_name]], na.rm = TRUE),
      loq          = loq,
      strat_var    = strat_var
    ),
    subclass = "vpc_stats"
  )
}



#' Validate a `vpc_stats` object
#'
#' @description
#' Internal helper. Asserts that `x` carries the `vpc_stats` class, contains
#' the `stats` and `obs` data.frames, and that each carries the column groups
#' downstream consumers (notably [plot_build_vpc()]) depend on. Aborts with a
#' clear message on failure; returns `x` invisibly on success.
#'
#' @param x Object to validate.
#'
#' @return `invisible(x)` on success.
#' @keywords internal

validate_vpc_stats <- function(x) {
  if (!inherits(x, "vpc_stats")) {
    rlang::abort("`x` must be a `vpc_stats` object (output of `df_vpcstats()`).")
  }
  validate_pmx_stats(x)
  if (!is.data.frame(x$obs)) {
    rlang::abort("`vpc_stats` object must contain `stats` and `obs` data.frames.")
  }

  ## Stats: identifiers, counts, full quantile sets (std + pc), metadata.
  ## Use the medians as the canonical column for each group so we don't
  ## depend on every CI bound being present.
  required_stats <- c(BIN_MID_VAR, .vpc_count_cols,
                      "obs_low", "obs_med", "obs_hi",
                      "sim_low_med", "sim_med_med", "sim_hi_med",
                      "pc_obs_low", "pc_obs_med", "pc_obs_hi",
                      "pc_sim_low_med", "pc_sim_med_med", "pc_sim_hi_med")
  missing_stats <- setdiff(required_stats, colnames(x$stats))
  if (length(missing_stats) > 0) {
    rlang::abort(paste0("`vpc_stats$stats` is missing required columns: ",
                        paste(missing_stats, collapse = ", ")))
  }

  required_obs <- c("OBSDV", "PC_OBSDV", "TIME")
  missing_obs <- setdiff(required_obs, colnames(x$obs))
  if (length(missing_obs) > 0) {
    rlang::abort(paste0("`vpc_stats$obs` is missing required columns: ",
                        paste(missing_obs, collapse = ", ")))
  }

  required_config <- c("n_replicates", "loq", "strat_var")
  missing_config  <- setdiff(required_config, names(x$config))
  if (length(missing_config) > 0) {
    rlang::abort(paste0("`vpc_stats$config` is missing required keys: ",
                        paste(missing_config, collapse = ", ")))
  }

  invisible(x)
}
