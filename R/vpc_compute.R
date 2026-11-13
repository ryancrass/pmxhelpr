## ---- VPC pipeline conventions -------------------------------------------
## Internal registry of column-naming conventions and column groups produced
## by df_vpccompute(). Used by compute_vpcstat() (var_infna mutate),
## df_vpccompute() (relocate, pc select + rename), validate_vpc_stats(), and
## the plot_build_vpc() x aesthetic. Keep these in sync if a new
## column or group is added.
##
##   .vpc_bin_col             standardized internal name for the bin column
##   .vpc_bin_bounds          interval-bin edges (only with bin_breaks)
##   .vpc_count_cols          single, flavor-independent (counts of rows)
##   .vpc_blq_cols            std-only (LOQ has no meaning on the pc scale)
##   .vpc_obs_quantile_cols   mirrored under pc_*
##   .vpc_sim_quantile_cols   mirrored under pc_*
##   .vpc_quantile_cols       union of obs + sim quantile cols
##   .vpc_meta_cols           single, run-config (ci, pi_low, pi_hi)

.vpc_bin_col <- "BIN_MID"


.vpc_bin_bounds <- c("BIN_LOW", "BIN_HI")


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
#' names, assigns bins (`BIN_MID`), and applies BLQ encoding to `OBSDV`.
#' Prediction-correction and the
#' (mode-aware) BLQ-encoding of `SIMDV` are handled per-flavor in
#' [df_vpccompute()].
#'
#' Bins: without `bin_breaks`, each nominal time (`NTIME`) is a bin and
#' `BIN_MID` is that time. With `bin_breaks`, actual time (`TIME`) is cut
#' into intervals (`[a, b)`, the last closed): `BIN_LOW` and `BIN_HI` are
#' the interval edges and `BIN_MID` the median time of the bin's records
#' (see `vpc_interval_bins()`).
#'
#' BLQ encoding (in this stage):
#' \itemize{
#'   \item `OBSDV` positions where `MDV == 1`, `is.na(OBSDV)`, or (when a LOQ
#'     source is available) `OBSDV < LOQ` are encoded as `-Inf` via
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
#' @param rep_var_str String. Replicate identifier column name. Default is `"SIM"`.
#' @param loq Numeric scalar, or `NULL`. When `NULL` and column `LLOQ` is
#'    present in `data`, per-row `LLOQ` values are used as the censoring
#'    threshold. A scalar `loq` broadcasts to a constant threshold across rows.
#' @param bin_breaks Numeric vector of interval breaks on actual time, a
#'    function returning them, or `NULL` (default) to bin by nominal time.
#' @param call The call reported in errors.
#'
#' @return A preprocessed data.frame with standardized columns and (when a LOQ
#'    source exists) a row-aligned `LOQ` column. The unique non-NA LOQ values
#'    are attached as `attr(., "loq")`, and the resolved interval breaks (or
#'    `NULL`) as `attr(., "bin_breaks")`.
#' @keywords internal

df_vpcpreprocess <- function(data, time_var_str, ntime_var_str,
                             pred_var_str,
                             sim_dv_var_str, obs_dv_var_str,
                             strat_var_str = NULL, rep_var_str = "SIM",
                             loq = NULL, bin_breaks = NULL,
                             call = rlang::caller_env()) {

  check_df(data, "data", call = call)
  check_varsindf(data,
                 c(time_var_str, ntime_var_str, pred_var_str,
                   sim_dv_var_str, obs_dv_var_str,
                   "MDV", "EVID", rep_var_str),
                 "data", "vars", call = call)
  if (!is.null(strat_var_str)) check_varsindf(data, strat_var_str, "data", "strat_var", call = call)

  if (!is.null(strat_var_str) && any(is.na(data[[strat_var_str]]))) {
    pmx_warn("{.arg strat_var} ({.var {strat_var_str}}) contains NA values; faceting will produce an {.val NA} facet.",
             "data")
  }

  if (!is.null(loq)) check_numeric_strict(loq, "loq", call = call)
  if (!is.null(bin_breaks)) check_bin_breaks(bin_breaks, call = call)
  data <- resolve_loq(data, loq)

  data <- df_prep_timevars(data, time_var_str, ntime_var_str)
  data <- dplyr::rename(data, dplyr::any_of(c(PRED = pred_var_str,
                                              SIMDV = sim_dv_var_str, OBSDV = obs_dv_var_str)))
  data <- dplyr::filter(data, .data$EVID == 0)
  resolved_breaks <- NULL
  if (is.null(bin_breaks)) {
    data <- dplyr::rename(data, !!.vpc_bin_col := "NTIME")
  } else {
    data <- vpc_interval_bins(data, bin_breaks, rep_var_str, strat_var_str, call = call)
    resolved_breaks <- attr(data, "bin_breaks")
  }
  check_single_cmt(data)

  loq_col <- if ("LOQ" %in% colnames(data)) data$LOQ else NULL
  data$OBSDV <- var_loqcens(data$OBSDV, loq = loq_col, mdv = data$MDV, call = call)

  attr(data, "loq") <- if ("LOQ" %in% colnames(data)) {
    sort(unique(data$LOQ[!is.na(data$LOQ)]))
  } else {
    NULL
  }
  attr(data, "bin_breaks") <- resolved_breaks
  data
}


## Interval bins on actual time. Each record gets the `[a, b)` interval of
## its TIME (the last closed): BIN_LOW / BIN_HI are the edges and BIN_MID the
## median TIME of the bin's records in the first replicate, within each
## stratum, so the bin is drawn where its data are.
## * Numeric `bin_breaks` apply to every stratum. Records outside them, or
##   with no TIME, are dropped with a warning that counts them per replicate.
## * A function is called on the first replicate's finite times, once per
##   stratum (once overall when unstratified). Its breaks are sorted,
##   de-duplicated, and their outer edges stretched to the stratum's time
##   range, so only records with no TIME are dropped.
## The resolved breaks ride along as attr "bin_breaks": one numeric vector,
## or a list named by stratum when a function ran per stratum.
vpc_interval_bins <- function(data, bin_breaks, rep_var_str, strat_var_str = NULL,
                              call = rlang::caller_env()) {
  per_stratum <- is.function(bin_breaks) && !is.null(strat_var_str)
  group <- if (per_stratum) {
    factor(data[[strat_var_str]], exclude = NULL)
  } else {
    factor(rep("all", nrow(data)))
  }
  rows_by_group <- split(seq_len(nrow(data)), group)
  first_rep <- is_first_rep(data, rep_var_str)
  timed <- is.finite(data$TIME)

  breaks <- lapply(names(rows_by_group), function(level) {
    if (!is.function(bin_breaks)) return(bin_breaks)
    rows <- rows_by_group[[level]]
    times <- data$TIME[rows][timed[rows]]
    resolved <- sort(unique(bin_breaks(data$TIME[rows][timed[rows] & first_rep[rows]])))
    check_resolved_breaks(resolved, if (per_stratum) paste(strat_var_str, "=", level),
                          call = call)
    resolved[1] <- min(resolved[1], times)
    resolved[length(resolved)] <- max(resolved[length(resolved)], times)
    resolved
  })
  names(breaks) <- names(rows_by_group)

  low <- hi <- rep(NA_real_, nrow(data))
  for (level in names(rows_by_group)) {
    rows <- rows_by_group[[level]]
    b <- breaks[[level]]
    i <- findInterval(data$TIME[rows], b, rightmost.closed = TRUE)
    i[i == 0 | i == length(b)] <- NA_integer_
    low[rows] <- b[i]
    hi[rows]  <- b[i + 1]
  }

  outside <- is.na(low)
  if (any(outside)) {
    n_out <- sum(outside & first_rep)
    range_txt <- if (is.function(bin_breaks)) {
      "no time"
    } else {
      paste0(min(bin_breaks), "-", max(bin_breaks))
    }
    pmx_warn("{n_out} record{?s} per replicate {?falls/fall} outside {.arg bin_breaks} ({range_txt}) and {cli::qty(n_out)}{?is/are} dropped.",
             "data")
  }
  data[[.vpc_bin_bounds[1]]] <- low
  data[[.vpc_bin_bounds[2]]] <- hi
  data <- data[!outside, , drop = FALSE]
  first_rep <- first_rep[!outside]
  bin <- paste(data[[.vpc_bin_bounds[1]]], if (!is.null(strat_var_str)) data[[strat_var_str]])
  median_time <- tapply(data$TIME[first_rep], bin[first_rep], stats::median)
  data[[.vpc_bin_col]] <- as.numeric(median_time[bin])

  attr(data, "bin_breaks") <- if (per_stratum) breaks else breaks[[1]]
  data
}


check_bin_breaks <- function(bin_breaks, call = rlang::caller_env()) {
  if (is.function(bin_breaks)) return(invisible())
  if (!is.numeric(bin_breaks) || length(bin_breaks) < 2 ||
      any(!is.finite(bin_breaks)) || is.unsorted(bin_breaks, strictly = TRUE)) {
    pmx_abort("{.arg bin_breaks} must be a function or a numeric vector of at least two finite, strictly increasing values.",
              "bad_arg", call = call)
  }
}


## The breaks a `bin_breaks` function returned, sorted and de-duplicated:
## at least two finite numbers. `stratum` names the stratum in the error.
check_resolved_breaks <- function(breaks, stratum = NULL, call = rlang::caller_env()) {
  if (!is.numeric(breaks) || length(breaks) < 2 || any(!is.finite(breaks))) {
    where <- if (!is.null(stratum)) paste0(" for ", stratum) else ""
    pmx_abort("The {.arg bin_breaks} function must return at least two distinct finite breaks{where}.",
              "bad_arg", call = call)
  }
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
#' @param rep_var String. Replicate identifier column name.
#' @param pi Numeric vector of length 2 specifying prediction interval
#'    quantiles.
#' @param ci_bounds Numeric vector of length 2 specifying CI bounds derived
#'    from `ci`.
#' @param has_loq Logical. When `TRUE`, the `data` frame is expected to carry
#'    a row-aligned `LOQ` column and `sim_prop_blq_*` columns are produced.
#'    When `FALSE`, `sim_prop_blq_*` columns are dropped.
#' @param pcvpc Logical. Selects the BLQ-detection expression for
#'    `sim_prop_blq` in stage 1: in pcVPC, BLQ rows were masked to NA before
#'    prediction-correction, so detection uses `is.na(SIMDV)`; in std VPC,
#'    detection uses `(SIMDV < LOQ) | is.na(SIMDV)` per row.
#'
#' @return A `data.frame` with `group_vars`, `obs_n`, `obs_n_blq`,
#'    `obs_prop_blq`, the simulated quantile CIs (`sim_low_low/med/hi`,
#'    `sim_med_low/med/hi`, `sim_hi_low/med/hi`), the observed quantiles
#'    (`obs_low`, `obs_med`, `obs_hi`), and (when `has_loq = TRUE`)
#'    `sim_prop_blq_low/med/hi`. Quantile columns are masked through
#'    [`var_infna()`].
#' @keywords internal

compute_vpcstat <- function(data, group_vars, rep_var, pi, ci_bounds,
                               has_loq, pcvpc) {
  pi_probs <- c(pi[1], 0.5, pi[2])

  ## Stage 1: per (group_vars x replicate). Group keys and ids come from
  ## group_by() so rows are in summarise() order; the statistics are computed
  ## over all groups at once rather than per group.
  g1 <- dplyr::group_by(data, dplyr::across(dplyr::all_of(c(group_vars, rep_var))))
  gid1 <- dplyr::group_indices(g1)
  stage1 <- dplyr::group_keys(g1)
  n1 <- nrow(stage1)
  stage1$obs_n     <- tabulate(gid1, n1)
  stage1$obs_n_blq <- tabulate(gid1[!is.finite(data[["OBSDV"]])], n1)
  stage1$sim_prop_blq <- if (!isTRUE(has_loq)) {
    rep(NA_real_, n1)
  } else {
    blq <- if (isTRUE(pcvpc)) {
      is.na(data[["SIMDV"]])
    } else {
      dplyr::coalesce((data[["SIMDV"]] < data[["LOQ"]]) | is.na(data[["SIMDV"]]), FALSE)
    }
    grouped_mean(blq, gid1, n1)
  }
  q1 <- grouped_quantile(data[["SIMDV"]], gid1, n1, pi_probs)
  stage1$sim_low <- q1[, 1]
  stage1$sim_med <- q1[, 2]
  stage1$sim_hi  <- q1[, 3]

  ## Stage 2: per group_vars, across replicates. Counts are per bin, so the
  ## first replicate's row carries them.
  g2 <- dplyr::group_by(stage1, dplyr::across(dplyr::all_of(group_vars)))
  gid2 <- dplyr::group_indices(g2)
  sim_quant <- dplyr::group_keys(g2)
  n2 <- nrow(sim_quant)
  first <- match(seq_len(n2), gid2)
  sim_quant$obs_n     <- stage1$obs_n[first]
  sim_quant$obs_n_blq <- stage1$obs_n_blq[first]
  ci_probs <- c(ci_bounds[1], 0.5, ci_bounds[2])
  stage1_cols <- c("sim_low", "sim_med", "sim_hi", if (isTRUE(has_loq)) "sim_prop_blq")
  for (col in stage1_cols) {
    q <- grouped_quantile(stage1[[col]], gid2, n2, ci_probs)
    sim_quant[[paste0(col, "_low")]] <- q[, 1]
    sim_quant[[paste0(col, "_med")]] <- q[, 2]
    sim_quant[[paste0(col, "_hi")]]  <- q[, 3]
  }

  ## Observed quantiles: the first replicate carries the observed data.
  obs_data <- data[is_first_rep(data, rep_var), , drop = FALSE]
  g3 <- dplyr::group_by(obs_data, dplyr::across(dplyr::all_of(group_vars)))
  gid3 <- dplyr::group_indices(g3)
  obs_quant <- dplyr::group_keys(g3)
  n3 <- nrow(obs_quant)
  obs_quant$obs_prop_blq <- tabulate(gid3[!is.finite(obs_data[["OBSDV"]])], n3) /
    tabulate(gid3, n3)
  q3 <- grouped_quantile(obs_data[["OBSDV"]], gid3, n3, pi_probs)
  obs_quant$obs_low <- q3[, 1]
  obs_quant$obs_med <- q3[, 2]
  obs_quant$obs_hi  <- q3[, 3]

  out <- dplyr::left_join(sim_quant, obs_quant, by = group_vars)

  out |>
    dplyr::mutate(dplyr::across(dplyr::all_of(.vpc_quantile_cols), var_infna))
}


## Internal: type-7 quantiles of `x` within each group, for all groups at once.
## `gid` holds group ids in 1..n_groups; returns an n_groups x length(probs)
## matrix, NA for groups with no non-NA values. Matches
## stats::quantile(x, probs, na.rm = TRUE) per group bit for bit: the indexing
## and interpolation follow quantile.default type 7 (interpolate only where
## x[hi] differs from x[lo], so -Inf BLQ ranks stay -Inf).
grouped_quantile <- function(x, gid, n_groups, probs) {
  keep <- !is.na(x)
  x <- x[keep]
  gid <- gid[keep]
  x <- x[order(gid, x)]
  n <- tabulate(gid, n_groups)
  start <- cumsum(n) - n
  has <- n > 0L
  out <- matrix(NA_real_, n_groups, length(probs))
  for (j in seq_along(probs)) {
    index <- 1 + (n[has] - 1) * probs[j]
    lo <- floor(index)
    hi <- ceiling(index)
    qs <- x[start[has] + lo]
    x_hi <- x[start[has] + hi]
    i <- which(index > lo & x_hi != qs)
    h <- (index - lo)[i]
    qs[i] <- (1 - h) * qs[i] + h * x_hi[i]
    out[has, j] <- qs
  }
  out
}


## Internal: mean() of `x` within each group (ids 1..n_groups). Uses mean()
## itself so the result matches a per-group summarise() exactly.
grouped_mean <- function(x, gid, n_groups) {
  unname(vapply(split(x, factor(gid, levels = seq_len(n_groups))), mean, numeric(1)))
}


## Rows of the first replicate: the replicate of the first row. Every
## replicate carries the observed data, so this one supplies it.
is_first_rep <- function(data, rep_var) {
  rep_id <- data[[rep_var]]
  !is.na(rep_id) & rep_id == rep_id[1]
}


## Whether PRED differs between replicates for the same record, as in
## df_mrgsim_replicate(params = ) output. Records are matched by position
## within each replicate; replicates of unequal size are not compared.
pred_varies_by_rep <- function(data, rep_var) {
  n_rec <- sum(is_first_rep(data, rep_var))
  if (n_rec == 0 || nrow(data) %% n_rec != 0) return(FALSE)
  pred <- matrix(data$PRED[order(data[[rep_var]])], nrow = n_rec)
  any(pred != pred[, 1], na.rm = TRUE)
}


#' Compute VPC quantile statistics from preprocessed data
#'
#' @description
#' Internal function. Always emits both standard and prediction-corrected
#' summary statistics: the std flavor uses raw `OBSDV`/`SIMDV`; the pc flavor
#' applies `var_loqcens` to `SIMDV` (when `loq` is supplied) and
#' [`var_predcorr()`] per (`bin_var` × `strat_var` × `CMT`) to both
#' `OBSDV` and `SIMDV`. The pc columns are returned with a `pc_` prefix.
#' `pc_sim_*` quantiles are masked to `NA` in bins with no quantifiable
#' observations (`obs_n == obs_n_blq`). When `PRED` differs by replicate
#' (parameter uncertainty), all pc columns and `PC_OBSDV` are `NA` and
#' `config$pred_corrected` is `FALSE`.
#'
#' @param data Preprocessed simulation data, typically from [df_vpcpreprocess()].
#' @param pi Numeric vector of length 2 specifying prediction interval quantiles.
#'    Default is `c(0.05, 0.95)`.
#' @param ci Numeric scalar in `(0, 1)` specifying the simulation interval
#'    (e.g. `0.90` for a 90% CI). Default is `0.90`.
#' @param bin_var String. Binning variable name. Default is `"BIN_MID"`.
#' @param strat_var String or `NULL`. Stratification variable name. Default is `NULL`.
#' @param rep_var String. Replicate identifier column name. Default is `"SIM"`.
#' @param lower_bound Numeric. Forwarded to [`var_predcorr()`] in the pc
#'    flavor.
#' @param mode One of `"auto"` (default), `"rank"`, or `"drop"`. Controls how
#'    BLQ-encoded values flow into quantile aggregation per-flavor: `"auto"`
#'    uses `"rank"` for the std flavor and `"drop"` for the pc flavor (the
#'    historical defaults); `"rank"` and `"drop"` apply to both flavors.
#' @param loq Numeric vector of unique LOQ values present in `data` (length 1
#'    when LLOQ is constant), or `NULL`. Defaults to `attr(data, "loq")`. When
#'    non-NULL, the std stats include `sim_prop_blq_low/med/hi` (BLQ detection
#'    reads per-row thresholds from `data$LOQ`). The pc stats do *not* emit a
#'    `pc_sim_prop_blq_*` set — LOQ has no meaning on the prediction-corrected
#'    scale.
#' @param call The call reported in errors.
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
                          bin_var = .vpc_bin_col,
                          strat_var = NULL,
                          rep_var = "SIM",
                          lower_bound = 0,
                          mode = c("auto", "rank", "drop"),
                          loq = attr(data, "loq"),
                          call = rlang::caller_env()) {

  ## Force the lazy default before any data mutation: dplyr's group_by /
  ## mutate strips attributes, so reading attr(data, "loq") later would
  ## return NULL.
  force(loq)

  mode <- match.arg(mode)
  check_quantile_pair(pi, "pi", call = call)
  check_quantile_scalar(ci, "ci", call = call)
  ci_bounds <- c((1 - ci) / 2, 1 - (1 - ci) / 2)

  group_vars <- c(bin_var)
  if (!is.null(strat_var)) group_vars <- c(bin_var, strat_var)

  ## ----- Standard -----
  std_mode <- if (mode == "auto") "rank" else mode
  data_std <- data
  if (std_mode == "drop") {
    data_std$OBSDV <- var_infna(data_std$OBSDV)
  }
  stats_std <- compute_vpcstat(data_std, group_vars, rep_var,
                                  pi, ci_bounds,
                                  has_loq = !is.null(loq), pcvpc = FALSE)

  ## ----- Prediction-corrected  -----
  pc_mode <- if (mode == "auto") "drop" else mode
  data_pc <- data
  ## SIMDV BLQ encoding is required for pc so that censoring is applied
  ## *before* prediction-correction. Std-VPC simulated quantiles intentionally
  ## leave SIMDV uncensored, so this encoding is pc-only.
  if (!is.null(loq)) {
    data_pc$SIMDV <- var_loqcens(data_pc$SIMDV, loq = data_pc$LOQ)
  } else {
    data_pc$SIMDV <- var_loqcens(data_pc$SIMDV, mdv = data_pc$MDV)
  }
  if (pc_mode == "drop") {
    data_pc$OBSDV <- var_infna(data_pc$OBSDV)
    data_pc$SIMDV <- var_infna(data_pc$SIMDV)
  }
  pc_group_vars <- c(bin_var, strat_var)
  if ("CMT" %in% colnames(data_pc)) pc_group_vars <- c(bin_var, "CMT", strat_var)
  data_pc <- data_pc |>
    dplyr::group_by(dplyr::across(dplyr::all_of(pc_group_vars))) |>
    dplyr::mutate(OBSDV = var_predcorr(.data$OBSDV, .data$PRED, lower_bound),
                  SIMDV = var_predcorr(.data$SIMDV, .data$PRED, lower_bound)) |>
    dplyr::ungroup()
  ## Pass has_loq = FALSE for pc so compute_vpcstat doesn't emit
  ## sim_prop_blq_* (LOQ has no meaning on the pc scale).
  stats_pc_full <- compute_vpcstat(data_pc, group_vars, rep_var,
                                      pi, ci_bounds,
                                      has_loq = FALSE, pcvpc = TRUE)

  ## Keep only the quantile columns from the pc flavor; counts, BLQ
  ## proportions, and any sim_prop_blq carry-over are std-only.
  stats_pc <- stats_pc_full |>
    dplyr::select(dplyr::all_of(c(group_vars, .vpc_quantile_cols))) |>
    dplyr::rename_with(~ paste0("pc_", .x), -dplyr::all_of(group_vars))

  stats <- dplyr::left_join(stats_std, stats_pc, by = group_vars)

  ## Mask pc simulated quantiles in bins with no quantifiable observations.
  ## Per-row LOQ censoring leaves only the few above-LOQ simulated values in
  ## these bins, so their quantiles are biased toward the LOQ and have no
  ## observed counterpart to compare against.
  pc_sim_cols <- paste0("pc_", .vpc_sim_quantile_cols)
  no_quant <- (stats$obs_n - stats$obs_n_blq) == 0
  stats[no_quant, pc_sim_cols] <- NA_real_

  ## PRED that differs by replicate (parameter uncertainty) has no single
  ## typical prediction to correct by, so the pc statistics are not reported.
  pred_corrected <- !pred_varies_by_rep(data, rep_var)
  if (!pred_corrected) {
    stats[paste0("pc_", .vpc_quantile_cols)] <- NA_real_
    data_pc$OBSDV <- NA_real_
  }

  stats <- stats |>
    dplyr::relocate(
      dplyr::all_of(group_vars),
      dplyr::all_of(.vpc_count_cols),
      dplyr::any_of(.vpc_blq_cols),
      dplyr::all_of(.vpc_quantile_cols),
      dplyr::all_of(paste0("pc_", .vpc_quantile_cols))
    ) |>
    ## One block per stratum, in time order (compute_vpcstat() returns bin,
    ## then stratum, order).
    dplyr::arrange(dplyr::across(dplyr::all_of(c(strat_var, bin_var))))

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
  obs <- data_full[is_first_rep(data_full, rep_var) & data_full$MDV == 0, , drop = FALSE]

  pmx_stats(
    stats  = stats,
    obs    = obs,
    config = list(
      n_replicates = dplyr::n_distinct(data[[rep_var]], na.rm = TRUE),
      ci           = ci,
      pi           = pi,
      mode         = mode,
      loq          = loq,
      strat_var    = strat_var,
      rep_var      = rep_var,
      pred_corrected = pred_corrected
    ),
    subclass = "vpc_stats"
  )
}
