#' Compute VPC summary statistics from raw simulation data
#'
#' @description
#' Returns the VPC summary statistics and observation overlay underlying a
#' continuous-data VPC plot. Takes raw simulation output (e.g., from
#' [df_mrgsim_replicate()]) and always emits **both** standard and
#' prediction-corrected statistics in a single call. Preprocessing — column
#' renames, dose-row filtering, BLQ encoding, prediction-correction — is
#' applied internally.
#'
#' For a plotted VPC, use [plot_vpc_cont()], which is a thin wrapper that
#' calls `df_vpcstats()` and forwards the result to the internal plot
#' builder. The standard vs. prediction-corrected view is selected at plot
#' time via `plot_vpc_cont(pcvpc = TRUE/FALSE)`.
#'
#' @param data Raw simulation output. Must contain `EVID`, `MDV`, `PRED`, and
#'    columns named by `time_var`, `ntime_var`, `sim_dv_var`, `obs_dv_var`,
#'    and `irep_name`.
#' @param time_var Column containing the actual time variable in `data`.
#'    Accepts bare names or strings. Default is `TIME`.
#' @param ntime_var Column containing the nominal time variable in `data`.
#'    Accepts bare names or strings. Default is `NTIME`.
#' @param pred_var Column containing population predictions in `data`.
#'    Accepts bare names or strings. Default is `PRED`. Required for the
#'    prediction-corrected statistics.
#' @param sim_dv_var Column containing simulated DV in `data`.
#'    Accepts bare names or strings. Default is `SIMDV`.
#' @param obs_dv_var Column containing observed DV in `data`.
#'    Accepts bare names or strings. Default is `OBSDV`.
#' @param irep_name Replicate identifier column. Accepts bare names or strings.
#'    Default is `SIM`.
#' @param strat_var Stratification variable. Accepts bare names or strings.
#'    Default is `NULL`. Only a single stratifying variable is supported.
#' @param loq Numeric scalar, or `NULL`. When `NULL` and column `LLOQ` is
#'    present in `data`, per-row `LLOQ` values are used as the censoring
#'    threshold; the unique non-NA values are exposed via `config$loq` for
#'    plotting. A scalar `loq` broadcasts to a constant threshold across rows.
#' @param mode One of `"auto"` (default), `"rank"`, or `"drop"`. See
#'    [plot_vpc_cont()] for semantics. `"auto"` resolves to `"rank"` for the
#'    standard flavor and `"drop"` for the prediction-corrected flavor.
#' @param pi Numeric vector of length 2 specifying prediction interval quantiles.
#'    Default is `c(0.05, 0.95)`.
#' @param ci Numeric scalar in `(0, 1)` specifying the simulation interval
#'    (e.g., `0.90` for a 90% CI). Default is `0.90`.
#' @inheritParams var_predcorr
#'
#' @family vpc
#' @return A `vpc_stats` container (subclass of `pmx_stats`) with three
#'    slots:
#'    \describe{
#'      \item{`stats`}{Wide summary statistics. Standard-VPC columns are
#'        unprefixed (`obs_low/med/hi`, `sim_low_low/med/hi`, etc.);
#'        prediction-corrected counterparts carry a `pc_` prefix
#'        (`pc_obs_low`, `pc_sim_low_med`, etc.). `obs_n`, `obs_n_blq`,
#'        `obs_prop_blq`, `sim_prop_blq_*`, `ci`, `pi_low`, `pi_hi` are
#'        single (not duplicated). `sim_prop_blq_*` is std-only — LOQ has
#'        no meaning on the prediction-corrected scale. `-Inf` quantile
#'        values from rank-mode fully-censored bins are masked to `NA`.
#'        `pc_sim_*` quantiles are `NA` in bins with no quantifiable
#'        observations (`obs_n == obs_n_blq`).}
#'      \item{`obs`}{First-replicate observation rows with `MDV == 0`, used as
#'        the scatter overlay in [plot_vpc_cont()]. `OBSDV` carries the
#'        std-scale value; `PC_OBSDV` carries the prediction-corrected
#'        counterpart.}
#'      \item{`config`}{Named list of the run configuration: `n_replicates`,
#'        `ci`, `pi`, `mode` (as passed, before the per-flavor `"auto"`
#'        resolution), `loq` (unique non-NA LOQ values, or `NULL`),
#'        `strat_var`, and the source column names `irep_name`, `time_var`,
#'        `ntime_var`, `pred_var`, `sim_dv_var`, and `obs_dv_var`.
#'        [plot_vpc_legend()] reads `ci`, `pi`, and `loq` from it via its
#'        `stats` argument.}
#'    }
#'    Pass directly to [plot_vpc_cont()], [plot_vpc_cens()], or
#'    [plot_build_vpc()] to replot without recomputing.
#' @export df_vpcstats
#'
#' @examples
#' model <- model_mread_load(model = "pkmodel")
#' simout <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT %in% c(1, 2)),
#'                               model = model, replicates = 10,
#'                               dv_var = ODV)
#' result <- df_vpcstats(simout, loq = 1)
#' head(result$stats)
#' head(result$obs)

df_vpcstats <- function(data,
                        time_var = "TIME",
                        ntime_var = "NTIME",
                        pred_var = "PRED",
                        sim_dv_var = "SIMDV",
                        obs_dv_var = "OBSDV",
                        strat_var = NULL,
                        loq = NULL,
                        irep_name = "SIM",
                        lower_bound = 0,
                        mode = c("auto", "rank", "drop"),
                        pi = c(0.05, 0.95),
                        ci = 0.90) {

  time_var_str   <- resolve_var(rlang::enquo(time_var))
  ntime_var_str  <- resolve_var(rlang::enquo(ntime_var))
  pred_var_str   <- resolve_var(rlang::enquo(pred_var))
  sim_dv_var_str <- resolve_var(rlang::enquo(sim_dv_var))
  obs_dv_var_str <- resolve_var(rlang::enquo(obs_dv_var))
  strat_var_str  <- resolve_var(rlang::enquo(strat_var), nullable = TRUE)
  irep_name_str  <- resolve_var(rlang::enquo(irep_name))

  pre <- df_vpcpreprocess(
    data,
    time_var_str   = time_var_str,
    ntime_var_str  = ntime_var_str,
    pred_var_str   = pred_var_str,
    sim_dv_var_str = sim_dv_var_str,
    obs_dv_var_str = obs_dv_var_str,
    strat_var_str  = strat_var_str,
    irep_name_str  = irep_name_str,
    loq = loq
  )

  out <- df_vpccompute(
    pre,
    pi = pi, ci = ci,
    bin_var = .vpc_bin_col,
    strat_var = strat_var_str,
    irep_name = irep_name_str,
    lower_bound = lower_bound,
    mode = mode
  )
  ## Source column names, for provenance; the stats use standardized names.
  out$config[c("time_var", "ntime_var", "pred_var", "sim_dv_var", "obs_dv_var")] <-
    list(time_var_str, ntime_var_str, pred_var_str, sim_dv_var_str, obs_dv_var_str)
  out
}


#' Test whether an object is a `vpc_stats` container
#'
#' @description
#' Predicate that returns `TRUE` if `x` carries the `"vpc_stats"` class — i.e.,
#' it is the container returned by [df_vpcstats()] (also accepted by
#' [plot_vpc_cont()] on the precomputed-stats fast path).
#'
#' @param x Object to test.
#' @param strict Logical. When `TRUE`, additionally runs
#'    [validate_pmx_stats()] and returns `FALSE` on validation failure.
#'    Default `FALSE` (class-tag check only, cheap).
#'
#' @family vpc
#' @return Logical scalar.
#' @export is_vpc_stats
#'
#' @examples
#' model <- model_mread_load(model = "pkmodel")
#' simout <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT %in% c(1, 2)),
#'                               model = model, replicates = 5,
#'                               dv_var = ODV)
#' out <- df_vpcstats(simout)
#' is_vpc_stats(out)        # TRUE
#' is_vpc_stats(out$stats)  # FALSE — that's the inner data.frame

is_vpc_stats <- function(x, strict = FALSE) {
  ok <- inherits(x, "vpc_stats")
  if (!ok || !isTRUE(strict)) return(ok)
  tryCatch({validate_vpc_stats(x); TRUE}, error = function(e) FALSE)
}


#' Print method for `vpc_stats`
#'
#' @description
#' Focused summary of a [df_vpcstats()] result: object dimensions, run-config
#' values (`n_replicates`, `ci`, `pi`, `mode`, `loq`, `strat_var`), the column groups present
#' in `stats`, and a short head preview. Inspect the underlying data.frames
#' directly via `x$stats` and `x$obs`; inspect run config via `x$config`.
#'
#' @param x A `vpc_stats` object.
#' @param n_head Integer. Number of rows of `stats` to preview.
#' @param ... Currently unused.
#'
#' @return `invisible(x)`.
#' @export
#' @method print vpc_stats

print.vpc_stats <- function(x, n_head = 3, ...) {
  pmx_stats_header(x, c("n_replicates", "ci", "pi", "mode", "loq", "strat_var"))

  bin_present  <- .vpc_bin_col %in% colnames(x$stats)
  strat_in_stats <- intersect(x$config$strat_var, colnames(x$stats))
  identifiers <- c(if (bin_present) .vpc_bin_col, strat_in_stats)
  blq_present <- intersect(.vpc_blq_cols, colnames(x$stats))
  std_obs <- intersect(.vpc_obs_quantile_cols, colnames(x$stats))
  std_sim <- intersect(.vpc_sim_quantile_cols, colnames(x$stats))
  pc_obs  <- intersect(paste0("pc_", .vpc_obs_quantile_cols), colnames(x$stats))
  pc_sim  <- intersect(paste0("pc_", .vpc_sim_quantile_cols), colnames(x$stats))
  meta_present <- intersect(.vpc_meta_cols, colnames(x$stats))

  fmt_group <- function(label, cols) {
    if (length(cols) == 0) return(invisible())
    cat(sprintf("    %-13s: %s\n", label, paste(cols, collapse = ", ")))
  }

  cat("  column groups (stats):\n")
  fmt_group("identifiers",  identifiers)
  fmt_group("counts",       intersect(.vpc_count_cols, colnames(x$stats)))
  if (length(blq_present) > 0) {
    cat(sprintf("    %-13s: %s  [std-only]\n", "sim BLQ",
                paste(blq_present, collapse = ", ")))
  }
  fmt_group("std observed", std_obs)
  fmt_group("std simulated", std_sim)
  fmt_group("pc observed",  pc_obs)
  fmt_group("pc simulated", pc_sim)
  fmt_group("metadata",     meta_present)

  if (n_head > 0 && nrow(x$stats) > 0) {
    cat(sprintf("\n  head(stats, %d):\n", n_head))
    print(utils::head(x$stats, n_head))
  }

  cat("\n  Use `x$stats` and `x$obs` for the underlying data.frames.\n")
  invisible(x)
}


#' Summary method for `vpc_stats`
#'
#' @description
#' Same content as [print.vpc_stats()] but without the head preview. Suitable
#' for vignette output and test snapshots.
#'
#' @param object A `vpc_stats` object.
#' @param ... Currently unused.
#'
#' @return `invisible(object)`.
#' @export
#' @method summary vpc_stats

summary.vpc_stats <- function(object, ...) {
  print.vpc_stats(object, n_head = 0)
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
#' @param arg,call The argument name and call reported in errors.
#'
#' @return `invisible(x)` on success.
#' @keywords internal

validate_vpc_stats <- function(x, arg = rlang::caller_arg(x),
                               call = rlang::caller_env()) {
  ## Stats: identifiers, counts, full quantile sets (std + pc), metadata.
  ## Use the medians as the canonical column for each group so we don't
  ## depend on every CI bound being present.
  check_pmx_stats(
    x, "vpc_stats", "df_vpcstats",
    stats_cols = c(.vpc_bin_col, .vpc_count_cols,
                   "obs_low", "obs_med", "obs_hi",
                   "sim_low_med", "sim_med_med", "sim_hi_med",
                   "pc_obs_low", "pc_obs_med", "pc_obs_hi",
                   "pc_sim_low_med", "pc_sim_med_med", "pc_sim_hi_med"),
    obs_cols = c("OBSDV", "PC_OBSDV", "TIME"),
    config_keys = c("n_replicates", "ci", "pi", "loq", "strat_var"),
    arg = arg, call = call
  )

  ## Cens invariant: if df_vpcstats() was called with a LOQ source, the
  ## sim_prop_blq_* columns must be present. Catches malformed
  ## manually-constructed containers; legit no-LOQ containers (config$loq
  ## NULL, columns absent) are handled by the cens user-facing check in
  ## plot_build_vpc().
  if (!is.null(x$config$loq) && !"sim_prop_blq_med" %in% colnames(x$stats)) {
    pmx_abort("{.code {arg}$config$loq} is set but {.code {arg}$stats} is missing {.code sim_prop_blq_*} columns.",
              "bad_stats", call = call)
  }

  invisible(x)
}


#' @rdname validate_pmx_stats
#' @export
validate_pmx_stats.vpc_stats <- function(x, ..., arg = rlang::caller_arg(x),
                                         call = rlang::caller_env()) {
  validate_vpc_stats(x, arg = arg, call = call)
}
