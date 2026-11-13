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
#'    and `rep_var`.
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
#' @param rep_var Replicate identifier column. Accepts bare names or strings.
#'    Default is `SIM`.
#' @param irep_name `r lifecycle::badge("deprecated")` Use `rep_var`.
#' @param strat_var Stratification variable. Accepts bare names or strings.
#'    Default is `NULL`. Only a single stratifying variable is supported.
#' @param loq Numeric scalar, or `NULL`. When `NULL` and column `LLOQ` is
#'    present in `data`, per-row `LLOQ` values are used as the censoring
#'    threshold; the unique non-NA values are exposed via `config$loq` for
#'    plotting. A scalar `loq` broadcasts to a constant threshold across rows.
#' @param mode One of `"auto"` (default), `"rank"`, or `"drop"`. See
#'    [plot_vpc_cont()] for semantics. `"auto"` resolves to `"rank"` for the
#'    standard VPC and `"drop"` for the prediction-corrected VPC.
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
#'        `ci`, `pi`, `mode` (as passed, before `"auto"` is resolved
#'        for each VPC), `loq` (unique non-NA LOQ values, or `NULL`),
#'        `strat_var`, and the source column names `rep_var`, `time_var`,
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
                        rep_var = "SIM",
                        lower_bound = 0,
                        mode = c("auto", "rank", "drop"),
                        pi = c(0.05, 0.95),
                        ci = 0.90,
                        irep_name) {

  data_cols <- names(data)
  time_var_str   <- resolve_var(rlang::enquo(time_var), cols = data_cols)
  ntime_var_str  <- resolve_var(rlang::enquo(ntime_var), cols = data_cols)
  pred_var_str   <- resolve_var(rlang::enquo(pred_var), cols = data_cols)
  sim_dv_var_str <- resolve_var(rlang::enquo(sim_dv_var), cols = data_cols)
  obs_dv_var_str <- resolve_var(rlang::enquo(obs_dv_var), cols = data_cols)
  strat_var_str  <- resolve_var(rlang::enquo(strat_var), nullable = TRUE, cols = data_cols)
  rep_var_q <- rlang::enquo(rep_var)
  if (use_deprecated_arg(!missing(irep_name), !missing(rep_var), "df_vpcstats",
                         "irep_name", "rep_var")) {
    rep_var_q <- rlang::enquo(irep_name)
  }
  rep_var_str    <- resolve_var(rep_var_q, cols = data_cols)

  pre <- df_vpcpreprocess(
    data,
    time_var_str   = time_var_str,
    ntime_var_str  = ntime_var_str,
    pred_var_str   = pred_var_str,
    sim_dv_var_str = sim_dv_var_str,
    obs_dv_var_str = obs_dv_var_str,
    strat_var_str  = strat_var_str,
    rep_var_str    = rep_var_str,
    loq = loq
  )

  out <- df_vpccompute(
    pre,
    pi = pi, ci = ci,
    bin_var = .vpc_bin_col,
    strat_var = strat_var_str,
    rep_var = rep_var_str,
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
  cat_summary_header(pmx_summary_header(x, .vpc_summary_keys))
  cat_vpc_column_groups(vpc_column_groups(x))

  if (n_head > 0 && nrow(x$stats) > 0) {
    cat(sprintf("\n  head(stats, %d):\n", n_head))
    print(utils::head(x$stats, n_head))
  }

  cat("\n  Use `x$stats` and `x$obs` for the underlying data.frames.\n")
  invisible(x)
}


.vpc_summary_keys <- c("n_replicates", "ci", "pi", "mode", "loq", "strat_var")


## Columns of `x$stats` in each print group, in print order; absent groups
## are character(0).
vpc_column_groups <- function(x) {
  cols <- colnames(x$stats)
  list(
    identifiers   = c(intersect(.vpc_bin_col, cols), intersect(x$config$strat_var, cols)),
    counts        = intersect(.vpc_count_cols, cols),
    sim_blq       = intersect(.vpc_blq_cols, cols),
    std_observed  = intersect(.vpc_obs_quantile_cols, cols),
    std_simulated = intersect(.vpc_sim_quantile_cols, cols),
    pc_observed   = intersect(paste0("pc_", .vpc_obs_quantile_cols), cols),
    pc_simulated  = intersect(paste0("pc_", .vpc_sim_quantile_cols), cols),
    metadata      = intersect(.vpc_meta_cols, cols)
  )
}

cat_vpc_column_groups <- function(groups) {
  labels <- c(identifiers = "identifiers", counts = "counts", sim_blq = "sim BLQ",
              std_observed = "std observed", std_simulated = "std simulated",
              pc_observed = "pc observed", pc_simulated = "pc simulated",
              metadata = "metadata")
  cat("  column groups (stats):\n")
  for (g in names(labels)) {
    cols <- groups[[g]]
    if (length(cols) == 0) next
    cat(sprintf("    %-13s: %s%s\n", labels[[g]], paste(cols, collapse = ", "),
                if (g == "sim_blq") "  [std-only]" else ""))
  }
}


#' Summary method for `vpc_stats`
#'
#' @description
#' Summarises a [df_vpcstats()] result: the run configuration, the column
#' groups present in `stats`, and the coverage of each observed quantile by
#' its simulated confidence interval. Printing it shows the same content as
#' [print.vpc_stats()] without the head preview, followed by the coverage
#' table, which suits vignette output and test snapshots.
#'
#' Coverage flags, in each bin and stratum, whether the observed quantile
#' lies inside the simulated CI of the same quantile, e.g.
#' `sim_med_low <= obs_med <= sim_med_hi`, and the `pc_` columns likewise for
#' the prediction-corrected VPC. The flag is `NA` where either side is `NA`
#' (fully censored bins in `"rank"` mode, or prediction-corrected bins with no
#' quantifiable observations), and those bins are not counted in the shares.
#' When `stats` has a LOQ (the `sim_prop_blq_*` columns), the censored VPC's
#' median line is flagged the same way: the observed proportion BLQ against
#' its simulated CI (`sim_prop_blq_low <= obs_prop_blq <= sim_prop_blq_hi`).
#' The censored VPC has no lower or upper line. The shares are the proportion of flagged bins inside the CI,
#' per stratum and across all strata. When the model describes the data,
#' each observed value falls inside its interval with probability of about
#' `ci`.
#' Neighbouring bins are correlated, so coverage is descriptive and is not a
#' test.
#'
#' @param object A `vpc_stats` object.
#' @param x A `summary.vpc_stats` object.
#' @param ... Currently unused.
#'
#' @return A `summary.vpc_stats` object (also `summary.pmx_stats`): a list
#'    with `header` (the class tag, `dim` of `stats`, `obs_rows`, and the
#'    `config` values `n_replicates`, `ci`, `pi`, `mode`, `loq`, `strat_var`),
#'    `column_groups` (a named list of the `stats` columns in each group:
#'    `identifiers`, `counts`, `sim_blq`, `std_observed`, `std_simulated`,
#'    `pc_observed`, `pc_simulated`, `metadata`; empty groups are
#'    `character(0)`), and `coverage`, a list of three data.frames. Each
#'    identifies the line by `quantile` (`"low"`, `"med"`, `"hi"`) and `prob`
#'    (its probability, from `pi`; `NA` for the censored VPC, whose median
#'    line is a proportion BLQ, not a quantile), and the VPC by `type`
#'    (`"cont"` or `"cens"`) and `pcvpc`, as in [plot_build_vpc()]: standard
#'    (`"cont"`, `FALSE`), prediction-corrected (`"cont"`, `TRUE`), or
#'    censored (`"cens"`, `FALSE`; only when `stats` has a LOQ):
#'    \describe{
#'      \item{`bins`}{One row per bin, stratum, quantile, and VPC: `BIN_MID`,
#'        the `strat_var` column, `quantile`, `prob`, `type`, `pcvpc`, and
#'        `inside` (`TRUE`, `FALSE`, or `NA` when the bin cannot be judged).}
#'      \item{`strata`}{Per stratum (`NULL` when the VPC is not stratified):
#'        the `strat_var` column, `quantile`, `prob`, `type`, `pcvpc`,
#'        `n_bins` (bins with a non-`NA` flag), `n_inside`, and `coverage`
#'        (`n_inside / n_bins`, `NA` when `n_bins` is 0).}
#'      \item{`overall`}{The same shares pooled across all bins and strata.}
#'    }
#' @export
#' @method summary vpc_stats
#'
#' @examples
#' model <- model_mread_load(model = "pkmodel")
#' simout <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT %in% c(1, 2)),
#'                               model = model, replicates = 10,
#'                               dv_var = ODV, carry_out = "FOOD")
#' s <- summary(df_vpcstats(simout, strat_var = FOOD, loq = 1))
#' s
#' s$coverage$strata

summary.vpc_stats <- function(object, ...) {
  structure(
    list(header = pmx_summary_header(object, .vpc_summary_keys),
         column_groups = vpc_column_groups(object),
         coverage = vpc_coverage(object$stats, object$config$strat_var,
                                 object$config$pi)),
    class = c("summary.vpc_stats", "summary.pmx_stats")
  )
}


#' @rdname summary.vpc_stats
#' @export
print.summary.vpc_stats <- function(x, ...) {
  cat_summary_header(x$header)
  cat_vpc_column_groups(x$column_groups)
  cat_vpc_coverage(x$coverage, x$header$config$ci, x$header$config$strat_var)
  cat("\n  Use `x$stats` and `x$obs` for the underlying data.frames.\n")
  invisible(x)
}


## Coverage of each observed quantile by the simulated CI of that quantile.
## Each row names its VPC by the plot builder's arguments: `type` ("cont" or
## "cens") and `pcvpc`. The cens VPC has one line, the median proportion BLQ,
## so its row is `quantile = "med"` with `prob = NA` (it is not a DV
## quantile); it is present only when `stats` carries the sim_prop_blq_*
## columns (a LOQ was given). `bins` flags each (bin x stratum x quantile x
## VPC): `inside` is NA when the observed value or a CI bound is NA.
## `strata` (NULL when unstratified) and `overall` are the shares of the
## flagged bins.
vpc_coverage <- function(stats, strat_var, pi) {
  quantiles <- c("low", "med", "hi")
  probs <- c(pi[1], 0.5, pi[2])
  keys <- as.data.frame(stats[c(.vpc_bin_col, strat_var)])
  flag <- function(quantile, prob, type, pcvpc, obs, low, hi) {
    part <- keys
    part$quantile <- factor(quantile, levels = quantiles)
    part$prob     <- prob
    part$type     <- factor(type, levels = c("cont", "cens"))
    part$pcvpc    <- pcvpc
    part$inside   <- obs >= low & obs <= hi
    part
  }
  parts <- list()
  for (pcvpc in c(FALSE, TRUE)) {
    prefix <- if (pcvpc) "pc_" else ""
    for (i in seq_along(quantiles)) {
      q <- quantiles[i]
      parts[[length(parts) + 1]] <- flag(
        q, probs[i], "cont", pcvpc,
        obs = stats[[paste0(prefix, "obs_", q)]],
        low = stats[[paste0(prefix, "sim_", q, "_low")]],
        hi  = stats[[paste0(prefix, "sim_", q, "_hi")]]
      )
    }
  }
  if (all(.vpc_blq_cols %in% names(stats))) {
    parts[[length(parts) + 1]] <- flag(
      "med", NA_real_, "cens", FALSE,
      obs = stats[["obs_prop_blq"]],
      low = stats[["sim_prop_blq_low"]],
      hi  = stats[["sim_prop_blq_hi"]]
    )
  }
  bins <- dplyr::bind_rows(parts) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(
      c(strat_var, .vpc_bin_col, "quantile", "type", "pcvpc")
    )))
  out <- list(
    bins    = bins,
    strata  = if (!is.null(strat_var)) coverage_shares(bins, strat_var),
    overall = coverage_shares(bins, NULL)
  )
  out$bins$quantile <- as.character(out$bins$quantile)
  out$bins$type     <- as.character(out$bins$type)
  out
}

coverage_shares <- function(bins, strat_var) {
  out <- bins |>
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c(strat_var, "quantile", "prob", "type", "pcvpc")
    ))) |>
    dplyr::summarise(n_bins = sum(!is.na(.data$inside)),
                     n_inside = sum(.data$inside, na.rm = TRUE),
                     .groups = "drop")
  out$coverage <- ifelse(out$n_bins > 0, out$n_inside / out$n_bins, NA_real_)
  out$quantile <- as.character(out$quantile)
  out$type     <- as.character(out$type)
  as.data.frame(out)
}

## Display label for a coverage row: the quantile's percent (`"5%"`), or
## `"BLQ"` for the cens VPC's proportion-BLQ line.
coverage_label <- function(prob, type) {
  ifelse(type == "cens", "BLQ", paste0(signif(100 * prob, 3), "%"))
}

## Display text for coverage shares: `"n_inside/n_bins pct"`, or `"-"` when
## no bin could be judged. `pad = TRUE` right-aligns the counts and percents
## across the vector, for tables; plot labels use it unpadded.
fmt_coverage <- function(n_inside, n_bins, coverage, pad = FALSE) {
  counts <- paste0(n_inside, "/", n_bins)
  pct <- sprintf("%.0f%%", 100 * coverage)
  if (pad) {
    counts <- format(counts, justify = "right")
    pct <- format(pct, justify = "right")
  }
  ifelse(n_bins == 0, "-", paste(counts, pct))
}

## One line per (stratum x quantile), then the Overall lines, with the std,
## pc, and (with a LOQ) cens cells side by side. The cens VPC has only the
## median line, so its cell is blank on the low and hi lines. Unstratified
## coverage prints the Overall lines without a stratum column.
cat_vpc_coverage <- function(coverage, ci, strat_var) {
  shares <- coverage$overall
  if (!is.null(coverage$strata)) {
    strata <- coverage$strata
    strata[[strat_var]] <- as.character(strata[[strat_var]])
    shares[[strat_var]] <- "Overall"
    shares <- rbind(strata, shares[names(strata)])
  }
  shares$cell <- fmt_coverage(shares$n_inside, shares$n_bins, shares$coverage, pad = TRUE)
  row_key <- function(d) do.call(paste, c(d[c(strat_var, "quantile")], sep = "\r"))
  vpcs <- list(std  = shares$type == "cont" & !shares$pcvpc,
               pc   = shares$type == "cont" & shares$pcvpc,
               cens = shares$type == "cens")
  std <- shares[vpcs$std, ]
  tab <- std[strat_var]
  tab$quantile <- coverage_label(std$prob, std$type)
  for (v in names(vpcs)) {
    if (!any(vpcs[[v]])) next
    rows <- shares[vpcs[[v]], ]
    cell <- rows$cell[match(row_key(std), row_key(rows))]
    tab[[v]] <- ifelse(is.na(cell), "", cell)
  }
  ## Left-justify each column (header included) to its widest entry.
  cols <- Map(function(col, nm) format(c(nm, as.character(col))), tab, names(tab))
  lines <- trimws(do.call(paste, c(cols, sep = "  ")), "right")
  cat(sprintf("  coverage (observed value inside the %s%% simulated CI):\n",
              format(100 * ci)))
  cat(paste0("    ", lines), sep = "\n")
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
