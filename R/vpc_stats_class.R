#' Test whether an object is a `vpc_stats` container
#'
#' @description
#' Predicate that returns `TRUE` if `x` carries the `"vpc_stats"` class — i.e.,
#' it is the container returned by [df_vpcstats()] (also accepted by
#' [plot_vpc_cont()] on the precomputed-stats fast path).
#'
#' @param x Object to test.
#' @param strict Logical. When `TRUE`, additionally runs
#'    `validate_vpc_stats()` and returns `FALSE` on validation failure.
#'    Default `FALSE` (class-tag check only, cheap).
#'
#' @return Logical scalar.
#' @export is_vpc_stats
#'
#' @examples
#' model <- model_mread_load(model = "pkmodel")
#' simout <- df_mrgsim_replicate(data = data_sad, model = model, replicates = 5,
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
#' values (`n_replicates`, `loq`, `strat_var`), the column groups present
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
  cat("<vpc_stats>\n")
  cat(sprintf("  stats: %d rows x %d columns\n",
              nrow(x$stats), ncol(x$stats)))
  cat(sprintf("  obs:   %d rows\n", nrow(x$obs)))
  cat(sprintf("  config: n_replicates = %s, loq = %s, strat_var = %s\n",
              format(x$config$n_replicates),
              format(x$config$loq),
              format(x$config$strat_var)))

  bin_present  <- BIN_MID_VAR %in% colnames(x$stats)
  strat_in_stats <- intersect(x$config$strat_var, colnames(x$stats))
  identifiers <- c(if (bin_present) BIN_MID_VAR, strat_in_stats)
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
