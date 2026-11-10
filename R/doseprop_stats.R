#' Compute estimate table for log-log regression
#'
#' Internal per-fit helper used by [df_doseprop()]. Converts a single `lm` fit
#' on `log(metric_value) ~ log(dose)` into a one-row stats table with Power,
#' CI, Proportional, PowerCI, and Interpretation columns.
#'
#' @param fit `lm` model object for the log-log regression
#' @param method character string specifying the distribution to be used to derived the confidence interval.
#'    Options are "normal" (default) and "tdist"
#' @param ci confidence interval to be calculated: a single numeric value
#'    in `(0, 1)`, e.g. `0.90` (default) or `0.95`.
#' @param sigdigits number of significant digits for the reported
#'    `Intercept`, `StandardError`, `Power`, `LCL`, `UCL`, and `PowerCI`
#'    columns. The `Proportional` / `Interpretation` verdict is taken from
#'    the unrounded confidence bounds, so it never moves with `sigdigits`.
#'
#' @return `data.frame`
#' @keywords internal
#' @noRd

df_loglog <- function(fit,
                      method = "normal",
                      ci = 0.9,
                      sigdigits = 3) {

  check_loglog_args(method, ci, sigdigits)

  int <- stats::coef(fit)[[1]]
  est <- stats::coef(fit)[[2]]
  se <- sqrt(diag(stats::vcov(fit)))[[2]]

  ## `method` is validated above, so the critical value is a straight choice
  ## between the normal and t quantile.
  crit <- switch(
    method,
    normal = stats::qnorm((1 + ci)/2),
    tdist  = stats::qt((1 + ci)/2, fit$df.residual)
  )
  lcl <- est - crit*se
  ucl <- est + crit*se

  tab <- data.frame(
    Intercept = signif(int, digits=sigdigits),
    StandardError = signif(se, digits=sigdigits),
    CI = paste0(ci*100, "%"),
    Power = signif(est, digits = sigdigits),
    LCL = signif(lcl, digits=sigdigits),
    UCL = signif(ucl, digits=sigdigits))

  ## The verdict is judged on the unrounded bounds: `sigdigits` is a display
  ## setting and must not steer the conclusion. A bound of 1.0004 rounds to
  ## 1.00 at three digits, which would turn "Greater than dose-proportional"
  ## into "Dose-proportional".
  tab$Proportional <- !((lcl < 1 & ucl < 1) | (lcl > 1 & ucl > 1))
  ## Pad every value to `sigdigits` so the facet labels line up
  ## ("1.00 (90% CI 0.900-1.20)", not "1 (90% CI 0.9-1.2)").
  tab$PowerCI <- paste0("Power: ", fmt_sig(est, sigdigits),
                        " (", tab$CI, " CI ",
                        fmt_sig(lcl, sigdigits), "-", fmt_sig(ucl, sigdigits), ")")
  tab$Interpretation <- dplyr::case_when(lcl < 1 & ucl < 1 ~ "Less than dose-proportional",
                                         lcl > 1 & ucl > 1 ~ "Greater than dose-proportional",
                                         .default = "Dose-proportional")

  tab
}


#' Compute and tabulate estimates for log-log regression
#'
#' @description
#' Computes per-metric log-log regression statistics and returns them in a
#' cacheable, replottable container. The returned object is a
#' [pmx_stats()] container with class
#' `c("doseprop_stats", "pmx_stats")` and three slots — `stats` (per-metric
#' regression body), `obs` (filtered observation rows used for the scatter
#' overlay), and `config` (regression configuration: `metric_name_var`, `metric_value_var`,
#' `dose_var`, `ci`, `method`) — so that [plot_doseprop()] /
#' [plot_build_doseprop()] can render directly from this object without
#' re-fitting any regressions.
#'
#' @param data Input dataset for log-log regression.
#'    Default expected format is output from `PKNCA::pk.nca()` (i.e., SDTM PP formatting).
#' @param metrics character vector of exposure metrics in `data` to plot
#' @param metric_name_var Column in `data` containing the metric names listed in `metrics`.
#'    Accepts bare names or strings. Default is `PPTESTCD`.
#' @param metric_value_var Column in `data` containing the exposure metric values
#'    (dependent variable). Accepts bare names or strings. Default is `PPORRES`.
#' @param dose_var Column in `data` containing the dose (independent variable).
#'    Accepts bare names or strings. Default is `DOSE`.
#' @param method character string specifying the distribution to be used to derive the confidence interval.
#'    Options are `"normal"` (default) and `"tdist"`.
#' @param ci confidence interval to be calculated: a single numeric value
#'    in `(0, 1)`, e.g. `0.90` (default) or `0.95`.
#' @param sigdigits number of significant digits for the reported
#'    `Intercept`, `StandardError`, `Power`, `LCL`, `UCL`, and `PowerCI`
#'    columns. `PowerCI` pads every value to this many digits so the facet
#'    labels line up. The `Proportional` / `Interpretation` verdict is taken
#'    from the unrounded confidence bounds, so it never moves with
#'    `sigdigits`.
#'
#' @family dose proportionality
#' @return A `doseprop_stats` container (subclass of `pmx_stats`) with three
#'    slots:
#'    \describe{
#'      \item{`stats`}{One row per metric and columns `Intercept`,
#'        `StandardError`, `CI`, `Power`, `LCL`, `UCL`, `Proportional`,
#'        `PowerCI`, `Interpretation`, plus the column named in `metric_name_var`.
#'        `Interpretation` is `"Dose-proportional"` when the `ci` interval for
#'        `Power` contains 1, and less or greater than dose-proportional when it
#'        lies wholly below or above 1. It is a screening label: a CI that
#'        contains 1 does not establish proportionality, and no predefined
#'        equivalence bounds are applied.}
#'      \item{`obs`}{The filtered observation rows used for the plot scatter
#'        overlay.}
#'      \item{`config`}{Named list with `metric_name_var`, `metric_value_var`, `dose_var`,
#'        `ci`, `method`.}
#'    }
#'    Pass directly to [plot_doseprop()] or [plot_build_doseprop()] to replot
#'    without refitting.
#' @export df_doseprop
#'
#' @examples
#' df_doseprop(dplyr::filter(data_sad_nca, PART == "Part 1-SAD"),
#'             metrics = c("aucinf.obs", "cmax"))

df_doseprop <- function(data,
                        metrics,
                        metric_name_var = "PPTESTCD",
                        metric_value_var = "PPORRES",
                        dose_var = "DOSE",
                        method = "normal",
                        ci = 0.9,
                        sigdigits = 3) {

  metric_name_var_str <- resolve_var(rlang::enquo(metric_name_var))
  metric_value_var_str    <- resolve_var(rlang::enquo(metric_value_var))
  dose_var_str   <- resolve_var(rlang::enquo(dose_var))

  check_df(data, "data")
  check_varsindf(data, metric_name_var_str, "data", "metric_name_var")
  check_varsindf(data, metric_value_var_str, "data", "metric_value_var")
  check_varsindf(data, dose_var_str, "data", "dose_var")
  if (!is.character(metrics) || length(metrics) == 0L) {
    pmx_abort("{.arg metrics} is required: a character vector of values in {.var {metric_name_var_str}} to analyze.",
              "bad_arg")
  }
  check_levelsinvar(data, metric_name_var_str, metrics, "metric_name_var", "metrics")
  check_loglog_args(method, ci, sigdigits)

  obs_filtered <- dplyr::filter(data, .data[[metric_name_var_str]] %in% metrics)
  form <- stats::reformulate(
    termlabels = paste0("log(", dose_var_str, ")"),
    response   = paste0("log(", metric_value_var_str, ")")
  )

  call <- rlang::current_env()
  tab_list <- lapply(metrics, function(m) {
    dat_m <- dplyr::filter(obs_filtered, .data[[metric_name_var_str]] == m)
    if (nrow(dat_m) < 2) {
      pmx_abort("Metric {.val {m}} has fewer than 2 rows; cannot fit log-log regression.",
                "bad_data", call = call)
    }
    fit <- stats::lm(form, dat_m)
    tab <- df_loglog(fit, method, ci, sigdigits)
    tab[[metric_name_var_str]] <- m
    tab
  })

  out <- do.call(rbind.data.frame, tab_list)

  pmx_stats(
    stats  = out,
    obs    = obs_filtered,
    config = list(
      metric_name_var = metric_name_var_str,
      metric_value_var    = metric_value_var_str,
      dose_var   = dose_var_str,
      ci         = ci,
      method     = method
    ),
    subclass = "doseprop_stats"
  )
}


#' Validate a `doseprop_stats` object
#'
#' @description
#' Internal helper. Asserts that `x` carries the `doseprop_stats` class,
#' contains the per-metric stats columns, and carries the attributes
#' downstream consumers (notably [plot_build_doseprop()]) depend on. Aborts
#' with a clear message on failure; returns `x` invisibly on success.
#'
#' @param x Object to validate.
#' @param arg,call The argument name and call reported in errors.
#'
#' @return `invisible(x)` on success.
#' @keywords internal

validate_doseprop_stats <- function(x, arg = rlang::caller_arg(x),
                                    call = rlang::caller_env()) {
  validate_pmx_stats(
    x, "doseprop_stats", "df_doseprop",
    stats_cols = c("Intercept", "Power", "LCL", "UCL", "PowerCI"),
    config_keys = c("metric_name_var", "metric_value_var", "dose_var", "ci"),
    require_obs = TRUE,
    arg = arg, call = call
  )
  obs_name <- paste0(arg, "$obs")
  check_varsindf(x$obs, x$config$metric_name_var,  obs_name, "config$metric_name_var", call = call)
  check_varsindf(x$obs, x$config$metric_value_var, obs_name, "config$metric_value_var", call = call)
  check_varsindf(x$obs, x$config$dose_var,         obs_name, "config$dose_var", call = call)
  invisible(x)
}


#' Test whether an object is a `doseprop_stats` container
#'
#' @description
#' Predicate that returns `TRUE` if `x` carries the `"doseprop_stats"` class
#' -- i.e., it is the container returned by [df_doseprop()] (also accepted by
#' [plot_doseprop()] and [plot_build_doseprop()] on the precomputed-stats
#' fast path).
#'
#' @param x Object to test.
#' @param strict Logical. When `TRUE`, additionally runs
#'    `validate_doseprop_stats()` and returns `FALSE` on validation failure.
#'    Default `FALSE` (class-tag check only, cheap).
#'
#' @family dose proportionality
#' @return Logical scalar.
#' @export is_doseprop_stats
#'
#' @examples
#' stats <- df_doseprop(dplyr::filter(data_sad_nca, PART == "Part 1-SAD"),
#'                     metrics = c("aucinf.obs", "cmax"))
#' is_doseprop_stats(stats)             # TRUE
#' is_doseprop_stats(as.data.frame(stats))  # FALSE -- coerced to plain frame

is_doseprop_stats <- function(x, strict = FALSE) {
  ok <- inherits(x, "doseprop_stats")
  if (!ok || !isTRUE(strict)) return(ok)
  tryCatch({validate_doseprop_stats(x); TRUE}, error = function(e) FALSE)
}


#' Print method for `doseprop_stats`
#'
#' @description
#' Focused summary of a [df_doseprop()] result: object dimensions, the
#' regression configuration values (`metric_name_var`, `metric_value_var`, `dose_var`,
#' `ci`, `method`), the number of observation rows attached for the plot
#' scatter overlay, and the per-metric stats body. Inspect the underlying
#' frames directly via `x$stats` and `x$obs`; inspect run config via
#' `x$config`.
#'
#' @param x A `doseprop_stats` object.
#' @param ... Passed to the underlying `print.data.frame()` call for the body.
#'
#' @return `invisible(x)`.
#' @export
#' @method print doseprop_stats

print.doseprop_stats <- function(x, ...) {
  doseprop_stats_header(x)
  cat("\n  stats body:\n")
  print(x$stats, ...)
  cat("\n  Use `x$obs` for the observation overlay.\n")
  invisible(x)
}


doseprop_stats_header <- function(x) {
  pmx_stats_header(x, c("metric_name_var", "metric_value_var", "dose_var",
                        "ci", "method"))
}


#' Summary method for `doseprop_stats`
#'
#' @description
#' Compact summary of a [df_doseprop()] result: the same header and
#' configuration values shown by [print.doseprop_stats()], but the body is
#' condensed to one line per metric using the `PowerCI` and `Interpretation`
#' columns. Suitable for vignette output and test snapshots.
#'
#' @param object A `doseprop_stats` object.
#' @param ... Currently unused.
#'
#' @return `invisible(object)`.
#' @export
#' @method summary doseprop_stats

summary.doseprop_stats <- function(object, ...) {
  metric_name_var_str <- object$config$metric_name_var
  doseprop_stats_header(object)
  cat("\n  per-metric:\n")
  for (i in seq_len(nrow(object$stats))) {
    cat(sprintf("    %s: %s -- %s\n",
                object$stats[[metric_name_var_str]][i],
                object$stats$PowerCI[i],
                object$stats$Interpretation[i]))
  }
  invisible(object)
}


check_loglog_args <- function(method, ci, sigdigits, call = rlang::caller_env()) {
  if (!method %in% c("normal", "tdist")) {
    pmx_abort("{.arg method} must be {.val normal} or {.val tdist}.", "bad_arg", call = call)
  }
  check_quantile_scalar(ci, "ci", call = call)
  check_positive_integer(sigdigits, "sigdigits", call = call)
}
