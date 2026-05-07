
#' Perform a log-log Regression
#'
#' @param data Input dataset for log-log regression.
#'    Default expected format is output from `PKNCA::pk.nca()` (i.e., SDTM PP formatting)
#' @param exp_var Column in `data` containing the exposure metric (dependent variable).
#'    Accepts bare names or strings. Default is `PPORRES`.
#' @param dose_var Column in `data` containing the dose (independent variable).
#'    Accepts bare names or strings. Default is `DOSE`.
#'
#' @family dose proportionality
#' @return `lm` object
#' @export mod_loglog
#'
#' @examples
#' mod_auc <- mod_loglog(dplyr::filter(data_sad_nca, PPTESTCD == "aucinf.obs"))
#' summary(mod_auc)
#'
#' mod_cmax <- mod_loglog(dplyr::filter(data_sad_nca, PPTESTCD == "cmax"))
#' summary(mod_cmax)
#'
mod_loglog <- function(data,
                       exp_var = PPORRES,
                       dose_var = DOSE) {

  exp_var_str  <- resolve_var(rlang::enquo(exp_var))
  dose_var_str <- resolve_var(rlang::enquo(dose_var))

  check_df(data, "data")
  check_varsindf(data, exp_var_str, "data", "exp_var")
  check_varsindf(data, dose_var_str, "data", "dose_var")
  if(nrow(data) < 2) {rlang::abort(message = "argument `data` must contain at least 2 rows for log-log regression")}

  form <- stats::as.formula(paste(paste0("log(",exp_var_str,")"),"~",paste0("log(",dose_var_str,")")))
  fit <- stats::lm(form, data)
  return(fit)

}


#' Compute estimate table for log-log regression
#'
#' @param fit `lm` model object for the log-log regression
#' @param method character string specifying the distribution to be used to derived the confidence interval.
#'    Options are "normal" (default) and "tdist"
#' @param ci confidence interval to be calculated.
#'    Options 0.90 (default) and 0.95
#' @param sigdigits number of significant digits for rounding
#'
#' @family dose proportionality
#' @return `data.frame`
#' @export df_loglog
#'
#' @examples
#' mod_auc <- mod_loglog(dplyr::filter(data_sad_nca, PPTESTCD == "aucinf.obs"))
#' df_loglog(mod_auc)
#'
#' mod_cmax <- mod_loglog(dplyr::filter(data_sad_nca, PPTESTCD == "cmax"))
#' df_loglog(mod_cmax)

df_loglog <- function(fit,
                      method = "normal",
                      ci = 0.9,
                      sigdigits = 3) {

  check_lm(fit, "fit")
  check_loglog_args(method, ci, sigdigits)

  int <- stats::coef(fit)[[1]]
  est <- stats::coef(fit)[[2]]
  se <- sqrt(diag(stats::vcov(fit)))[[2]]
  z <- stats::qt((1 + ci)/2, (length(fit$residuals)-1))

  tab <- data.frame(
    Intercept = signif(int, digits=sigdigits),
    StandardError = signif(se, digits=sigdigits),
    CI = paste0(ci*100, "%"),
    Power = signif(est, digits = sigdigits),
    LCL = dplyr::case_when(method == "normal" ~ signif(est - stats::qnorm((1 + ci)/2)*se, digits=sigdigits),
                           method == "tdist" ~ signif(est - z*se, digits=sigdigits),
                           .default = NA_real_),
    UCL = dplyr::case_when(method == "normal" ~ signif(est + stats::qnorm((1 + ci)/2)*se, digits=sigdigits),
                           method == "tdist" ~ signif(est + z*se, digits=sigdigits),
                           .default = NA_real_)) |>
    dplyr::mutate(Proportional = dplyr::case_when(LCL < 1 & UCL < 1 ~ FALSE,
                                        LCL > 1 & UCL > 1 ~ FALSE,
                                        .default = TRUE),
                  PowerCI = paste0("Power: ", Power, " (", CI," CI ",LCL ,"-",UCL,")"),
                  Interpretation = dplyr::case_when(LCL < 1 & UCL < 1 ~ "Less than dose-proportional",
                                      LCL > 1 & UCL > 1 ~ "Greater than dose-proportional",
                                      .default = "Dose-proportional"))

  return(tab)
}


#' Compute and tabulate estimates for log-log regression
#'
#' @description
#' Computes per-metric log-log regression statistics and returns them in a
#' cacheable, replottable container. The returned object is a
#' [pmx_stats()] container with class
#' `c("doseprop_stats", "pmx_stats")` and three slots — `stats` (per-metric
#' regression body), `obs` (filtered observation rows used for the scatter
#' overlay), and `config` (regression configuration: `metric_var`, `exp_var`,
#' `dose_var`, `ci`, `method`) — so that [plot_doseprop()] /
#' [plot_build_doseprop()] can render directly from this object without
#' re-fitting any regressions.
#'
#' @param metrics character vector of exposure metrics in `data` to plot
#' @param metric_var Column in `data` containing the values provided in `metrics`.
#'    Accepts bare names or strings. Default is `PPTESTCD`.
#' @inheritParams mod_loglog
#' @inheritParams df_loglog
#'
#' @family dose proportionality
#' @return A `doseprop_stats` container (subclass of `pmx_stats`) with three
#'    slots:
#'    \describe{
#'      \item{`stats`}{One row per metric and columns `Intercept`,
#'        `StandardError`, `CI`, `Power`, `LCL`, `UCL`, `Proportional`,
#'        `PowerCI`, `Interpretation`, plus the column named in `metric_var`.}
#'      \item{`obs`}{The filtered observation rows used for the plot scatter
#'        overlay.}
#'      \item{`config`}{Named list with `metric_var`, `exp_var`, `dose_var`,
#'        `ci`, `method`.}
#'    }
#'    Pass directly to [plot_doseprop()] or [plot_build_doseprop()] to replot
#'    without refitting.
#' @export df_doseprop
#'
#' @examples
#' df_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"))

df_doseprop <- function(data,
                        metrics,
                        metric_var = PPTESTCD,
                        exp_var = PPORRES,
                        dose_var = DOSE,
                        method = "normal",
                        ci = 0.90,
                        sigdigits=3) {

  metric_var_str <- resolve_var(rlang::enquo(metric_var))
  exp_var_str    <- resolve_var(rlang::enquo(exp_var))
  dose_var_str   <- resolve_var(rlang::enquo(dose_var))

  check_df(data, "data")
  check_varsindf(data, metric_var_str, "data", "metric_var")
  check_varsindf(data, exp_var_str, "data", "exp_var")
  check_varsindf(data, dose_var_str, "data", "dose_var")
  check_levelsinvar(data, metric_var_str, metrics, "metric_var", "metrics")
  check_loglog_args(method, ci, sigdigits)

  tab_list <- lapply(metrics, function(m) {
    dat_m <- dplyr::filter(data, .data[[metric_var_str]] == m)
    fit   <- mod_loglog(dat_m, exp_var = exp_var_str, dose_var = dose_var_str)
    tab   <- df_loglog(fit, method, ci, sigdigits)
    tab[[metric_var_str]] <- m
    tab
  })

  out <- do.call(rbind.data.frame, tab_list)

  obs_filtered <- dplyr::filter(data, .data[[metric_var_str]] %in% metrics)

  pmx_stats(
    stats  = out,
    obs    = obs_filtered,
    config = list(
      metric_var = metric_var_str,
      exp_var    = exp_var_str,
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
#'
#' @return `invisible(x)` on success.
#' @keywords internal

validate_doseprop_stats <- function(x) {
  if (!inherits(x, "doseprop_stats")) {
    rlang::abort("`x` must be a `doseprop_stats` object (output of `df_doseprop()`).")
  }
  validate_pmx_stats(x)
  required_cols <- c("Intercept", "Power", "LCL", "UCL", "PowerCI")
  missing_cols <- setdiff(required_cols, colnames(x$stats))
  if (length(missing_cols) > 0) {
    rlang::abort(paste0("`doseprop_stats` is missing required columns: ",
                        paste(missing_cols, collapse = ", ")))
  }
  required_config <- c("metric_var", "exp_var", "dose_var", "ci")
  missing_config <- setdiff(required_config, names(x$config))
  if (length(missing_config) > 0) {
    rlang::abort(paste0("`doseprop_stats` is missing required config keys: ",
                        paste(missing_config, collapse = ", ")))
  }
  if (!is.data.frame(x$obs)) {
    rlang::abort("`doseprop_stats$obs` must be a data.frame.")
  }
  invisible(x)
}



#' Internal helper: snap a positive range outward to half-decade boundaries
#'
#' Returns `c(lower, upper)` where each end lands on the nearest value of the
#' form `10^k` or `5 * 10^k` (integer `k`) that strictly brackets the input
#' range. Used to set tight log10 axis limits in `plot_build_doseprop`.
#'
#' @param x Length-2 numeric vector `c(min, max)` of positive values.
#' @return Length-2 numeric vector `c(lower, upper)` with `lower < x[1]` and
#'    `upper > x[2]`, each on a half-decade grid position.
#' @keywords internal
#' @noRd
half_decade_bracket <- function(x) {
  klo <- floor(log10(x[1])); blo <- 10^klo
  khi <- floor(log10(x[2])); bhi <- 10^khi
  lo <- if (x[1] > 5 * blo) 5 * blo
        else if (x[1] > blo) blo
        else 5 * 10^(klo - 1)
  hi <- if (x[2] < 5 * bhi) 5 * bhi
        else if (x[2] < 10 * bhi) 10 * bhi
        else 5 * 10^(khi + 1)
  c(lo, hi)
}


#' Build a dose-proportionality ggplot from a `doseprop_stats` object
#'
#' @description
#' Constructs a log-log regression scatter plot from a [df_doseprop()] result
#' (or any object satisfying the `doseprop_stats` contract). Recovers the
#' observation rows and column names from the object's attributes, builds the
#' faceting label from the per-metric `PowerCI` text, and renders the scatter
#' + trend layers.
#'
#' Most users will reach this function indirectly via [plot_doseprop()].
#' Call `plot_build_doseprop()` directly when working from a manually-
#' constructed or cached `doseprop_stats` object — for example, plotting a
#' precomputed result from disk or a custom pipeline that produces compatible
#' columns and attributes.
#'
#' @param stats A `doseprop_stats` object (typically the output of
#'    [df_doseprop()]). Must contain `$obs` and `$config` slots with
#'    `metric_var`, `exp_var`, `dose_var`, and `ci`. Validated by
#'    `validate_doseprop_stats()` at entry.
#' @param theme Named list of aesthetic parameters for the plot created by
#'    [plot_doseprop_theme()]. Defaults can be viewed by running
#'    `plot_doseprop_theme()` with no arguments.
#' @param se logical to display confidence interval around regression. Default
#'    is `TRUE`.
#'
#' @family dose proportionality
#' @return a `ggplot` plot object
#' @export plot_build_doseprop
#'
#' @examples
#' stats <- df_doseprop(dplyr::filter(data_sad_nca, PART == "Part 1-SAD"),
#'                       metrics = c("aucinf.obs", "cmax"))
#' plot_build_doseprop(stats)
#' plot_build_doseprop(stats, se = FALSE)

plot_build_doseprop <- function(stats,
                                theme = NULL,
                                se = TRUE) {

  validate_doseprop_stats(stats)

  metric_var_str <- stats$config$metric_var
  exp_var_str    <- stats$config$exp_var
  dose_var_str   <- stats$config$dose_var
  ci             <- stats$config$ci
  obs            <- stats$obs

  plottheme <- merge_theme(theme, plot_doseprop_theme())

  tab <- stats$stats
  tab$label <- paste0(tab[[metric_var_str]], " | ", tab$PowerCI)

  plot_data <- dplyr::left_join(obs, tab, by = metric_var_str)

  base <- init_plot(plot_data, dose_var_str, exp_var_str)

  plot <- add_obs_layers(base, id_var_str = NULL,
                         point_el = plottheme$obs_point,
                         line_el = NULL)
  plot <- add_trend_layers(plot, method = "lm", show = TRUE, se = se,
                           plottheme = plottheme,
                           col_var_str = NULL, col_trend = FALSE,
                           formula = y ~ x, level = ci,
                           theme_key = "linear")
  plot +
    ggplot2::labs(x = "Dose", y = "Exposure") +
    ggplot2::scale_x_log10(
      guide  = "axis_logticks",
      limits = half_decade_bracket,
      expand = ggplot2::expansion(mult = 0)
    ) +
    ggplot2::scale_y_log10(
      guide  = "axis_logticks",
      limits = half_decade_bracket,
      expand = ggplot2::expansion(mult = 0)
    ) +
    ggplot2::facet_wrap(~label, scales = "free")
}


#' Plot a dose-proportionality assessment via power law (log-log) regression
#'
#' @description
#' Dual-mode wrapper that delegates to [df_doseprop()] for computation and
#' [plot_build_doseprop()] for rendering. Accepts either:
#'
#' * raw observation data (e.g. PKNCA output) plus a `metrics` vector — the
#'   common one-shot mode; or
#' * a precomputed `doseprop_stats` object returned by [df_doseprop()] — skip
#'   the regression refit and replot with different `theme` / `se` settings.
#'
#' On the precomputed path, pipeline arguments (`metrics`, `metric_var`,
#' `exp_var`, `dose_var`, `method`, `ci`, `sigdigits`) cannot be honored
#' because the regression does not run again — passing any of them aborts
#' with a message pointing the caller at [df_doseprop()]. Only `theme` and
#' `se` are accepted on both paths.
#'
#' @param data Either raw observation data (data.frame, default expected
#'    format is output from `PKNCA::pk.nca()`) or a `doseprop_stats` object
#'    returned by [df_doseprop()].
#' @param metrics character vector of exposure metrics in `data` to plot.
#'    Required on the raw-data path; ignored when `data` is a
#'    `doseprop_stats` object.
#' @param se logical to display confidence interval around regression.
#'    Default is `TRUE`.
#' @param theme Named list of aesthetic parameters for the plot created by
#'    [plot_doseprop_theme()]. Defaults can be viewed by running
#'    `plot_doseprop_theme()` with no arguments.
#' @inheritParams mod_loglog
#' @inheritParams df_loglog
#' @inheritParams df_doseprop
#'
#' @family dose proportionality
#' @return a `ggplot` plot object
#' @export plot_doseprop
#'
#' @examples
#' # Raw-data path
#' plot_doseprop(dplyr::filter(data_sad_nca, PART == "Part 1-SAD"),
#'                metrics = c("aucinf.obs", "cmax"))
#'
#' # Precomputed path: compute once, replot many times
#' stats <- df_doseprop(dplyr::filter(data_sad_nca, PART == "Part 1-SAD"),
#'                       metrics = c("aucinf.obs", "cmax"))
#' plot_doseprop(stats)
#' plot_doseprop(stats, se = FALSE)

plot_doseprop <- function(data,
                          metrics = NULL,
                          metric_var = PPTESTCD,
                          exp_var = PPORRES,
                          dose_var = DOSE,
                          method = "normal",
                          ci = 0.90,
                          sigdigits = 3,
                          se = TRUE,
                          theme = NULL) {

  if (inherits(data, "doseprop_stats")) {
    check_pipeline_args_dropped(
      call           = match.call(),
      plot_only_args = c("data", "theme", "se"),
      fn_name        = "plot_doseprop"
    )
    return(plot_build_doseprop(data, theme = theme, se = se))
  }

  metric_var_str <- resolve_var(rlang::enquo(metric_var))
  exp_var_str    <- resolve_var(rlang::enquo(exp_var))
  dose_var_str   <- resolve_var(rlang::enquo(dose_var))

  check_df(data, "data")
  check_varsindf(data, metric_var_str, "data", "metric_var")
  check_varsindf(data, exp_var_str, "data", "exp_var")
  check_varsindf(data, dose_var_str, "data", "dose_var")
  check_levelsinvar(data, metric_var_str, metrics, "metric_var", "metrics")
  check_loglog_args(method, ci, sigdigits)

  stats <- df_doseprop(data, metrics,
                       metric_var = metric_var_str,
                       exp_var = exp_var_str,
                       dose_var = dose_var_str,
                       method = method, ci = ci, sigdigits = sigdigits)

  plot_build_doseprop(stats, theme = theme, se = se)
}
