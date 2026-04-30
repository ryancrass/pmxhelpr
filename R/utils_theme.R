
#' Internal helper: Remove NULL entries from a list
#'
#' @param x A named list potentially containing NULL values.
#'
#' @return A list with all NULL entries removed
#' @keywords internal
#' @examples
#' pmxhelpr:::compact(list(a = 1, b = NULL, c = 3))
#'
compact <- function(x) x[!vapply(x, is.null, logical(1))]


#' Internal helper: Merge user element overrides into a complete default element
#'
#' Iterates over names in the user-supplied element and overwrites matching
#' fields in the default. Warns on unrecognized field names.
#'
#' @param user User-supplied element with partial overrides, or `NULL`.
#' @param default Complete default element.
#'
#' @return A merged element with the same class as `default`
#' @keywords internal
#' @examples
#' defaults <- pmx_point(shape = 1, size = 0.75, alpha = 0.5)
#' pmxhelpr:::merge_element(pmx_point(size = 2), defaults)
#'
merge_element <- function(user, default) {
  if (is.null(user)) return(default)
  out <- default
  for (nm in names(user)) {
    if (!nm %in% names(default)) {
      warning(paste0("`", nm, "` is not a valid field of ", class(default)[1]))
    } else {
      out[[nm]] <- user[[nm]]
    }
  }
  class(out) <- class(default)
  out
}


#' Internal helper: Merge user theme overrides into a complete default theme
#'
#' Iterates over groups in the user-supplied theme and merges each group
#' element-by-element into the default theme using [merge_element()].
#' When a user-supplied entry is a [pmx_style()] object and the default theme
#' contains matching `_point` and `_line` sub-keys, the style fields are
#' applied to both sub-elements.
#'
#' @param user User-supplied theme with partial group overrides, or `NULL`.
#' @param default Complete default theme.
#'
#' @return A merged theme list
#' @keywords internal
#' @examples
#' defaults <- plot_dvtime_theme()
#' pmxhelpr:::merge_theme(list(obs_point = pmx_point(size = 2)), defaults)
#'
merge_theme <- function(user, default) {
  if (is.null(user)) return(default)
  out <- default
  for (nm in names(user)) {
    if (inherits(user[[nm]], "pmx_style")) {
      out <- apply_style(user[[nm]], nm, out)
    } else if (!nm %in% names(default)) {
      warning(paste0("`", nm, "` is not a valid group in the theme"))
    } else {
      out[[nm]] <- merge_element(user[[nm]], out[[nm]])
    }
  }
  out
}


#' Internal helper: Expand a pmx_style into matching point and line sub-keys
#'
#' @param style A `pmx_style` object.
#' @param prefix Character role prefix (e.g., `"obs"`, `"cent"`).
#' @param defaults Named list of theme defaults.
#'
#' @return The modified defaults list with style fields applied.
#' @keywords internal
apply_style <- function(style, prefix, defaults) {
  pt_key <- paste0(prefix, "_point")
  ln_key <- paste0(prefix, "_line")
  for (field in names(style)) {
    if (pt_key %in% names(defaults) && field %in% names(defaults[[pt_key]])) {
      defaults[[pt_key]][[field]] <- style[[field]]
    }
    if (ln_key %in% names(defaults) && field %in% names(defaults[[ln_key]])) {
      defaults[[ln_key]][[field]] <- style[[field]]
    }
  }
  defaults
}


# --- Element Constructors ---

#' Point aesthetics
#'
#' Constructor for point layer aesthetics. Used for observed data points and
#' central tendency points across all plot types.
#'
#' @param shape Point shape. Default varies by plot type.
#' @param size Point size. Default varies by plot type.
#' @param alpha Point alpha. Default varies by plot type.
#' @param color Point color. Default varies by plot type.
#'
#' @return A `pmx_point` element object
#' @export
pmx_point <- function(shape = NULL, size = NULL, alpha = NULL, color = NULL) {
  structure(
    compact(list(shape = shape, size = size, alpha = alpha, color = color)),
    class = "pmx_point"
  )
}


#' Line aesthetics
#'
#' Constructor for line layer aesthetics. Used for trend lines, reference lines,
#' spaghetti lines, and central tendency lines across all plot types.
#'
#' @param linewidth Line width. Default varies by plot type.
#' @param linetype Line type. Default varies by plot type.
#' @param alpha Line alpha. Default varies by plot type.
#' @param color Line color. Default varies by plot type.
#'
#' @return A `pmx_line` element object
#' @export
pmx_line <- function(linewidth = NULL, linetype = NULL, alpha = NULL,
                     color = NULL) {
  structure(
    compact(list(linewidth = linewidth, linetype = linetype,
                 alpha = alpha, color = color)),
    class = "pmx_line"
  )
}


#' Ribbon aesthetics
#'
#' Constructor for ribbon layer aesthetics. Used for simulated prediction
#' interval and median CI ribbons in VPC plots.
#'
#' @param fill Ribbon fill color. Default varies by ribbon type.
#' @param alpha Ribbon alpha. Default varies by ribbon type.
#' @param color Ribbon border color. Default varies by ribbon type.
#' @param linetype Ribbon border line type. Default varies by ribbon type.
#' @param linewidth Ribbon border line width. Default varies by ribbon type.
#'
#' @return A `pmx_ribbon` element object
#' @export
pmx_ribbon <- function(fill = NULL, alpha = NULL, color = NULL,
                       linetype = NULL, linewidth = NULL) {
  structure(
    compact(list(fill = fill, alpha = alpha, color = color,
                 linetype = linetype, linewidth = linewidth)),
    class = "pmx_ribbon"
  )
}


#' Shared style for point and line layers
#'
#' Convenience constructor for setting aesthetics that apply to both point
#' and line elements of a role. Pass to role-level theme arguments (e.g.,
#' `obs`, `cent`, `dv`) to set shared properties without specifying each
#' element individually.
#'
#' @param color Color applied to both point and line elements.
#' @param alpha Alpha applied to both point and line elements.
#'
#' @return A `pmx_style` element object
#' @export
#'
#' @examples
#' plot_gof_theme(pred = pmx_style(color = "purple"))
pmx_style <- function(color = NULL, alpha = NULL) {
  structure(
    compact(list(color = color, alpha = alpha)),
    class = "pmx_style"
  )
}


#' Error bar aesthetics
#'
#' @param linewidth Error bar line width. Default varies by plot type.
#' @param linetype Error bar line type. Default varies by plot type.
#' @param alpha Error bar alpha. Default varies by plot type.
#' @param width Error bar cap width. Default is 2.5 percent of maximum `NTIME`.
#'
#' @return A `pmx_errorbar` element object
#' @export
pmx_errorbar <- function(linewidth = NULL, linetype = NULL,
                         alpha = NULL, width = NULL) {
  structure(
    compact(list(linewidth = linewidth, linetype = linetype,
                 alpha = alpha, width = width)),
    class = "pmx_errorbar"
  )
}


#' Trend line aesthetics (dvconc loess/linear)
#'
#' @param linewidth Line width. Default varies by trend type.
#' @param linetype Line type. Default varies by trend type.
#' @param color Line color. Default is `"black"`.
#' @param se_color Standard error ribbon color. Default is `"lightgrey"`.
#' @param se_alpha Standard error ribbon alpha. Default is 0.4.
#'
#' @return A `pmx_trend` element object
#' @export
pmx_trend <- function(linewidth = NULL, linetype = NULL, color = NULL,
                      se_color = NULL, se_alpha = NULL) {
  structure(
    compact(list(linewidth = linewidth, linetype = linetype, color = color,
                 se_color = se_color, se_alpha = se_alpha)),
    class = "pmx_trend"
  )
}


# --- Theme Constructor-Factories ---

#' Concentration-time plot theme
#'
#' Constructor and factory for `plot_dvtime` plot aesthetics.
#' Call with no arguments to view defaults. Pass element overrides to customize.
#' Use role-level shortcuts `obs` and `cent` with [pmx_style()] to set shared
#' aesthetics (e.g., color, alpha) for both point and line elements at once.
#'
#' @param obs_point Observed data point aesthetics. See [pmx_point()].
#' @param obs_line Observed data line aesthetics (spaghetti). See [pmx_line()].
#' @param cent_point Central tendency point aesthetics. See [pmx_point()].
#' @param cent_line Central tendency line aesthetics. See [pmx_line()].
#' @param errorbar Error bar aesthetics. See [pmx_errorbar()].
#' @param ref Reference line aesthetics (e.g., change-from-baseline). See [pmx_line()].
#' @param loq LOQ reference line aesthetics. See [pmx_line()].
#' @param obs Shortcut: apply shared aesthetics to both `obs_point` and `obs_line`.
#'   See [pmx_style()].
#' @param cent Shortcut: apply shared aesthetics to both `cent_point` and `cent_line`.
#'   See [pmx_style()].
#'
#' @return A named list of theme elements
#' @export
#'
#' @examples
#' plot_dvtime_theme()
#' plot_dvtime_theme(obs_point = pmx_point(size = 2), ref = pmx_line(linetype = 3))
#' plot_dvtime_theme(obs = pmx_style(alpha = 0.3))
plot_dvtime_theme <- function(obs_point = NULL, obs_line = NULL,
                              cent_point = NULL, cent_line = NULL,
                              errorbar = NULL, ref = NULL, loq = NULL,
                              obs = NULL, cent = NULL) {
  defaults <- list(
    obs_point  = pmx_point(shape = 1, size = 0.75, alpha = 0.5),
    obs_line   = pmx_line(linewidth = 0.5, linetype = 1, alpha = 0.5),
    cent_point = pmx_point(shape = 16, size = 1.25, alpha = 1),
    cent_line  = pmx_line(linewidth = 0.75, linetype = 1, alpha = 1),
    errorbar   = pmx_errorbar(linewidth = 0.75, linetype = 1, alpha = 1, width = NULL),
    ref        = pmx_line(linewidth = 0.5, linetype = 2, alpha = 1),
    loq        = pmx_line(linewidth = 0.5, linetype = 2, alpha = 1)
  )
  user <- compact(list(
    obs_point = obs_point, obs_line = obs_line,
    cent_point = cent_point, cent_line = cent_line,
    errorbar = errorbar, ref = ref, loq = loq,
    obs = obs, cent = cent
  ))
  merge_theme(user, defaults)
}


#' Population overlay GOF plot theme
#'
#' Constructor and factory for `plot_gof` plot aesthetics.
#' Call with no arguments to view defaults. Pass element overrides to customize.
#' Use role-level shortcuts `obs`, `dv`, `pred`, `ipred` with [pmx_style()] to
#' set shared aesthetics (e.g., color) for both point and line elements at once.
#'
#' @param obs_point Observed data point aesthetics. See [pmx_point()].
#' @param obs_line Observed data line aesthetics. See [pmx_line()].
#' @param dv_point DV central tendency point aesthetics. See [pmx_point()].
#' @param dv_line DV central tendency line aesthetics. See [pmx_line()].
#' @param pred_point PRED central tendency point aesthetics. See [pmx_point()].
#' @param pred_line PRED central tendency line aesthetics. See [pmx_line()].
#' @param ipred_point IPRED central tendency point aesthetics. See [pmx_point()].
#' @param ipred_line IPRED central tendency line aesthetics. See [pmx_line()].
#' @param errorbar Error bar aesthetics. See [pmx_errorbar()].
#' @param ref Reference line aesthetics. See [pmx_line()].
#' @param loq LOQ reference line aesthetics. See [pmx_line()].
#' @param obs Shortcut: apply shared aesthetics to both `obs_point` and `obs_line`.
#'   See [pmx_style()].
#' @param dv Shortcut: apply shared aesthetics to both `dv_point` and `dv_line`.
#'   See [pmx_style()].
#' @param pred Shortcut: apply shared aesthetics to both `pred_point` and `pred_line`.
#'   See [pmx_style()].
#' @param ipred Shortcut: apply shared aesthetics to both `ipred_point` and `ipred_line`.
#'   See [pmx_style()].
#'
#' @return A named list of theme elements
#' @export
#'
#' @examples
#' plot_gof_theme()
#' plot_gof_theme(pred = pmx_style(color = "purple"))
#' plot_gof_theme(pred_point = pmx_point(size = 3))
plot_gof_theme <- function(obs_point = NULL, obs_line = NULL,
                              dv_point = NULL, dv_line = NULL,
                              pred_point = NULL, pred_line = NULL,
                              ipred_point = NULL, ipred_line = NULL,
                              errorbar = NULL, ref = NULL, loq = NULL,
                              obs = NULL, dv = NULL,
                              pred = NULL, ipred = NULL) {
  defaults <- list(
    obs_point   = pmx_point(shape = 1, size = 0.75, alpha = 0.5, color = "darkgrey"),
    obs_line    = pmx_line(linewidth = 0.5, linetype = 1, alpha = 0.75, color = "darkgrey"),
    dv_point    = pmx_point(shape = 1, size = 1.25, alpha = 1, color = "blue"),
    dv_line     = pmx_line(linewidth = 0.75, linetype = 1, alpha = 1, color = "blue"),
    pred_point  = pmx_point(shape = 1, size = 1.25, alpha = 1, color = "red"),
    pred_line   = pmx_line(linewidth = 0.75, linetype = 1, alpha = 1, color = "red"),
    ipred_point = pmx_point(shape = 1, size = 1.25, alpha = 1, color = "green"),
    ipred_line  = pmx_line(linewidth = 0.75, linetype = 1, alpha = 1, color = "green"),
    errorbar    = pmx_errorbar(linewidth = 0.75, linetype = 1, alpha = 1, width = NULL),
    ref         = pmx_line(linewidth = 0.5, linetype = 2, alpha = 1),
    loq         = pmx_line(linewidth = 0.5, linetype = 2, alpha = 1)
  )
  user <- compact(list(
    obs_point = obs_point, obs_line = obs_line,
    dv_point = dv_point, dv_line = dv_line,
    pred_point = pred_point, pred_line = pred_line,
    ipred_point = ipred_point, ipred_line = ipred_line,
    errorbar = errorbar, ref = ref, loq = loq,
    obs = obs, dv = dv, pred = pred, ipred = ipred
  ))
  merge_theme(user, defaults)
}


#' Response versus concentration plot theme
#'
#' Constructor and factory for `plot_dvconc` plot aesthetics.
#' Call with no arguments to view defaults. Pass element overrides to customize.
#'
#' @param obs Observed data point aesthetics. See [pmx_point()].
#' @param ref Reference line aesthetics. See [pmx_line()].
#' @param loess LOESS trend line aesthetics. See [pmx_trend()].
#' @param linear Linear trend line aesthetics. See [pmx_trend()].
#'
#' @return A named list of theme elements
#' @export
#'
#' @examples
#' plot_dvconc_theme()
#' plot_dvconc_theme(loess = pmx_trend(color = "red"))
plot_dvconc_theme <- function(obs = NULL, ref = NULL, loess = NULL, linear = NULL) {
  defaults <- list(
    obs    = pmx_point(shape = 1, size = 1.25, alpha = 0.5),
    ref    = pmx_line(linewidth = 0.5, linetype = 2, alpha = 1),
    loess  = pmx_trend(linewidth = 1, linetype = 1,
                       color = "black", se_color = "lightgrey", se_alpha = 0.4),
    linear = pmx_trend(linewidth = 1, linetype = 2,
                       color = "black", se_color = "lightgrey", se_alpha = 0.4)
  )
  merge_theme(compact(list(obs = obs, ref = ref, loess = loess, linear = linear)),
              defaults)
}


#' VPC plot theme
#'
#' Constructor and factory for `plot_vpc_cont` plot aesthetics.
#' Call with no arguments to view defaults. Pass element overrides to customize.
#'
#' @param obs Observed data point aesthetics. See [pmx_point()].
#' @param obs_median Observed median line aesthetics. See [pmx_line()].
#' @param obs_ci Observed CI line aesthetics. See [pmx_line()].
#' @param sim_pi Simulated prediction interval ribbon aesthetics. See [pmx_ribbon()].
#' @param sim_median Simulated median ribbon aesthetics. See [pmx_ribbon()].
#' @param loq LOQ reference line aesthetics. See [pmx_line()].
#' @param bin_sep Bin separator aesthetics. See [pmx_line()].
#'
#' @return A named list of theme elements
#' @export
#'
#' @examples
#' plot_vpc_theme()
#' plot_vpc_theme(obs = pmx_point(color = "#000000"))
plot_vpc_theme <- function(obs = NULL, obs_median = NULL, obs_ci = NULL,
                          sim_pi = NULL, sim_median = NULL,
                          loq = NULL, bin_sep = NULL) {
  defaults <- list(
    obs        = pmx_point(color = "#0000FF", size = 1, shape = 1, alpha = 0.7),
    obs_median = pmx_line(color = "#FF0000", linetype = "solid", linewidth = 1),
    obs_ci     = pmx_line(color = "#0000FF", linetype = "dashed", linewidth = 0.5),
    sim_pi     = pmx_ribbon(fill = "#0000FF", alpha = 0.15,
                            color = "#000000", linetype = "dotted", linewidth = 1),
    sim_median = pmx_ribbon(fill = "#FF0000", alpha = 0.3,
                            color = "#000000", linetype = "dashed", linewidth = 1),
    loq        = pmx_line(color = "#990000", linetype = "dashed", linewidth = 0.5),
    bin_sep    = pmx_line(color = "#000000")
  )
  merge_theme(compact(list(obs = obs, obs_median = obs_median, obs_ci = obs_ci,
                           sim_pi = sim_pi, sim_median = sim_median,
                           loq = loq, bin_sep = bin_sep)),
              defaults)
}


#' VPC layer visibility settings
#'
#' Constructor and factory for controlling which VPC layers are displayed.
#' Call with no arguments to view defaults. Pass overrides to customize.
#'
#' @param obs_dv Show observed data points. Default is `TRUE`.
#' @param obs_ci Show observed quantile lines. Default is `TRUE`.
#' @param obs_median Show observed median line. Default is `TRUE`.
#' @param sim_median Show simulated median line. Default is `FALSE`.
#' @param sim_median_ci Show simulated median confidence interval. Default is `TRUE`.
#' @param pi Show prediction interval lines. Default is `FALSE`.
#' @param pi_ci Show prediction interval confidence intervals. Default is `TRUE`.
#' @param pi_as_area Show prediction interval as shaded area. Default is `FALSE`.
#'
#' @return A named list of logicals
#' @export
#'
#' @examples
#' plot_vpc_shown()
#' plot_vpc_shown(obs_dv = FALSE, pi = TRUE)
plot_vpc_shown <- function(obs_dv = NULL, obs_ci = NULL, obs_median = NULL,
                           sim_median = NULL, sim_median_ci = NULL,
                           pi = NULL, pi_ci = NULL, pi_as_area = NULL) {
  defaults <- list(
    obs_dv = TRUE, obs_ci = TRUE, obs_median = TRUE,
    sim_median = FALSE, sim_median_ci = TRUE,
    pi = FALSE, pi_ci = TRUE, pi_as_area = FALSE
  )
  user <- compact(list(obs_dv = obs_dv, obs_ci = obs_ci, obs_median = obs_median,
                       sim_median = sim_median, sim_median_ci = sim_median_ci,
                       pi = pi, pi_ci = pi_ci, pi_as_area = pi_as_area))
  out <- defaults
  for (nm in names(user)) out[[nm]] <- user[[nm]]
  out
}
