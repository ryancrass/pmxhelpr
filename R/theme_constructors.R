
# Internal helper: remove NULL entries from a list
compact <- function(x) x[!vapply(x, is.null, logical(1))]


# Internal helper: merge user element overrides into a complete default element
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


# Internal helper: merge user theme overrides into a complete default theme
merge_theme <- function(user, default) {
  if (is.null(user)) return(default)
  out <- default
  for (nm in names(user)) {
    if (!nm %in% names(default)) {
      warning(paste0("`", nm, "` is not a valid group in the theme"))
    } else {
      out[[nm]] <- merge_element(user[[nm]], default[[nm]])
    }
  }
  out
}


# --- Element Constructors: dvtime / popgof / dvconc family ---

#' Observed data point and line aesthetics
#'
#' @param shape Point shape. Default varies by plot type.
#' @param size Point size. Default varies by plot type.
#' @param alpha Point alpha. Default varies by plot type.
#' @param linewidth Spaghetti line width. Default varies by plot type.
#' @param linetype Spaghetti line type. Default varies by plot type.
#' @param line_alpha Spaghetti line alpha. Default varies by plot type.
#'
#' @return A `pmx_obs` element object
#' @export
pmx_obs <- function(shape = NULL, size = NULL, alpha = NULL,
                    linewidth = NULL, linetype = NULL, line_alpha = NULL) {
  structure(
    compact(list(shape = shape, size = size, alpha = alpha,
                 linewidth = linewidth, linetype = linetype, line_alpha = line_alpha)),
    class = "pmx_obs"
  )
}


#' Reference line aesthetics
#'
#' @param linewidth Line width. Default varies by plot type.
#' @param linetype Line type. Default varies by plot type.
#' @param alpha Line alpha. Default varies by plot type.
#'
#' @return A `pmx_ref` element object
#' @export
pmx_ref <- function(linewidth = NULL, linetype = NULL, alpha = NULL) {
  structure(
    compact(list(linewidth = linewidth, linetype = linetype, alpha = alpha)),
    class = "pmx_ref"
  )
}


#' Central tendency point and line aesthetics
#'
#' @param shape Point shape. Default varies by plot type.
#' @param size Point size. Default varies by plot type.
#' @param alpha Point alpha. Default varies by plot type.
#' @param linewidth Line width. Default varies by plot type.
#' @param linetype Line type. Default varies by plot type.
#' @param line_alpha Line alpha. Default varies by plot type.
#'
#' @return A `pmx_cent` element object
#' @export
pmx_cent <- function(shape = NULL, size = NULL, alpha = NULL,
                     linewidth = NULL, linetype = NULL, line_alpha = NULL) {
  structure(
    compact(list(shape = shape, size = size, alpha = alpha,
                 linewidth = linewidth, linetype = linetype, line_alpha = line_alpha)),
    class = "pmx_cent"
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


#' DV overlay line aesthetics (popgof only)
#'
#' @param linewidth Line width. Default is 1.
#' @param linetype Line type. Default is 1.
#' @param line_alpha Line alpha. Default is 1.
#'
#' @return A `pmx_dv_line` element object
#' @export
pmx_dv_line <- function(linewidth = NULL, linetype = NULL, line_alpha = NULL) {
  structure(
    compact(list(linewidth = linewidth, linetype = linetype, line_alpha = line_alpha)),
    class = "pmx_dv_line"
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


# --- Element Constructors: VPC family ---

#' VPC observed data point aesthetics
#'
#' @param color Point color. Default is `"#0000FF"`.
#' @param size Point size. Default is 1.
#' @param shape Point shape. Default is 1.
#' @param alpha Point alpha. Default is 0.7.
#'
#' @return A `pmx_vpc_point` element object
#' @export
pmx_vpc_point <- function(color = NULL, size = NULL, shape = NULL, alpha = NULL) {
  structure(
    compact(list(color = color, size = size, shape = shape, alpha = alpha)),
    class = "pmx_vpc_point"
  )
}


#' VPC line aesthetics
#'
#' Used for observed median and CI lines.
#'
#' @param color Line color. Default varies by line type.
#' @param linetype Line type. Default varies by line type.
#' @param size Line size. Default varies by line type.
#'
#' @return A `pmx_vpc_line` element object
#' @export
pmx_vpc_line <- function(color = NULL, linetype = NULL, size = NULL) {
  structure(
    compact(list(color = color, linetype = linetype, size = size)),
    class = "pmx_vpc_line"
  )
}


#' VPC ribbon aesthetics
#'
#' Used for simulated prediction interval and median CI ribbons.
#'
#' @param fill Ribbon fill color. Default varies by ribbon type.
#' @param alpha Ribbon alpha. Default varies by ribbon type.
#' @param color Ribbon border color. Default varies by ribbon type.
#' @param linetype Ribbon border line type. Default varies by ribbon type.
#' @param size Ribbon border size. Default varies by ribbon type.
#'
#' @return A `pmx_vpc_ribbon` element object
#' @export
pmx_vpc_ribbon <- function(fill = NULL, alpha = NULL, color = NULL,
                           linetype = NULL, size = NULL) {
  structure(
    compact(list(fill = fill, alpha = alpha, color = color,
                 linetype = linetype, size = size)),
    class = "pmx_vpc_ribbon"
  )
}


#' VPC LOQ reference line aesthetics
#'
#' @param color Line color. Default is `"#990000"`.
#' @param linetype Line type. Default is `"dashed"`.
#'
#' @return A `pmx_vpc_loq` element object
#' @export
pmx_vpc_loq <- function(color = NULL, linetype = NULL) {
  structure(
    compact(list(color = color, linetype = linetype)),
    class = "pmx_vpc_loq"
  )
}


#' VPC bin separator aesthetics
#'
#' @param color Line color. Default is `"#000000"`.
#'
#' @return A `pmx_vpc_bin_sep` element object
#' @export
pmx_vpc_bin_sep <- function(color = NULL) {
  structure(
    compact(list(color = color)),
    class = "pmx_vpc_bin_sep"
  )
}


# --- Theme Constructor-Factories ---

#' Concentration-time plot theme
#'
#' Constructor and factory for `plot_dvtime` plot aesthetics.
#' Call with no arguments to view defaults. Pass element overrides to customize.
#'
#' @param obs Observed data aesthetics. See [pmx_obs()].
#' @param ref Reference line aesthetics. See [pmx_ref()].
#' @param cent Central tendency aesthetics. See [pmx_cent()].
#' @param errorbar Error bar aesthetics. See [pmx_errorbar()].
#'
#' @return A named list of theme elements
#' @export
#'
#' @examples
#' plot_dvtime_theme()
#' plot_dvtime_theme(obs = pmx_obs(size = 2), ref = pmx_ref(linetype = 3))
plot_dvtime_theme <- function(obs = NULL, ref = NULL, cent = NULL, errorbar = NULL) {
  defaults <- list(
    obs      = pmx_obs(shape = 1, size = 0.75, alpha = 0.5,
                       linewidth = 0.5, linetype = 1, line_alpha = 0.5),
    ref      = pmx_ref(linewidth = 0.5, linetype = 2, alpha = 1),
    cent     = pmx_cent(shape = 16, size = 1.25, alpha = 1,
                        linewidth = 0.75, linetype = 1, line_alpha = 1),
    errorbar = pmx_errorbar(linewidth = 0.75, linetype = 1, alpha = 1, width = NULL)
  )
  merge_theme(compact(list(obs = obs, ref = ref, cent = cent, errorbar = errorbar)),
              defaults)
}


#' Population overlay GOF plot theme
#'
#' Constructor and factory for `plot_popgof` plot aesthetics.
#' Call with no arguments to view defaults. Pass element overrides to customize.
#'
#' @inheritParams plot_dvtime_theme
#' @param dv_line DV overlay line aesthetics. See [pmx_dv_line()].
#'
#' @return A named list of theme elements
#' @export
#'
#' @examples
#' plot_popgof_theme()
#' plot_popgof_theme(obs = pmx_obs(size = 2))
plot_popgof_theme <- function(obs = NULL, ref = NULL, cent = NULL,
                             errorbar = NULL, dv_line = NULL) {
  defaults <- list(
    obs      = pmx_obs(shape = 1, size = 0.75, alpha = 0.5,
                       linewidth = 0.5, linetype = 1, line_alpha = 0.75),
    ref      = pmx_ref(linewidth = 0.5, linetype = 2, alpha = 1),
    cent     = pmx_cent(shape = 1, size = 1.25, alpha = 1,
                        linewidth = 0.75, linetype = 1, line_alpha = 1),
    errorbar = pmx_errorbar(linewidth = 0.75, linetype = 1, alpha = 1, width = NULL),
    dv_line  = pmx_dv_line(linewidth = 1, linetype = 1, line_alpha = 1)
  )
  merge_theme(compact(list(obs = obs, ref = ref, cent = cent,
                           errorbar = errorbar, dv_line = dv_line)),
              defaults)
}


#' Response versus concentration plot theme
#'
#' Constructor and factory for `plot_dvconc` plot aesthetics.
#' Call with no arguments to view defaults. Pass element overrides to customize.
#'
#' @param obs Observed data aesthetics. See [pmx_obs()].
#' @param ref Reference line aesthetics. See [pmx_ref()].
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
    obs    = pmx_obs(shape = 1, size = 1.25, alpha = 0.5),
    ref    = pmx_ref(linewidth = 0.5, linetype = 2, alpha = 1),
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
#' Constructor and factory for `plot_vpc_exactbins` plot aesthetics.
#' Call with no arguments to view defaults. Pass element overrides to customize.
#'
#' @param obs Observed data point aesthetics. See [pmx_vpc_point()].
#' @param obs_median Observed median line aesthetics. See [pmx_vpc_line()].
#' @param obs_ci Observed CI line aesthetics. See [pmx_vpc_line()].
#' @param sim_pi Simulated prediction interval ribbon aesthetics. See [pmx_vpc_ribbon()].
#' @param sim_median Simulated median ribbon aesthetics. See [pmx_vpc_ribbon()].
#' @param loq LOQ reference line aesthetics. See [pmx_vpc_loq()].
#' @param bin_sep Bin separator aesthetics. See [pmx_vpc_bin_sep()].
#'
#' @return A named list of theme elements
#' @export
#'
#' @examples
#' plot_vpc_theme()
#' plot_vpc_theme(obs = pmx_vpc_point(color = "#000000"))
plot_vpc_theme <- function(obs = NULL, obs_median = NULL, obs_ci = NULL,
                          sim_pi = NULL, sim_median = NULL,
                          loq = NULL, bin_sep = NULL) {
  defaults <- list(
    obs        = pmx_vpc_point(color = "#0000FF", size = 1, shape = 1, alpha = 0.7),
    obs_median = pmx_vpc_line(color = "#FF0000", linetype = "solid", size = 1),
    obs_ci     = pmx_vpc_line(color = "#0000FF", linetype = "dashed", size = 0.5),
    sim_pi     = pmx_vpc_ribbon(fill = "#0000FF", alpha = 0.15,
                                color = "#000000", linetype = "dotted", size = 1),
    sim_median = pmx_vpc_ribbon(fill = "#FF0000", alpha = 0.3,
                                color = "#000000", linetype = "dashed", size = 1),
    loq        = pmx_vpc_loq(color = "#990000", linetype = "dashed"),
    bin_sep    = pmx_vpc_bin_sep(color = "#000000")
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
