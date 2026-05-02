
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
  cls <- class(default)[1]
  valid <- element_fields(cls)
  if (length(valid) == 0) valid <- names(default)
  out <- default
  for (nm in names(user)) {
    if (!nm %in% valid) {
      warning(paste0("`", nm, "` is not a valid field of ", cls))
    } else {
      out[[nm]] <- user[[nm]]
    }
  }
  class(out) <- class(default)
  out
}

#' Valid fields for each element class
#' @keywords internal
element_fields <- function(cls) {
  switch(cls,
    pmx_point    = c("shape", "size", "alpha", "color"),
    pmx_line     = c("linewidth", "linetype", "alpha", "color"),
    pmx_ribbon   = c("fill", "alpha", "color", "linetype", "linewidth"),
    pmx_errorbar = c("linewidth", "linetype", "alpha", "color", "width"),
    pmx_trend    = c("linewidth", "linetype", "color", "se_color", "se_alpha"),
    pmx_style    = c("color", "alpha"),
    pmx_color    = c("dv", "pred", "ipred"),
    character(0)
  )
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
  eb_key <- paste0(prefix, "_errorbar")
  pt_fields <- c("shape", "size", "alpha", "color")
  ln_fields <- c("linewidth", "linetype", "alpha", "color")
  eb_fields <- c("linewidth", "linetype", "alpha", "color")
  for (field in names(style)) {
    if (pt_key %in% names(defaults) && field %in% pt_fields) {
      defaults[[pt_key]][[field]] <- style[[field]]
    }
    if (ln_key %in% names(defaults) && field %in% ln_fields) {
      defaults[[ln_key]][[field]] <- style[[field]]
    }
    if (eb_key %in% names(defaults) && field %in% eb_fields) {
      defaults[[eb_key]][[field]] <- style[[field]]
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
  check_shape(shape, "shape")
  check_size(size, "size")
  check_color(color, "color")
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
  check_size(linewidth, "linewidth")
  check_color(color, "color")
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
  check_color(fill, "fill")
  check_color(color, "color")
  check_size(linewidth, "linewidth")
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
#' `obs`, `cent`) to set shared properties without specifying each
#' element individually.
#'
#' @param color Color applied to both point and line elements.
#' @param alpha Alpha applied to both point and line elements.
#'
#' @return A `pmx_style` element object
#' @export
#'
#' @examples
#' plot_dvtime_theme(obs = pmx_style(alpha = 0.3))
pmx_style <- function(color = NULL, alpha = NULL) {
  check_color(color, "color")
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
#' @param color Error bar color. Default is `NULL` (inherits from ggplot2 default).
#' @param width Error bar cap width. Default is 2.5 percent of maximum `NTIME`.
#'
#' @return A `pmx_errorbar` element object
#' @export
pmx_errorbar <- function(linewidth = NULL, linetype = NULL,
                         alpha = NULL, color = NULL, width = NULL) {
  check_size(linewidth, "linewidth")
  check_color(color, "color")
  check_size(width, "width")
  out <- compact(list(linewidth = linewidth, linetype = linetype,
                      alpha = alpha, color = color))
  out["width"] <- list(width)
  structure(out, class = "pmx_errorbar")
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
  check_size(linewidth, "linewidth")
  check_color(color, "color")
  check_color(se_color, "se_color")
  structure(
    compact(list(linewidth = linewidth, linetype = linetype, color = color,
                 se_color = se_color, se_alpha = se_alpha)),
    class = "pmx_trend"
  )
}


#' GOF overlay color aesthetics
#'
#' Creates a color element for [plot_gof_theme()] controlling the manual
#' color scale for DV, PRED, and IPRED overlay lines.
#'
#' @param dv Color for DV central tendency. Default `"blue"`.
#' @param pred Color for PRED central tendency. Default `"red"`.
#' @param ipred Color for IPRED central tendency. Default `"green"`.
#'
#' @return A `pmx_color` element object
#' @export
pmx_color <- function(dv = NULL, pred = NULL, ipred = NULL) {
  check_color(dv, "dv")
  check_color(pred, "pred")
  check_color(ipred, "ipred")
  structure(
    compact(list(dv = dv, pred = pred, ipred = ipred)),
    class = "pmx_color"
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
#' @param cent_errorbar Central tendency error bar aesthetics. See [pmx_errorbar()].
#' @param ref_line Reference line aesthetics (e.g., change-from-baseline). See [pmx_line()].
#' @param loq_line LOQ reference line aesthetics. See [pmx_line()].
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
#' plot_dvtime_theme(obs_point = pmx_point(size = 2), ref_line = pmx_line(linetype = 3))
#' plot_dvtime_theme(obs = pmx_style(alpha = 0.3))
plot_dvtime_theme <- function(obs_point = NULL, obs_line = NULL,
                              cent_point = NULL, cent_line = NULL,
                              cent_errorbar = NULL, ref_line = NULL, loq_line = NULL,
                              obs = NULL, cent = NULL) {
  defaults <- list(
    obs_point     = pmx_point(shape = 1, size = 0.75, alpha = 0.5),
    obs_line      = pmx_line(linewidth = 0.5, linetype = 1, alpha = 0.5),
    cent_point    = pmx_point(shape = 16, size = 1.25, alpha = 0),
    cent_line     = pmx_line(linewidth = 0.75, linetype = 1, alpha = 1),
    cent_errorbar = pmx_errorbar(linewidth = 0.75, linetype = 1, alpha = 1),
    ref_line      = pmx_line(linewidth = 0.5, linetype = 2, alpha = 1),
    loq_line      = pmx_line(linewidth = 0.5, linetype = 2, alpha = 1)
  )
  user <- compact(list(
    obs_point = obs_point, obs_line = obs_line,
    cent_point = cent_point, cent_line = cent_line,
    cent_errorbar = cent_errorbar, ref_line = ref_line, loq_line = loq_line,
    obs = obs, cent = cent
  ))
  merge_theme(user, defaults)
}


#' Population overlay GOF plot theme
#'
#' Constructor and factory for `plot_gof` plot aesthetics.
#' Call with no arguments to view defaults. Pass element overrides to customize.
#' Use role-level shortcuts `obs` and `cent` with [pmx_style()] to set shared
#' aesthetics for both point and line elements at once.
#' Override overlay colors with `cent_color = pmx_color()`.
#'
#' @param obs_point Observed data point aesthetics. See [pmx_point()].
#' @param obs_line Observed data line aesthetics. See [pmx_line()].
#' @param cent_point Shared central tendency point aesthetics for DV, PRED, and IPRED.
#'   See [pmx_point()].
#' @param cent_line Shared central tendency line aesthetics for DV, PRED, and IPRED.
#'   See [pmx_line()].
#' @param cent_errorbar Central tendency error bar aesthetics. See [pmx_errorbar()].
#' @param ref_line Reference line aesthetics. See [pmx_line()].
#' @param loq_line LOQ reference line aesthetics. See [pmx_line()].
#' @param obs Shortcut: apply shared aesthetics to both `obs_point` and `obs_line`.
#'   See [pmx_style()].
#' @param cent Shortcut: apply shared aesthetics to both `cent_point` and `cent_line`.
#'   See [pmx_style()].
#' @param cent_color Overlay color mapping for DV, PRED, and IPRED. See [pmx_color()].
#'
#' @return A named list of theme elements
#' @export
#'
#' @examples
#' plot_gof_theme()
#' plot_gof_theme(cent_color = pmx_color(pred = "purple"))
#' plot_gof_theme(cent = pmx_style(alpha = 0.8))
plot_gof_theme <- function(obs_point = NULL, obs_line = NULL,
                           cent_point = NULL, cent_line = NULL,
                           cent_errorbar = NULL, ref_line = NULL, loq_line = NULL,
                           obs = NULL, cent = NULL, cent_color = NULL) {
  defaults <- list(
    obs_point     = pmx_point(shape = 1, size = 0.75, alpha = 0.5, color = "darkgrey"),
    obs_line      = pmx_line(linewidth = 0.5, linetype = 1, alpha = 0.75, color = "darkgrey"),
    cent_point    = pmx_point(shape = 16, size = 1.25, alpha = 0),
    cent_line     = pmx_line(linewidth = 0.75, linetype = 1, alpha = 1),
    cent_errorbar = pmx_errorbar(linewidth = 0.75, linetype = 1, alpha = 1),
    ref_line      = pmx_line(linewidth = 0.5, linetype = 2, alpha = 1),
    loq_line      = pmx_line(linewidth = 0.5, linetype = 2, alpha = 1),
    cent_color    = pmx_color(dv = "blue", pred = "red", ipred = "green")
  )
  user <- compact(list(
    obs_point = obs_point, obs_line = obs_line,
    cent_point = cent_point, cent_line = cent_line,
    cent_errorbar = cent_errorbar, ref_line = ref_line, loq_line = loq_line,
    obs = obs, cent = cent, cent_color = cent_color
  ))
  merge_theme(user, defaults)
}


#' Response versus concentration plot theme
#'
#' Constructor and factory for `plot_dvconc` plot aesthetics.
#' Call with no arguments to view defaults. Pass element overrides to customize.
#'
#' @param obs_point Observed data point aesthetics. See [pmx_point()].
#' @param ref_line Reference line aesthetics. See [pmx_line()].
#' @param loess LOESS trend line aesthetics. See [pmx_trend()].
#' @param linear Linear trend line aesthetics. See [pmx_trend()].
#' @param obs Shortcut: apply shared aesthetics to `obs_point`. See [pmx_style()].
#'
#' @return A named list of theme elements
#' @export
#'
#' @examples
#' plot_dvconc_theme()
#' plot_dvconc_theme(loess = pmx_trend(color = "red"))
#' plot_dvconc_theme(obs = pmx_style(alpha = 0.3))
plot_dvconc_theme <- function(obs_point = NULL, ref_line = NULL, loess = NULL,
                              linear = NULL, obs = NULL) {
  defaults <- list(
    obs_point = pmx_point(shape = 1, size = 1.25, alpha = 0.5),
    ref_line = pmx_line(linewidth = 0.5, linetype = 2, alpha = 1),
    loess  = pmx_trend(linewidth = 1, linetype = 1,
                       color = "black", se_color = "lightgrey", se_alpha = 0.4),
    linear = pmx_trend(linewidth = 1, linetype = 2,
                       color = "black", se_color = "lightgrey", se_alpha = 0.4)
  )
  merge_theme(compact(list(obs_point = obs_point, ref_line = ref_line, loess = loess,
                           linear = linear, obs = obs)),
              defaults)
}


#' VPC plot theme
#'
#' Constructor and factory for `plot_vpc_cont` plot aesthetics.
#' Call with no arguments to view defaults. Pass element overrides to customize.
#' Each theme key maps 1:1 with a [plot_vpc_shown()] visibility toggle.
#'
#' @param obs_point Observed data point aesthetics. See [pmx_point()].
#' @param obs_median_line Observed median line aesthetics. See [pmx_line()].
#' @param obs_pi_line Observed quantile line aesthetics. See [pmx_line()].
#' @param sim_pi_line Simulated prediction interval line aesthetics. See [pmx_line()].
#' @param sim_pi_ci Simulated prediction interval CI ribbon aesthetics. See [pmx_ribbon()].
#' @param sim_pi_area Simulated prediction interval area ribbon aesthetics. See [pmx_ribbon()].
#' @param sim_median_line Simulated median line aesthetics. See [pmx_line()].
#' @param sim_median_ci Simulated median CI ribbon aesthetics. See [pmx_ribbon()].
#' @param loq_line LOQ reference line aesthetics. See [pmx_line()].
#'
#' @return A named list of theme elements
#' @export
#'
#' @examples
#' plot_vpc_theme()
#' plot_vpc_theme(obs_point = pmx_point(color = "#000000"))
plot_vpc_theme <- function(obs_point = NULL, obs_median_line = NULL, obs_pi_line = NULL,
                           sim_pi_line = NULL, sim_pi_ci = NULL, sim_pi_area = NULL,
                           sim_median_line = NULL, sim_median_ci = NULL,
                           loq_line = NULL) {
  defaults <- list(
    obs_point      = pmx_point(color = "#0000FF", size = 1, shape = 1, alpha = 0.7),
    obs_median_line    = pmx_line(color = "#FF0000", linetype = "solid", linewidth = 1),
    obs_pi_line     = pmx_line(color = "#0000FF", linetype = "dashed", linewidth = 0.5),
    sim_pi_line     = pmx_line(color = "#000000", linetype = "dotted", linewidth = 1),
    sim_pi_ci       = pmx_ribbon(fill = "#0000FF", alpha = 0.15),
    sim_pi_area     = pmx_ribbon(fill = "#0000FF", alpha = 0.15),
    sim_median_line = pmx_line(color = "#000000", linetype = "dashed", linewidth = 1),
    sim_median_ci   = pmx_ribbon(fill = "#FF0000", alpha = 0.3),
    loq_line        = pmx_line(color = "#990000", linetype = "dashed", linewidth = 0.5)
  )
  merge_theme(compact(list(obs_point = obs_point, obs_median_line = obs_median_line,
                           obs_pi_line = obs_pi_line, sim_pi_line = sim_pi_line,
                           sim_pi_ci = sim_pi_ci, sim_pi_area = sim_pi_area,
                           sim_median_line = sim_median_line, sim_median_ci = sim_median_ci,
                           loq_line = loq_line)),
              defaults)
}


#' GOF layer visibility settings
#'
#' Constructor and factory for controlling which GOF overlay layers are displayed.
#' Call with no arguments to view defaults. Pass overrides to customize.
#'
#' @param obs Show observed data points/lines. Default is `TRUE`.
#' @param dv Show DV central tendency. Default is `TRUE`.
#' @param pred Show PRED central tendency. Default is `TRUE`.
#' @param ipred Show IPRED central tendency. Default is `TRUE`.
#'
#' @return A named list of logicals
#' @export
#'
#' @examples
#' plot_gof_shown()
#' plot_gof_shown(pred = FALSE)
plot_gof_shown <- function(obs = NULL, dv = NULL, pred = NULL, ipred = NULL) {
  defaults <- list(obs = TRUE, dv = TRUE, pred = TRUE, ipred = TRUE)
  user <- compact(list(obs = obs, dv = dv, pred = pred, ipred = ipred))
  out <- defaults
  for (nm in names(user)) out[[nm]] <- user[[nm]]
  out
}


#' VPC layer visibility settings
#'
#' Constructor and factory for controlling which VPC layers are displayed.
#' Call with no arguments to view defaults. Pass overrides to customize.
#' Each element maps 1:1 with a [plot_vpc_theme()] aesthetic key.
#'
#' @param obs_point Show observed data points. Default is `TRUE`.
#' @param obs_pi_line Show observed quantile lines. Default is `TRUE`.
#' @param obs_median_line Show observed median line. Default is `TRUE`.
#' @param sim_pi_line Show simulated prediction interval lines. Default is `FALSE`.
#' @param sim_pi_ci Show simulated prediction interval CI ribbons. Default is `TRUE`.
#' @param sim_pi_area Show simulated prediction interval as shaded area. Default is `FALSE`.
#' @param sim_median_line Show simulated median line. Default is `FALSE`.
#' @param sim_median_ci Show simulated median CI ribbon. Default is `TRUE`.
#'
#' @return A named list of logicals
#' @export
#'
#' @examples
#' plot_vpc_shown()
#' plot_vpc_shown(obs_point = FALSE, sim_pi_line = TRUE)
plot_vpc_shown <- function(obs_point = NULL, obs_pi_line = NULL, obs_median_line = NULL,
                           sim_pi_line = NULL, sim_pi_ci = NULL, sim_pi_area = NULL,
                           sim_median_line = NULL, sim_median_ci = NULL) {
  defaults <- list(
    obs_point = TRUE, obs_pi_line = TRUE, obs_median_line = TRUE,
    sim_pi_line = FALSE, sim_pi_ci = TRUE, sim_pi_area = FALSE,
    sim_median_line = FALSE, sim_median_ci = TRUE
  )
  user <- compact(list(obs_point = obs_point, obs_pi_line = obs_pi_line,
                       obs_median_line = obs_median_line, sim_pi_line = sim_pi_line,
                       sim_pi_ci = sim_pi_ci, sim_pi_area = sim_pi_area,
                       sim_median_line = sim_median_line, sim_median_ci = sim_median_ci))
  out <- defaults
  for (nm in names(user)) out[[nm]] <- user[[nm]]
  out
}
