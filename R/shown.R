#' GOF layer visibility settings
#'
#' Constructor for the `shown` argument of [plot_gof()], controlling which GOF
#' overlay layers are built. Call with no arguments to view the defaults; pass
#' `TRUE`/`FALSE` overrides to customize. Each key names a series of the plot:
#' its color is the `style_gof(colors = )` entry of the same name in upper
#' case, which is also its legend label.
#'
#' @param obs Show observed data points and lines (the `OBS` series). Default
#'    is `TRUE`.
#' @param dv Show DV central tendency (the `DV` series). Default is `TRUE`.
#' @param pred Show PRED central tendency (the `PRED` series). Default is
#'    `TRUE`.
#' @param ipred Show IPRED central tendency (the `IPRED` series). Default is
#'    `TRUE`.
#'
#' @family goodness-of-fit
#' @return A `gof_shown` object: a named list of `TRUE`/`FALSE`.
#' @export
#'
#' @examples
#' plot_gof_shown()
#' plot_gof_shown(pred = FALSE)
plot_gof_shown <- function(obs = NULL, dv = NULL, pred = NULL, ipred = NULL) {
  new_shown(
    defaults = list(obs = TRUE, dv = TRUE, pred = TRUE, ipred = TRUE),
    user = list(obs = obs, dv = dv, pred = pred, ipred = ipred),
    family = "gof"
  )
}


#' VPC layer visibility settings
#'
#' Constructor for the `shown` argument of the VPC plot functions and
#' [plot_vpc_legend()], controlling which VPC layers are built. Call with no
#' arguments to view the defaults; pass `TRUE`/`FALSE` overrides to customize.
#' Each element maps 1:1 with a [style_vpc()] role/series key.
#'
#' @param obs_point Show observed data points. Default is `TRUE`.
#' @param obs_pi_line Show observed quantile lines. Default is `TRUE`.
#' @param obs_median_line Show observed median line. Default is `TRUE`.
#' @param sim_pi_line Show simulated prediction interval lines. Default is `FALSE`.
#' @param sim_pi_ci Show simulated prediction interval CI ribbons. Default is `TRUE`.
#' @param sim_pi_area Show simulated prediction interval as shaded area. Default is `FALSE`.
#' @param sim_median_line Show simulated median line. Default is `FALSE`.
#' @param sim_median_ci Show simulated median CI ribbon. Default is `TRUE`.
#' @param loq_line Show the LLOQ reference line on standard VPCs, and its
#'    entry in [plot_vpc_legend()]. Default is `TRUE`.
#'
#' @family vpc
#' @return A `vpc_shown` object: a named list of `TRUE`/`FALSE`.
#' @export
#'
#' @examples
#' plot_vpc_shown()
#' plot_vpc_shown(obs_point = FALSE, sim_pi_line = TRUE)
plot_vpc_shown <- function(obs_point = NULL, obs_pi_line = NULL, obs_median_line = NULL,
                           sim_pi_line = NULL, sim_pi_ci = NULL, sim_pi_area = NULL,
                           sim_median_line = NULL, sim_median_ci = NULL,
                           loq_line = NULL) {
  new_shown(
    defaults = list(
      obs_point = TRUE, obs_pi_line = TRUE, obs_median_line = TRUE,
      sim_pi_line = FALSE, sim_pi_ci = TRUE, sim_pi_area = FALSE,
      sim_median_line = FALSE, sim_median_ci = TRUE, loq_line = TRUE
    ),
    user = list(obs_point = obs_point, obs_pi_line = obs_pi_line,
                obs_median_line = obs_median_line, sim_pi_line = sim_pi_line,
                sim_pi_ci = sim_pi_ci, sim_pi_area = sim_pi_area,
                sim_median_line = sim_median_line, sim_median_ci = sim_median_ci,
                loq_line = loq_line),
    family = "vpc"
  )
}


## Overlay the non-NULL `user` values on `defaults` and tag the result with
## the family's class. Values are checked against the caller's arguments, so
## errors name plot_vpc_shown() / plot_gof_shown().
new_shown <- function(defaults, user, family, call = rlang::caller_env()) {
  user <- compact(user)
  for (nm in names(user)) check_boolean(user[[nm]], nm, call = call)
  out <- defaults
  out[names(user)] <- user
  structure(out, class = c(paste0(family, "_shown"), "pmx_shown"))
}


#' Print a layer visibility object
#'
#' Prints a [plot_vpc_shown()] or [plot_gof_shown()] object as one
#' `key  TRUE/FALSE` line per layer.
#'
#' @param x A `vpc_shown` or `gof_shown` object.
#' @param ... Currently unused.
#'
#' @return `invisible(x)`.
#' @export
#' @method print pmx_shown
print.pmx_shown <- function(x, ...) {
  cat(sprintf("<%s>\n", class(x)[1]))
  keys <- format(names(x))
  cat(sprintf("  %s  %s\n", keys, vapply(x, format, character(1))), sep = "")
  invisible(x)
}


## Resolve a user `shown` against the family default (a plot_*_shown()
## object): NULL gives the default; a named list or named logical vector is
## merged over it. Unknown keys warn once and are dropped; values that are
## not TRUE/FALSE, and the other family's object, abort.
resolve_shown <- function(shown, default, arg = rlang::caller_arg(shown),
                          call = rlang::caller_env()) {
  force(arg)
  if (is.null(shown)) return(default)
  family <- class(default)[1]
  ctor <- paste0("plot_", sub("_shown$", "", family), "_shown")
  if (inherits(shown, "pmx_shown") && !inherits(shown, family)) {
    pmx_abort("{.arg {arg}} is a {.cls {class(shown)[1]}} object; use {.fn {ctor}}.",
              "bad_arg", call = call)
  }
  if (!(is.list(shown) || is.logical(shown)) || is.null(names(shown)) ||
      any(!nzchar(names(shown)))) {
    pmx_abort("{.arg {arg}} must be a named list of {.code TRUE}/{.code FALSE} values, e.g. from {.fn {ctor}}.",
              "bad_arg", call = call)
  }
  shown <- as.list(shown)
  unknown <- setdiff(names(shown), names(default))
  if (length(unknown) > 0) {
    pmx_warn("{.arg {arg}} has unknown {cli::qty(unknown)}key{?s} {.val {unknown}}; valid keys: {.val {names(default)}}.",
             "ignored_arg")
    shown <- shown[setdiff(names(shown), unknown)]
  }
  bad <- names(shown)[!vapply(shown, function(v) is.logical(v) && length(v) == 1L && !is.na(v),
                              logical(1))]
  if (length(bad) > 0) {
    pmx_abort("{.arg {arg}} must hold {.code TRUE} or {.code FALSE}; {cli::qty(bad)}key{?s} {.val {bad}} {?does/do} not.",
              "bad_arg", call = call)
  }
  out <- default
  out[names(shown)] <- shown
  out
}
