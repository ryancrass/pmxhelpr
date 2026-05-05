#' Test whether an object is a pmx plot theme
#'
#' @description
#' Predicate that returns `TRUE` if `x` carries the shared `"pmx_theme"`
#' class -- i.e., it was constructed by one of [plot_dvtime_theme()],
#' [plot_gof_theme()], [plot_dvconc_theme()], [plot_doseprop_theme()], or
#' [plot_vpc_theme()]. Use the per-type predicates
#' ([is_plot_dvtime_theme()] etc.) when you need to distinguish among
#' theme flavors.
#'
#' @param x Object to test.
#'
#' @return Logical scalar.
#' @export is_pmx_theme
#'
#' @examples
#' is_pmx_theme(plot_dvtime_theme())   # TRUE
#' is_pmx_theme(plot_vpc_theme())      # TRUE
#' is_pmx_theme(pmx_point())           # FALSE
#' is_pmx_theme(list(obs_point = 1))   # FALSE

is_pmx_theme <- function(x) {
  inherits(x, "pmx_theme")
}



#' Per-type predicates for pmx plot themes
#'
#' @description
#' Predicates that return `TRUE` only when `x` is the specific theme type
#' indicated by the function name. Each is a thin wrapper over
#' `inherits(x, "<tag>")`. Pair with [is_pmx_theme()] for the broader
#' "any pmx theme" test.
#'
#' @param x Object to test.
#'
#' @return Logical scalar.
#' @name pmx_theme_predicates
#'
#' @examples
#' is_plot_dvtime_theme(plot_dvtime_theme())   # TRUE
#' is_plot_dvtime_theme(plot_vpc_theme())      # FALSE
#' is_plot_vpc_theme(plot_vpc_theme())         # TRUE
NULL


#' @rdname pmx_theme_predicates
#' @export is_plot_dvtime_theme
is_plot_dvtime_theme <- function(x) inherits(x, "plot_dvtime_theme")

#' @rdname pmx_theme_predicates
#' @export is_plot_gof_theme
is_plot_gof_theme <- function(x) inherits(x, "plot_gof_theme")

#' @rdname pmx_theme_predicates
#' @export is_plot_dvconc_theme
is_plot_dvconc_theme <- function(x) inherits(x, "plot_dvconc_theme")

#' @rdname pmx_theme_predicates
#' @export is_plot_doseprop_theme
is_plot_doseprop_theme <- function(x) inherits(x, "plot_doseprop_theme")

#' @rdname pmx_theme_predicates
#' @export is_plot_vpc_theme
is_plot_vpc_theme <- function(x) inherits(x, "plot_vpc_theme")



#' Print method for pmx plot themes
#'
#' @description
#' Compact REPL display for any object inheriting `"pmx_theme"` -- the
#' values returned by [plot_dvtime_theme()], [plot_gof_theme()],
#' [plot_dvconc_theme()], [plot_doseprop_theme()], and [plot_vpc_theme()].
#' Shows the theme type as a banner and one line per theme key, listing the
#' inner element type and its set fields inline.
#'
#' @param x A pmx theme object.
#' @param ... Currently unused.
#'
#' @return `invisible(x)`.
#' @export
#' @method print pmx_theme

print.pmx_theme <- function(x, ...) {
  cls <- class(x)[1]
  cat(sprintf("<%s>\n", cls))
  keys <- names(x)
  if (length(keys) == 0) {
    cat("  (empty theme)\n")
    return(invisible(x))
  }
  key_width <- max(nchar(keys))
  for (k in keys) {
    entry <- x[[k]]
    if (is_pmx_element(entry)) {
      inner_cls <- class(entry)[1]
      cat(sprintf("  %-*s <%s>: %s\n",
                  key_width, k, inner_cls,
                  format_pmx_fields(unclass(entry))))
    } else {
      cat(sprintf("  %-*s %s\n",
                  key_width, k, format(entry)))
    }
  }
  invisible(x)
}
