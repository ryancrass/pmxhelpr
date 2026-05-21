#' Internal helper: format pmx element fields as a comma-separated string
#'
#' Renders the set fields of a `pmx_element` (or any list-of-named-fields)
#' as `name = value` pairs, joined with `, `. Used by both
#' [print.pmx_element()] and [print.pmx_theme()].
#'
#' @param x A list of named fields.
#'
#' @return A character scalar.
#' @keywords internal

format_pmx_fields <- function(x) {
  if (length(x) == 0) return("(no fields set)")
  parts <- vapply(seq_along(x),
                  function(i) sprintf("%s = %s",
                                      names(x)[i],
                                      format(x[[i]])),
                  character(1))
  paste(parts, collapse = ", ")
}



#' Test whether an object is a pmx theme element
#'
#' @description
#' Predicate that returns `TRUE` if `x` carries the shared `"pmx_element"`
#' class -- i.e., it was constructed by one of [pmx_point()], [pmx_line()],
#' [pmx_ribbon()], [pmx_errorbar()], [pmx_trend()], [pmx_style()], or
#' [pmx_color()]. For a specific-type check, use `inherits(x, "pmx_point")`
#' (or whichever type) directly.
#'
#' @param x Object to test.
#'
#' @family pmx theme class
#' @return Logical scalar.
#' @export is_pmx_element
#'
#' @examples
#' is_pmx_element(pmx_point(shape = 1))     # TRUE
#' is_pmx_element(pmx_line(linewidth = 1))  # TRUE
#' is_pmx_element(list(shape = 1))          # FALSE
#' inherits(pmx_point(), "pmx_point")       # TRUE -- specific-type check

is_pmx_element <- function(x) {
  inherits(x, "pmx_element")
}



#' Combine two pmx theme elements
#'
#' @description
#' Overlay one `pmx_element` onto another of the same subclass. Each set
#' field in `b` overrides the matching field in `a`; fields unique to `a` are
#' unchanged. Useful for layering partial overrides on a base element:
#' `pmx_point(size = 2) + pmx_point(color = "red")` returns
#' `pmx_point(size = 2, color = "red")`.
#'
#' Cross-subclass combinations (e.g. `pmx_style + pmx_point`) are
#' intentionally disallowed — `pmx_style` is a theme-level shortcut applied
#' at theme construction time, not a sibling element type.
#'
#' @param a A `pmx_element` object (left side, the "base").
#' @param b A `pmx_element` object of the same subclass as `a` (right side,
#'    the "override"), or `NULL`. When `NULL`, `a` is returned unchanged.
#'
#' @return A `pmx_element` with the same class as `a`.
#' @export
#' @method + pmx_element
#'
#' @examples
#' pmx_point(size = 2) + pmx_point(color = "red")
#' pmx_line(linewidth = 1) + NULL

`+.pmx_element` <- function(a, b) {
  if (is.null(b)) return(a)
  if (!is_pmx_element(b)) {
    rlang::abort(
      "right-hand side of `+.pmx_element` must be a `pmx_element` or `NULL`."
    )
  }
  if (class(a)[1] != class(b)[1]) {
    rlang::abort(paste0(
      "cannot combine `", class(a)[1], "` with `", class(b)[1],
      "` via `+.pmx_element` (subclasses must match)."
    ))
  }
  merge_element(b, a)
}



#' Print method for pmx theme elements
#'
#' @description
#' Compact REPL display for any object inheriting `"pmx_element"` -- the
#' values returned by [pmx_point()], [pmx_line()], [pmx_ribbon()],
#' [pmx_errorbar()], [pmx_trend()], [pmx_style()], and [pmx_color()]. Shows
#' the element type as a banner and the set fields inline as
#' `name = value, name = value`.
#'
#' @param x A pmx element object.
#' @param ... Currently unused.
#'
#' @return `invisible(x)`.
#' @export
#' @method print pmx_element

print.pmx_element <- function(x, ...) {
  cls <- class(x)[1]
  cat(sprintf("<%s>\n", cls))
  cat(sprintf("  %s\n", format_pmx_fields(unclass(x))))
  invisible(x)
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
#' @family element constructors
#' @return A `pmx_point` element object
#' @export
#' @examples
#' pmx_point(shape = 1, size = 2, color = "blue")
pmx_point <- function(shape = NULL, size = NULL, alpha = NULL, color = NULL) {
  check_shape(shape, "shape")
  check_size(size, "size")
  check_color(color, "color")
  structure(
    compact(list(shape = shape, size = size, alpha = alpha, color = color)),
    class = c("pmx_point", "pmx_element")
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
#' @family element constructors
#' @return A `pmx_line` element object
#' @export
#' @examples
#' pmx_line(linewidth = 1, linetype = "dashed", color = "red")
pmx_line <- function(linewidth = NULL, linetype = NULL, alpha = NULL,
                     color = NULL) {
  check_size(linewidth, "linewidth")
  check_color(color, "color")
  structure(
    compact(list(linewidth = linewidth, linetype = linetype,
                 alpha = alpha, color = color)),
    class = c("pmx_line", "pmx_element")
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
#' @family element constructors
#' @return A `pmx_ribbon` element object
#' @export
#' @examples
#' pmx_ribbon(fill = "skyblue", alpha = 0.3)
pmx_ribbon <- function(fill = NULL, alpha = NULL, color = NULL,
                       linetype = NULL, linewidth = NULL) {
  check_color(fill, "fill")
  check_color(color, "color")
  check_size(linewidth, "linewidth")
  structure(
    compact(list(fill = fill, alpha = alpha, color = color,
                 linetype = linetype, linewidth = linewidth)),
    class = c("pmx_ribbon", "pmx_element")
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
#' @family element constructors
#' @return A `pmx_style` element object
#' @export
#'
#' @examples
#' plot_dvtime_theme(obs = pmx_style(alpha = 0.3))
pmx_style <- function(color = NULL, alpha = NULL) {
  check_color(color, "color")
  structure(
    compact(list(color = color, alpha = alpha)),
    class = c("pmx_style", "pmx_element")
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
#' @family element constructors
#' @return A `pmx_errorbar` element object
#' @export
#' @examples
#' pmx_errorbar(linewidth = 0.5, width = 0.5)
pmx_errorbar <- function(linewidth = NULL, linetype = NULL,
                         alpha = NULL, color = NULL, width = NULL) {
  check_size(linewidth, "linewidth")
  check_color(color, "color")
  check_size(width, "width")
  out <- compact(list(linewidth = linewidth, linetype = linetype,
                      alpha = alpha, color = color))
  out["width"] <- list(width)
  structure(out, class = c("pmx_errorbar", "pmx_element"))
}


#' Trend line aesthetics (dvconc loess/linear)
#'
#' @param linewidth Line width. Default varies by trend type.
#' @param linetype Line type. Default varies by trend type.
#' @param color Line color. Default is `"black"`.
#' @param se_color Standard error ribbon color. Default is `"lightgrey"`.
#' @param se_alpha Standard error ribbon alpha. Default is 0.4.
#'
#' @family element constructors
#' @return A `pmx_trend` element object
#' @export
#' @examples
#' pmx_trend(linewidth = 1.2, color = "darkblue", se_color = "lightgrey")
pmx_trend <- function(linewidth = NULL, linetype = NULL, color = NULL,
                      se_color = NULL, se_alpha = NULL) {
  check_size(linewidth, "linewidth")
  check_color(color, "color")
  check_color(se_color, "se_color")
  structure(
    compact(list(linewidth = linewidth, linetype = linetype, color = color,
                 se_color = se_color, se_alpha = se_alpha)),
    class = c("pmx_trend", "pmx_element")
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
#' @family element constructors
#' @return A `pmx_color` element object
#' @export
#' @examples
#' pmx_color(dv = "black", pred = "purple", ipred = "darkgreen")
pmx_color <- function(dv = NULL, pred = NULL, ipred = NULL) {
  check_color(dv, "dv")
  check_color(pred, "pred")
  check_color(ipred, "ipred")
  structure(
    compact(list(dv = dv, pred = pred, ipred = ipred)),
    class = c("pmx_color", "pmx_element")
  )
}
