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
#' [pmx_color()]. Use the per-type predicates ([is_pmx_point()] etc.) when
#' you need to distinguish among element flavors.
#'
#' @param x Object to test.
#'
#' @return Logical scalar.
#' @export is_pmx_element
#'
#' @examples
#' is_pmx_element(pmx_point(shape = 1))     # TRUE
#' is_pmx_element(pmx_line(linewidth = 1))  # TRUE
#' is_pmx_element(list(shape = 1))          # FALSE

is_pmx_element <- function(x) {
  inherits(x, "pmx_element")
}



#' Per-type predicates for pmx theme elements
#'
#' @description
#' Predicates that return `TRUE` only when `x` is the specific element type
#' indicated by the function name. Each is a thin wrapper over
#' `inherits(x, "<tag>")`. Pair with [is_pmx_element()] for the broader
#' "any pmx element" test.
#'
#' @param x Object to test.
#'
#' @return Logical scalar.
#' @name pmx_element_predicates
#'
#' @examples
#' is_pmx_point(pmx_point())        # TRUE
#' is_pmx_point(pmx_line())         # FALSE
#' is_pmx_line(pmx_line())          # TRUE
#' is_pmx_ribbon(pmx_ribbon())      # TRUE
NULL


#' @rdname pmx_element_predicates
#' @export is_pmx_point
is_pmx_point <- function(x) inherits(x, "pmx_point")

#' @rdname pmx_element_predicates
#' @export is_pmx_line
is_pmx_line <- function(x) inherits(x, "pmx_line")

#' @rdname pmx_element_predicates
#' @export is_pmx_ribbon
is_pmx_ribbon <- function(x) inherits(x, "pmx_ribbon")

#' @rdname pmx_element_predicates
#' @export is_pmx_errorbar
is_pmx_errorbar <- function(x) inherits(x, "pmx_errorbar")

#' @rdname pmx_element_predicates
#' @export is_pmx_trend
is_pmx_trend <- function(x) inherits(x, "pmx_trend")

#' @rdname pmx_element_predicates
#' @export is_pmx_style
is_pmx_style <- function(x) inherits(x, "pmx_style")

#' @rdname pmx_element_predicates
#' @export is_pmx_color
is_pmx_color <- function(x) inherits(x, "pmx_color")



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
