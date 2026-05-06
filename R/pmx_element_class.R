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
