#' Construct a `pmx_theme`
#'
#' @description
#' Public factory for the shared `pmx_theme` base class. Builds a (possibly
#' partial) named list of `pmx_element` objects and tags it with a class
#' vector. Pair with [+.pmx_theme()] to compose a partial override onto a
#' complete plot theme:
#'
#' ```
#' base  <- plot_vpc_theme()
#' patch <- pmx_theme(list(obs_point = pmx_point(color = "red")))
#' base + patch
#' ```
#'
#' The five per-plot theme factories ([plot_dvtime_theme()],
#' [plot_gof_theme()], [plot_dvconc_theme()], [plot_doseprop_theme()],
#' [plot_vpc_theme()]) call `pmx_theme()` internally with their own subclass
#' tag, so user code rarely needs the `subclass` argument.
#'
#' @param elements Named list of `pmx_element` objects (e.g. as returned by
#'    [pmx_point()], [pmx_line()], [pmx_ribbon()]). May be empty. `NULL`
#'    entries are dropped.
#' @param subclass Character scalar tag prepended to the class vector
#'    (e.g. `"plot_vpc_theme"`). When `NULL` (default), the result carries
#'    only `"pmx_theme"` — the right shape for a generic partial.
#'
#' @family pmx theme class
#' @return An object of class `c(subclass, "pmx_theme")` (or just
#'    `"pmx_theme"` when `subclass` is `NULL`).
#' @export
#'
#' @examples
#' # Generic partial — no subclass; useful as a `+.pmx_theme` right-hand side
#' pmx_theme(list(obs_point = pmx_point(color = "red")))
#'
#' # Compose onto a complete theme
#' plot_vpc_theme() + pmx_theme(list(obs_point = pmx_point(color = "red")))

pmx_theme <- function(elements = list(), subclass = NULL) {
  if (!is.list(elements)) {
    rlang::abort("`elements` must be a named list.")
  }
  elements <- compact(elements)
  if (length(elements) > 0) {
    if (is.null(names(elements)) || any(!nzchar(names(elements)))) {
      rlang::abort("`elements` must be a fully-named list.")
    }
    for (nm in names(elements)) {
      if (!is_pmx_element(elements[[nm]])) {
        rlang::abort(paste0("Theme entry `", nm,
                            "` must be a `pmx_element`."))
      }
    }
  }
  cls <- if (is.null(subclass)) "pmx_theme" else c(subclass, "pmx_theme")
  structure(elements, class = cls)
}



#' Test whether an object is a pmx plot theme
#'
#' @description
#' Predicate that returns `TRUE` if `x` carries the shared `"pmx_theme"`
#' class -- i.e., it was constructed by [pmx_theme()] or one of
#' [plot_dvtime_theme()], [plot_gof_theme()], [plot_dvconc_theme()],
#' [plot_doseprop_theme()], or [plot_vpc_theme()]. For a specific-type
#' check, use `inherits(x, "plot_vpc_theme")` (or whichever subclass)
#' directly.
#'
#' @param x Object to test.
#' @param strict Logical. When `TRUE`, additionally checks that every named
#'    entry of the theme is a `pmx_element`. Default `FALSE`
#'    (class-tag check only, cheap).
#'
#' @family pmx theme class
#' @return Logical scalar.
#' @export is_pmx_theme
#'
#' @examples
#' is_pmx_theme(plot_dvtime_theme())                     # TRUE
#' is_pmx_theme(plot_vpc_theme())                        # TRUE
#' is_pmx_theme(pmx_point())                             # FALSE
#' is_pmx_theme(list(obs_point = 1))                     # FALSE
#' inherits(plot_vpc_theme(), "plot_vpc_theme")          # TRUE -- specific-type check

is_pmx_theme <- function(x, strict = FALSE) {
  ok <- inherits(x, "pmx_theme")
  if (!ok || !isTRUE(strict)) return(ok)
  if (length(x) == 0) return(TRUE)
  all(vapply(x, is_pmx_element, logical(1)))
}



#' Combine two pmx plot themes
#'
#' @description
#' Left-to-right merge of two `pmx_theme` objects. Each named entry in `b`
#' overrides the matching entry in `a`; entries unique to `a` are unchanged.
#' The class vector of `a` is preserved on the result, so a partial override
#' applied to a typed theme keeps its subclass (e.g.
#' `plot_vpc_theme() + pmx_theme(list(obs_point = ...))` is still a
#' `plot_vpc_theme`).
#'
#' @param a A `pmx_theme` object (left side, the "base").
#' @param b A `pmx_theme` object (right side, the "override"), or `NULL`.
#'    When `NULL`, `a` is returned unchanged.
#'
#' @return A `pmx_theme` with the same class vector as `a`.
#' @export
#' @method + pmx_theme
#'
#' @examples
#' base  <- plot_vpc_theme()
#' patch <- pmx_theme(list(obs_point = pmx_point(color = "red")))
#' out   <- base + patch
#' inherits(out, "plot_vpc_theme")   # TRUE -- class preserved
#' out$obs_point$color               # "red"
#'
#' base + NULL                       # identical to `base`

`+.pmx_theme` <- function(a, b) {
  if (is.null(b)) return(a)
  if (!inherits(b, "pmx_theme")) {
    rlang::abort(
      "right-hand side of `+.pmx_theme` must be a `pmx_theme` or `NULL`."
    )
  }
  merged <- merge_theme(b, a)
  class(merged) <- class(a)
  merged
}



#' Print method for pmx plot themes
#'
#' @description
#' Compact REPL display for any object inheriting `"pmx_theme"` -- the
#' values returned by [pmx_theme()] and the per-plot theme factories.
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
