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


#' Internal helper: Merge user overrides into a complete default named list
#'
#' Iterates over names in the user-supplied list and overwrites matching entries
#' in the default. Warns on unrecognized names. Used to merge the layer
#' visibility lists from [plot_gof_shown()] / [plot_vpc_shown()] over their
#' defaults.
#'
#' @param user User-supplied list with partial overrides, or `NULL`.
#' @param default Complete default list.
#'
#' @return A merged list with the same class as `default`
#' @keywords internal
#' @examples
#' pmxhelpr:::merge_element(list(obs = FALSE), plot_gof_shown())
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
#'
#' Retained as a lookup used by [merge_element()]; returns `character(0)` for
#' non-`pmx_element` inputs (e.g. the plain visibility lists from
#' [plot_gof_shown()] / [plot_vpc_shown()]), so `merge_element()` falls back to
#' the default's own names.
#'
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
