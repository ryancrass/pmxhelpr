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
