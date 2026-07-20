# ---------------------------------------------------------------------------
# ggstylekit styling internals
#
# pmxhelpr styles its plots with ggstylekit: each builder emits a plain ggplot
# (raw `aes()` for stratified/overlay series, `ggstylekit::series_layer()` to
# tag fixed reference/role layers), constructs a family `style_spec` via one of
# the `style_*()` presets, then finalizes with `ggstylekit::style_plot()`.
#
# Series names are the plot's role keys (e.g. "obs_point", "cent_line",
# "ref_line", "loq_line"). ggstylekit's per-series maps (colors, shapes,
# linetypes, ...) are keyed by these names; `style_plot()` fills the aesthetics
# each tagged layer's geom owns.
#
# ggstylekit's entity registry does NOT cover `geom_ribbon` or error bars
# (`GeomErrorbar`/`GeomLinerange`), so `style_plot()` passes those layers
# through untouched. Their fixed aesthetics are set inline by the builder,
# reading values out of the style_spec via `series_aes()`.
# ---------------------------------------------------------------------------


#' Internal helper: pmxhelpr house ggplot2 theme
#'
#' The shared base theme applied to every pmxhelpr plot via each preset's
#' `style_spec(theme = ...)` field: `theme_bw()` with the minor and vertical
#' major gridlines blanked. The VPC family passes `white_panel = TRUE` for a
#' white panel background with a thin black border.
#'
#' @param white_panel Logical. When `TRUE`, sets `panel.background` to a white
#'   rectangle with a thin black border (VPC family). Default `FALSE`.
#'
#' @return A ggplot2 theme object.
#' @keywords internal
pmx_house_theme <- function(white_panel = FALSE) {
  args <- list(panel.grid.minor = ggplot2::element_blank(),
               panel.grid.major.x = ggplot2::element_blank())
  if (isTRUE(white_panel)) {
    args$panel.background <- ggplot2::element_rect(fill = "white",
                                                   linewidth = 0.5,
                                                   color = "black")
  }
  ggplot2::theme_bw() + do.call(ggplot2::theme, args)
}


#' Internal helper: read a series' aesthetics out of a style_spec
#'
#' `ggstylekit::style_plot()` only resolves fixed aesthetics for geoms in its
#' entity registry (point/line/bar/area/box). Error bars and `geom_ribbon` are
#' not entities, so the builder sets their aesthetics inline, sourcing the
#' values from the style_spec's per-series maps keyed by `series`.
#'
#' @param spec A `ggstylekit_style_spec` (or any list with the per-series map
#'   fields `colors`, `fill`, `alphas`, `shapes`, `sizes`, `linetypes`,
#'   `linewidths`).
#' @param series Character scalar series/role name (e.g. `"cent_errorbar"`,
#'   `"sim_pi_ci"`).
#'
#' @return A named list of the aesthetics set for `series` (unset aesthetics are
#'   dropped), using ggplot2 aesthetic names (`colour`, `fill`, `alpha`,
#'   `shape`, `size`, `linetype`, `linewidth`).
#' @keywords internal
series_aes <- function(spec, series) {
  # Per-series maps are atomic named vectors, where `[[` on a missing name
  # errors (unlike lists); guard the lookup so unset aesthetics return NULL.
  pick <- function(map) {
    if (!is.null(map) && series %in% names(map)) unname(map[[series]]) else NULL
  }
  out <- list(
    colour    = pick(spec$colors),
    fill      = pick(spec$fill),
    alpha     = pick(spec$alphas),
    shape     = pick(spec$shapes),
    size      = pick(spec$sizes),
    linetype  = pick(spec$linetypes),
    linewidth = pick(spec$linewidths)
  )
  out[!vapply(out, is.null, logical(1))]
}


#' Internal helper: build a style_spec from preset defaults plus user overrides
#'
#' Each `style_*()` preset supplies a named list of default `style_spec` fields.
#' Per-series map fields (`colors`, `fill`, `linetypes`, `alphas`, `shapes`,
#' `sizes`, `linewidths`) are merged entry-wise onto the defaults so that a
#' partial override (e.g. `colors = c(obs_point = "red")`) keeps the other
#' roles' default values; all other fields replace their default wholesale. The
#' merged fields are passed to `ggstylekit::style_spec()`.
#'
#' @param defaults Named list of default `style_spec` arguments.
#' @param overrides Named list of user overrides (typically `list(...)`).
#'
#' @return A `ggstylekit_style_spec` object.
#' @keywords internal
build_style <- function(defaults, overrides = list()) {
  if (length(overrides) &&
      (is.null(names(overrides)) || any(!nzchar(names(overrides))))) {
    rlang::abort("style overrides must be named `style_spec()` fields.")
  }
  map_fields <- c("colors", "fill", "linetypes", "alphas", "shapes",
                  "sizes", "linewidths")
  merged <- defaults
  for (nm in names(overrides)) {
    ov <- overrides[[nm]]
    if (nm %in% map_fields && !is.null(defaults[[nm]]) && !is.null(ov)) {
      base <- defaults[[nm]]
      base[names(ov)] <- ov            # entry-wise merge; override wins per key
      merged[[nm]] <- base
    } else {
      merged[[nm]] <- ov               # NULL removes the key -> style_spec default
    }
  }
  do.call(ggstylekit::style_spec, merged)
}
