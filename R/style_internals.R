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
# A few layers are built inline instead of tagged: the VPC ribbons (the builder
# is all-inline by design), the LLOQ line (its label maps to a manual linetype
# scale), and the `plot_vpc_legend()` proxies. Their fixed aesthetics are read
# out of the style_spec via `series_aes()`; inline aesthetics take precedence
# over ggstylekit's entity base fields, so `style_plot()` leaves them as set.
# ---------------------------------------------------------------------------


#' Internal helper: pmxhelpr house ggplot2 theme
#'
#' The shared base theme applied to every pmxhelpr plot via each preset's
#' `style_spec(theme = ...)` field: `theme_bw()` with the minor and vertical
#' major gridlines blanked. The VPC family passes `white_panel = TRUE` for a
#' white panel background with a thin black border.
#'
#' The forest family (`forest_panel = TRUE`) inverts the gridline orientation:
#' it keeps `panel.grid.major.x` as a faint ruler aligned with the point
#' estimates and blanks `panel.grid.major.y` (the `facet_grid()` rows group the
#' y-axis by covariate name instead). Panels are transparent with a thin grey
#' border and no vertical spacing, so the row facets stack into one frame
#' with their shared borders acting as separator lines. The covariate row
#' strips are styled for `switch = "y"`: placed outside the y-axis, no
#' background fill, bold horizontal text aligned to the top-left so each
#' covariate name heads its group of rows. The y-axis text is black. The
#' right-hand secondary y-axis carries the estimate column (left-aligned text
#' at the axis text size, an 8 pt gap from the panel, no ticks) and the plot
#' subtitle is its bold header, right-aligned across the full plot width
#' (`plot.title.position = "plot"`) so it sits on the top edge of the panels
#' above the column.
#'
#' @param white_panel Logical. When `TRUE`, sets `panel.background` to a white
#'   rectangle with a thin black border (VPC family). Default `FALSE`.
#' @param forest_panel Logical. When `TRUE`, applies the forest-family panel
#'   variant described above; `white_panel` is then ignored. Default `FALSE`.
#'
#' @return A ggplot2 theme object.
#' @keywords internal
pmx_house_theme <- function(white_panel = FALSE, forest_panel = FALSE) {
  args <- list(panel.grid.minor = ggplot2::element_blank())
  if (isTRUE(forest_panel)) {
    # Panels touch (no vertical spacing) so the shared borders read as
    # separator lines inside one frame rather than as separate boxes.
    args$panel.background   <- ggplot2::element_rect(fill = NA, color = "grey60")
    args$panel.spacing.y    <- ggplot2::unit(0, "pt")
    args$panel.grid.major.x <- ggplot2::element_line(color = "grey85",
                                                     linewidth = 0.3)
    args$panel.grid.major.y <- ggplot2::element_blank()
    args$strip.background   <- ggplot2::element_blank()
    args$strip.placement    <- "outside"
    # Top-left aligned, so each covariate name reads as the header of its
    # group of level rows.
    args$strip.text.y.left  <- ggplot2::element_text(angle = 0, face = "bold",
                                                     hjust = 0, vjust = 1)
    # Titles span the whole plot, so the right-aligned subtitle (the
    # estimate column header) ends at the column's right edge; the subtitle
    # row sits directly on top of the panels.
    args$plot.title          <- ggplot2::element_text(face = "bold")
    args$plot.title.position <- "plot"
    args$plot.subtitle       <- ggplot2::element_text(
      face = "bold", hjust = 1, size = ggplot2::rel(1),
      margin = ggplot2::margin(b = 4)
    )
    args$axis.text.y.left   <- ggplot2::element_text(colour = "black")
    # Secondary y-axis = the estimate column: black left-aligned text at full
    # axis size, set off from the panel, no ticks.
    args$axis.text.y.right  <- ggplot2::element_text(
      hjust = 0, colour = "black", margin = ggplot2::margin(l = 8)
    )
    args$axis.ticks.y.right <- ggplot2::element_blank()
  } else {
    args$panel.grid.major.x <- ggplot2::element_blank()
    if (isTRUE(white_panel)) {
      args$panel.background <- ggplot2::element_rect(fill = "white",
                                                     linewidth = 0.5,
                                                     color = "black")
    }
  }
  ggplot2::theme_bw() + do.call(ggplot2::theme, args)
}


#' Internal helper: read a series' aesthetics out of a style_spec
#'
#' Layers the builders set inline rather than tagging with
#' `ggstylekit::series_layer()` (VPC ribbons, the LLOQ line, and the
#' `plot_vpc_legend()` proxies) source their aesthetics from the style_spec's
#' per-series maps keyed by `series` through this reader.
#'
#' @param spec A `ggstylekit_style_spec` (or any list with the per-series map
#'   fields `colors`, `fill`, `alphas`, `shapes`, `sizes`, `linetypes`,
#'   `linewidths`).
#' @param series Character scalar series/role name (e.g. `"loq_line"`,
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
    map <- map_pins(map)
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


## Internal: the named entries of a per-series map. A named vector is returned
## as is; a palette function returns the entries `ggstylekit::with_defaults()`
## pinned on it (NULL when none). ggstylekit keeps pins in an internal
## `ggstylekit_pins` attribute and does not export a reader for them yet.
map_pins <- function(map) {
  if (is.function(map)) attr(map, "ggstylekit_pins", exact = TRUE) else map
}


#' Internal helper: build a style_spec from preset defaults plus user overrides
#'
#' Each `style_*()` preset supplies a named list of default `style_spec` fields.
#' Per-series map fields (`colors`, `fill`, `linetypes`, `alphas`, `shapes`,
#' `sizes`, `linewidths`) are merged entry-wise onto the defaults so that a
#' partial override (e.g. `colors = c(obs_point = "red")`) keeps the other
#' roles' default values; all other fields replace their default wholesale. A
#' per-series map may also be given as a palette `function(n)` (e.g.
#' `function(n) grDevices::hcl.colors(n, "Viridis")`), which `ggstylekit` calls
#' with the number of mapped groups. When either side of a map is a palette,
#' the two merge through `ggstylekit::with_defaults()`: the named entries
#' (the preset's role colours, or the user's pins over a palette default such
#' as [style_forest_pmforest()]'s) are pinned on the palette, and every other
#' group draws from it. A map named in `level_maps` is keyed by data levels
#' rather than roles (e.g. [style_gof()]'s `OBS`/`DV`/`PRED`/`IPRED` colours),
#' so a palette override replaces it instead of being pinned over. The merged
#' fields are passed to `ggstylekit::style_spec()`.
#'
#' @param defaults Named list of default `style_spec` arguments.
#' @param overrides Named list of user overrides (typically `list(...)`).
#' @param level_maps Per-series map fields whose defaults are keyed by data
#'   levels; a palette override replaces them wholesale.
#' @param call The call reported in the error.
#'
#' @return A `ggstylekit_style_spec` object.
#' @keywords internal
build_style <- function(defaults, overrides = list(), level_maps = character(),
                        call = rlang::caller_env()) {
  if (length(overrides) &&
      (is.null(names(overrides)) || any(!nzchar(names(overrides))))) {
    pmx_abort("Style overrides must be named {.fn ggstylekit::style_spec} fields.",
              "bad_style", call = call)
  }
  map_fields <- c("colors", "fill", "linetypes", "alphas", "shapes",
                  "sizes", "linewidths")
  merged <- defaults
  for (nm in names(overrides)) {
    ov <- overrides[[nm]]
    base <- defaults[[nm]]
    if (!(nm %in% map_fields) || is.null(base) || is.null(ov) ||
        (nm %in% level_maps && is.function(ov))) {
      merged[[nm]] <- ov               # non-map field or a palette over level
                                       # keys: replace wholesale; NULL
                                       # restores the style_spec default
    } else if (is.function(ov) || is.function(base)) {
      merged[[nm]] <- merge_palette_map(nm, ov, base)
    } else {
      base[names(ov)] <- ov            # entry-wise merge; override wins per key
      merged[[nm]] <- base
    }
  }
  do.call(ggstylekit::style_spec, merged)
}


## Internal: merge one per-series map field where the override or the default
## is a palette function. `with_defaults()` pins the named side's entries on
## the palette (the override wins by name), so a palette override keeps a
## preset's role colours and a named override keeps a preset's palette.
merge_palette_map <- function(field, override, default) {
  spec <- function(map) do.call(ggstylekit::style_spec, stats::setNames(list(map), field))
  ggstylekit::with_defaults(spec(override), spec(default))[[field]]
}


#' Internal helper: resolve and validate the `style` argument
#'
#' Every plot builder accepts `style = NULL` (use the family preset) or a
#' [ggstylekit::style_spec()] object. This helper applies the default and
#' aborts early, with a pmxhelpr-branded message, when `style` is anything
#' else; without it, a bad `style` fails deep inside `ggstylekit` or, for
#' [plot_vpc_legend()], with an uninformative R error.
#'
#' @param style `NULL` or a `ggstylekit_style_spec` object.
#' @param default A function returning the family preset (e.g. [style_dvtime]),
#'    called only when `style` is `NULL`.
#' @param call The call reported in the error.
#'
#' @return A `ggstylekit_style_spec` object.
#' @keywords internal
resolve_style <- function(style, default, call = rlang::caller_env()) {
  if (is.null(style)) return(default())
  if (!inherits(style, "ggstylekit_style_spec")) {
    cls <- paste(class(style), collapse = "/")
    pmx_abort(c(
      "{.arg style} must be a {.fn ggstylekit::style_spec} object, typically from a {.code style_*()} preset.",
      "x" = "Got {.cls {cls}}."
    ), "bad_style", call = call)
  }
  style
}
