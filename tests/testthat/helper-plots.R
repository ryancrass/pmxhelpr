## Built data of the first layer matching `geom` (and `stat`); fails the test
## when no layer matches.
styled_layer <- function(p, geom, stat = NULL) {
  b   <- suppressWarnings(ggplot2::ggplot_build(p))
  idx <- which(vapply(b$plot$layers, function(L)
    inherits(L$geom, geom) && (is.null(stat) || inherits(L$stat, stat)),
    logical(1)))[1]
  expect_false(is.na(idx))
  b$data[[idx]]
}

## The built plot's colour scale.
colour_scale <- function(p) {
  suppressWarnings(ggplot2::ggplot_build(p))$plot$scales$get_scales("colour")
}

## Name of the y-axis transformation ("identity", "log-10", ...).
y_trans <- function(p) {
  suppressMessages(suppressWarnings(ggplot2::layer_scales(p)$y$get_transformation()$name))
}

## Whether any layer of `p` uses a geom inheriting from `cls`.
has_geom <- function(p, cls) {
  any(vapply(p$layers, function(L) inherits(L$geom, cls), logical(1)))
}
