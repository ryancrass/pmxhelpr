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

## Compact, platform-stable text description of a built plot for snapshots:
## per layer the geom/stat, row count, fixed aesthetics, and position ranges;
## then scale transformations, facets, and labels. Numbers use 4 significant
## digits so cross-platform floating-point noise does not change the output.
layer_summary <- function(p) {
  b <- suppressMessages(suppressWarnings(ggplot2::ggplot_build(p)))
  num <- function(v) format(signif(v, 4), trim = TRUE)
  aes_line <- function(d) {
    cols <- intersect(c("colour", "fill", "alpha", "linetype", "linewidth",
                        "shape", "size"), names(d))
    vapply(cols, function(a) {
      u <- unique(d[[a]])
      u <- if (is.numeric(u)) num(u) else as.character(u)
      more <- if (length(u) > 6) ",..." else ""
      paste0(a, "=", paste(utils::head(u, 6), collapse = ","), more)
    }, character(1))
  }
  pos_line <- function(d) {
    cols <- intersect(c("x", "y", "ymin", "ymax", "xmin", "xmax",
                        "xintercept", "yintercept"), names(d))
    vapply(cols, function(a) {
      v <- as.numeric(d[[a]])
      v <- v[is.finite(v)]
      if (length(v) == 0) return(NA_character_)
      paste0(a, ": [", num(min(v)), ", ", num(max(v)), "] mean=", num(mean(v)))
    }, character(1))
  }
  trans <- function(aes) {
    s <- b$plot$scales$get_scales(aes)
    if (is.null(s)) return("default")
    if (inherits(s, "ScaleDiscrete")) return("discrete")
    s$get_transformation()$name
  }
  for (i in seq_along(b$data)) {
    L <- b$plot$layers[[i]]
    d <- b$data[[i]]
    cat(sprintf("layer %d: %s / %s  rows=%d\n", i, class(L$geom)[1],
                class(L$stat)[1], nrow(d)))
    a <- aes_line(d)
    if (length(a)) cat("  ", paste(a, collapse = " "), "\n", sep = "")
    for (pl in stats::na.omit(pos_line(d))) cat("  ", pl, "\n", sep = "")
  }
  cat("scales: x=", trans("x"), " y=", trans("y"), "\n", sep = "")
  fp <- b$plot$facet$params
  facet_vars <- names(c(fp$facets, fp$rows, fp$cols))
  cat("facet: ", class(b$plot$facet)[1],
      if (length(facet_vars)) paste0("(", paste(facet_vars, collapse = ", "), ")"),
      " panels=", nrow(b$layout$layout), "\n", sep = "")
  labs <- if (exists("get_labs", asNamespace("ggplot2"))) {
    suppressMessages(suppressWarnings(ggplot2::get_labs(p)))
  } else p$labels
  for (nm in intersect(c("x", "y", "caption"), names(labs))) {
    if (is.character(labs[[nm]])) cat("label ", nm, ": ", labs[[nm]], "\n", sep = "")
  }
  invisible(NULL)
}
