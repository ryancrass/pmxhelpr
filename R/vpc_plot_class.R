#' Test whether an object is a `pmx_vpc_plot`
#'
#' @description
#' Predicate that returns `TRUE` if `x` carries the `"pmx_vpc_plot"` class —
#' i.e., it is the ggplot returned by [plot_vpc_cont()], [plot_vpc_cens()], or
#' [plot_build_vpc()].
#'
#' @param x Object to test.
#' @return Logical scalar.
#' @family vpc
#' @export is_pmx_vpc_plot
#'
#' @examples
#' model <- model_mread_load(model = "pkmodel")
#' simout <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT %in% c(1, 2)),
#'                               model = model, replicates = 5,
#'                               dv_var = ODV)
#' p <- plot_vpc_cont(data = simout)
#' is_pmx_vpc_plot(p)         # TRUE
#' is_pmx_vpc_plot(simout)    # FALSE

is_pmx_vpc_plot <- function(x) {
  inherits(x, "pmx_vpc_plot")
}



#' Add a layer to a `pmx_vpc_plot` with a facet warning
#'
#' @description
#' S3 method for `+` applied to objects returned by [plot_vpc_cont()],
#' [plot_vpc_cens()], and [plot_build_vpc()]. Delegates to the standard ggplot2 `+` operation for all
#' layer types. When `e2` is a [ggplot2::facet_wrap()] or [ggplot2::facet_grid()]
#' specification, a warning is issued because the VPC summary statistics are
#' pre-computed and will not reflect the added stratification.
#' Use `strat_var` in [plot_vpc_cont()], [plot_vpc_cens()], or [df_vpcstats()]
#' to stratify both statistics and panels.
#'
#' Facets inside a `list()` are caught the same way. [restyle_plot()] and
#' [reveal()] keep the `pmx_vpc_plot` class, and `reveal(as = "facet")` and
#' `restyle_plot(facet = )` warn like `+ facet_*()`.
#' `restyle_plot(logy = TRUE)` on a censored VPC is ignored with a warning.
#'
#' The `pmx_vpc_plot` class is preserved on the result so that subsequent `+`
#' calls are also intercepted. Combining plots (patchwork `+`, `|`, `/`, or
#' [combine_styled_plots()]) returns a patchwork that is not a
#' `pmx_vpc_plot`, except a `+` chain ending on a VPC (`p2 + p`): patchwork
#' treats that figure as its last panel, so it keeps the class and a facet
#' added to it warns.
#'
#' @param e1 A `pmx_vpc_plot` object.
#' @param e2 A ggplot2 layer, scale, theme, facet, or other addable object.
#' @param p A `pmx_vpc_plot` object.
#' @param col,as,on,... Passed to the ggstylekit [reveal()] or [restyle_plot()]
#'    method.
#'
#' @return A `pmx_vpc_plot` object.
#' @method + pmx_vpc_plot
#' @export

`+.pmx_vpc_plot` <- function(e1, e2) {
  if (adds_facet(e2)) warn_vpc_facet()
  class(e1) <- setdiff(class(e1), "pmx_vpc_plot")
  as_pmx_vpc_plot(e1 + e2)
}

#' @rdname plus-.pmx_vpc_plot
#' @exportS3Method ggstylekit::restyle_plot
restyle_plot.pmx_vpc_plot <- function(p, ...) {
  dots <- list(...)
  if (!is.null(dots[["facet"]])) warn_vpc_facet()
  if (isTRUE(dots[["logy"]]) && is_vpc_cens_plot(p)) {
    warn_vpc_cens_logy()
    dots$logy <- NULL
  }
  ## A cens VPC's obs points take the obs line's colour (vpc_cens_style()), so
  ## a recoloured obs line carries its colour to the points unless the call
  ## sets obs_point itself.
  colors <- dots[["colors"]]
  if (is.character(colors) && "obs_median_line" %in% names(colors) &&
      !"obs_point" %in% names(colors) && is_vpc_cens_plot(p)) {
    dots$colors[["obs_point"]] <- colors[["obs_median_line"]]
  }
  ## Warned once above; drop the class so the ggplot method's own additions do
  ## not re-trigger `+.pmx_vpc_plot`.
  class(p) <- setdiff(class(p), "pmx_vpc_plot")
  as_pmx_vpc_plot(rlang::exec(ggstylekit::restyle_plot, p, !!!dots))
}

#' @rdname plus-.pmx_vpc_plot
#' @exportS3Method ggstylekit::reveal
reveal.pmx_vpc_plot <- function(p, col, ..., as = NULL, on = NULL) {
  if (any(c("facet", "facets") %in% as)) warn_vpc_facet()
  ## The ggplot method reads `col` with ensym(), so forward the caller's
  ## symbol itself rather than this method's `col` argument.
  col <- rlang::ensym(col)
  class(p) <- setdiff(class(p), "pmx_vpc_plot")
  as_pmx_vpc_plot(rlang::inject(ggstylekit::reveal(p, !!col, ..., as = as, on = on)))
}


## Internal: TRUE when `e2` adds a facet, directly or inside a plain list.
adds_facet <- function(e2) {
  if (inherits(e2, "FacetWrap") || inherits(e2, "FacetGrid")) return(TRUE)
  if (is.list(e2) && identical(class(e2), "list")) {
    return(any(vapply(e2, adds_facet, logical(1))))
  }
  FALSE
}

## Internal: TRUE for a censored VPC, the only VPC whose layers draw the
## observed BLQ proportion (the `cens` columns in vpc_roles.R).
is_vpc_cens_plot <- function(p) {
  any(vapply(p$layers, function(L) {
    !is.null(L$mapping$y) && identical(rlang::as_label(L$mapping$y), "obs_prop_blq")
  }, logical(1)))
}

## Internal: the shared warning for faceting a VPC after its statistics exist.
warn_vpc_facet <- function() {
  pmx_warn(c(
    "Adding {.code facet_*()} to a VPC plot produces incorrect VPC statistics.",
    "i" = "Summary statistics are pre-computed before plotting.",
    "i" = "Pass {.arg strat_var} to {.fn plot_vpc_cont}, {.fn plot_vpc_cens}, or {.fn df_vpcstats} to stratify both statistics and panels correctly."
  ), "vpc_facet")
}

## Internal: the shared warning for a log y axis requested on a censored VPC.
warn_vpc_cens_logy <- function() {
  pmx_warn(c(
    "{.code logy = TRUE} is ignored for a censored VPC.",
    "i" = "The proportion BLQ is drawn on a fixed 0-1 axis; it is 0 in bins with no BLQ observations, which a log axis cannot show."
  ), "ignored_arg")
}

## Internal: (re)attach the `pmx_vpc_plot` class once, at the front. A
## patchwork is left as is: patchwork treats it as its last panel, which
## carries its own class (VPC or not).
as_pmx_vpc_plot <- function(p) {
  if (inherits(p, "patchwork")) return(p)
  class(p) <- c("pmx_vpc_plot", setdiff(class(p), "pmx_vpc_plot"))
  p
}
