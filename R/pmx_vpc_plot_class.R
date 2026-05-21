#' Test whether an object is a `pmx_vpc_plot`
#'
#' @description
#' Predicate that returns `TRUE` if `x` carries the `"pmx_vpc_plot"` class —
#' i.e., it is the ggplot returned by [plot_vpc_cont()] or [plot_build_vpc()].
#'
#' @param x Object to test.
#' @return Logical scalar.
#' @family vpc
#' @export is_pmx_vpc_plot
#'
#' @examples
#' model <- model_mread_load(model = "pkmodel")
#' simout <- df_mrgsim_replicate(data = data_sad, model = model, replicates = 5,
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
#' S3 method for `+` applied to objects returned by [plot_vpc_cont()] and
#' [plot_build_vpc()]. Delegates to the standard ggplot2 `+` operation for all
#' layer types. When `e2` is a [ggplot2::facet_wrap()] or [ggplot2::facet_grid()]
#' specification, a warning is issued because the VPC summary statistics are
#' pre-computed and will not reflect the added stratification.
#' Use `strat_var` in [plot_vpc_cont()] to stratify both statistics and panels.
#'
#' The `pmx_vpc_plot` class is preserved on the result so that subsequent `+`
#' calls are also intercepted.
#'
#' @param e1 A `pmx_vpc_plot` object.
#' @param e2 A ggplot2 layer, scale, theme, facet, or other addable object.
#'
#' @return A `pmx_vpc_plot` object.
#' @method + pmx_vpc_plot
#' @export

`+.pmx_vpc_plot` <- function(e1, e2) {
  if (inherits(e2, "FacetWrap") || inherits(e2, "FacetGrid")) {
    rlang::warn(
      c(
        "Adding `facet_*()` to a `plot_vpc_cont()` plot produces incorrect VPC statistics.",
        "i" = "Summary statistics are pre-computed before plotting.",
        "i" = "Pass `strat_var` to `plot_vpc_cont()` to stratify both statistics and panels correctly."
      )
    )
  }
  class(e1) <- setdiff(class(e1), "pmx_vpc_plot")
  p <- e1 + e2
  class(p) <- c("pmx_vpc_plot", class(p))
  p
}
