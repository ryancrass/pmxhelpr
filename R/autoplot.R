#' Draw a `pmx_stats` container with `autoplot()`
#'
#' @description
#' `autoplot()` methods for the cacheable stats containers. Each one calls the
#' container's builder with every other argument passed through:
#'
#' * `vpc_stats` ([df_vpcstats()]): [plot_build_vpc()], e.g.
#'   `autoplot(stats, type = "cens")` or `autoplot(stats, pcvpc = TRUE)`.
#'   `loq` still defaults from `stats$config$loq` unless passed.
#' * `doseprop_stats` ([df_doseprop()]): [plot_build_doseprop()].
#' * `forest_stats` ([df_forest()]): [plot_build_forest()]; pass `metric`
#'   when the container holds several metrics.
#'
#' Errors are reported by the builder (e.g. ``Error in `plot_build_vpc()` ``).
#' `autoplot()` is re-exported from ggplot2, so it is available under
#' `library(pmxhelpr)` alone.
#'
#' @param object A `vpc_stats`, `doseprop_stats`, or `forest_stats` container.
#' @param ... Arguments passed to the builder.
#'
#' @return The builder's plot: a `pmx_vpc_plot` for `vpc_stats`, a `ggplot`
#'    otherwise.
#' @seealso [plot_build_vpc()], [plot_build_doseprop()], [plot_build_forest()]
#' @name autoplot.pmx_stats
#'
#' @examples
#' model <- model_mread_load(model = "pkmodel")
#' simout <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT %in% c(1, 2)),
#'                               model = model, replicates = 5,
#'                               dv_var = ODV)
#' vs <- df_vpcstats(simout, loq = 1)
#' autoplot(vs)
#' autoplot(vs, type = "cens")
#' autoplot(df_forest(data_sad_pkforest), metric = "AUCRATIO")
NULL


#' @rdname autoplot.pmx_stats
#' @exportS3Method ggplot2::autoplot
autoplot.vpc_stats <- function(object, ...) {
  plot_build_vpc(object, ...)
}


#' @rdname autoplot.pmx_stats
#' @exportS3Method ggplot2::autoplot
autoplot.doseprop_stats <- function(object, ...) {
  plot_build_doseprop(object, ...)
}


#' @rdname autoplot.pmx_stats
#' @exportS3Method ggplot2::autoplot
autoplot.forest_stats <- function(object, ...) {
  plot_build_forest(object, ...)
}
