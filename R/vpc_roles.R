## ---- VPC role table -------------------------------------------------------
## One entry per VPC role, in plot drawing order. Role names are the keys of
## plot_vpc_shown() and style_vpc(). The builders (vpc_build.R) and the
## legend (vpc_legend.R) both walk this table.
##
##   geom     "ribbon", "line", "point", or "hline" (the LLOQ line, drawn by
##            loq_line_layer())
##   cont     stats columns per layer for the continuous VPC: a ribbon takes a
##            c(ymin, ymax) pair, a line a single y column; one layer per
##            element. `pc_`-prefixed for the prediction-corrected flavor.
##   cens     the same for the censored (BLQ-proportion) VPC; NULL = not drawn
##   legend   the legend scale the role's key joins: "shape", "linetype", "fill"
##
## obs_point and loq_line draw from their own data and are placed by the
## builders at their table position; their `cont`/`cens` are NULL.
.vpc_roles <- list(
  sim_pi_area     = list(geom = "ribbon", legend = "fill",
                         cont = list(c("sim_low_med", "sim_hi_med"))),
  sim_pi_ci       = list(geom = "ribbon", legend = "fill",
                         cont = list(c("sim_low_low", "sim_low_hi"),
                                     c("sim_hi_low", "sim_hi_hi"))),
  sim_pi_line     = list(geom = "line", legend = "linetype",
                         cont = list("sim_low_med", "sim_hi_med")),
  sim_median_ci   = list(geom = "ribbon", legend = "fill",
                         cont = list(c("sim_med_low", "sim_med_hi")),
                         cens = list(c("sim_prop_blq_low", "sim_prop_blq_hi"))),
  sim_median_line = list(geom = "line", legend = "linetype",
                         cont = list("sim_med_med"),
                         cens = list("sim_prop_blq_med")),
  obs_median_line = list(geom = "line", legend = "linetype",
                         cont = list("obs_med"),
                         cens = list("obs_prop_blq")),
  obs_pi_line     = list(geom = "line", legend = "linetype",
                         cont = list("obs_low", "obs_hi")),
  loq_line        = list(geom = "hline", legend = "linetype"),
  obs_point       = list(geom = "point", legend = "shape")
)

## Legend layer order (proxy geoms) and key order within each scale. Keys are
## grouped as points, lines (LLOQ last), then intervals.
.vpc_legend_layer_order <- c("obs_point", "loq_line", "obs_median_line",
                             "obs_pi_line", "sim_median_line", "sim_pi_line",
                             "sim_median_ci", "sim_pi_ci", "sim_pi_area")
.vpc_legend_key_order <- c("obs_point", "obs_median_line", "obs_pi_line",
                           "sim_median_line", "sim_pi_line", "loq_line",
                           "sim_median_ci", "sim_pi_ci", "sim_pi_area")


#' Internal: legend label for a VPC role
#'
#' @param role A `.vpc_roles` name.
#' @param ci,pi,type As in [plot_vpc_legend()].
#' @param loq_lab The LLOQ key label (for `loq_line`).
#' @return A string, or `NULL` when the role has no key for `type`.
#' @noRd
vpc_role_label <- function(role, ci, pi, type, loq_lab = NULL) {
  sim_ci <- paste0("Sim ", ci * 100, "% CI")
  cens <- type == "cens"
  pi_lo <- paste0(pi[1] * 100, "th")
  pi_hi <- paste0(pi[2] * 100, "th")
  switch(role,
    obs_point       = "Obs",
    loq_line        = loq_lab,
    obs_median_line = if (cens) "Obs Prop BLQ" else "Obs Med",
    obs_pi_line     = if (!cens) paste0("Obs ", pi_lo, " and ", pi_hi),
    sim_median_line = if (cens) "Sim Prop BLQ" else "Sim Med",
    sim_pi_line     = if (!cens) paste0("Sim ", pi_lo, " - ", pi_hi),
    sim_median_ci   = paste0(sim_ci, if (cens) " Prop BLQ" else " Med"),
    sim_pi_ci       = if (!cens) paste0(sim_ci, " ", pi_lo, " and ", pi_hi),
    sim_pi_area     = if (!cens) paste0("Sim ", pi_lo, " - ", pi_hi)
  )
}


#' Internal: the tagged statistic layers of a VPC role
#'
#' One `series_layer()` per column set of `role` for `type`; aesthetics are
#' filled by [ggstylekit::style_plot()] from the role's style entries.
#' With `bin_style = "rect"`, each ribbon (a simulated interval) becomes a
#' `geom_rect()` spanning its bin (`BIN_LOW`-`BIN_HI`) under the same role;
#' lines stay lines through the bin midpoints, each followed by a point at
#' every bin midpoint (`mid_points`) tagged with the line's role, so it takes
#' the line's colour.
#'
#' @param role A `.vpc_roles` name with `ribbon` or `line` geom.
#' @param type `"cont"` or `"cens"`.
#' @param col Column-name function (adds the `pc_` prefix when needed).
#' @param bin_style `"line"` (ribbons through bin midpoints) or `"rect"`.
#' @param mid_points Whether a line gets points at the bin midpoints; on by
#'    default with `bin_style = "rect"`.
#' @return A list of layers (empty when the role is not drawn for `type`).
#' @noRd
vpc_role_layers <- function(role, type, col = identity, bin_style = "line",
                            mid_points = bin_style == "rect") {
  spec <- .vpc_roles[[role]]
  geom <- if (spec$geom == "ribbon" && bin_style == "rect") "rect" else spec$geom
  layers <- lapply(spec[[type]], function(cols) {
    layer <- switch(geom,
      ribbon = ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[[col(cols[1])]],
                                                 ymax = .data[[col(cols[2])]])),
      rect   = ggplot2::geom_rect(ggplot2::aes(xmin = .data[[.vpc_bin_bounds[1]]],
                                               xmax = .data[[.vpc_bin_bounds[2]]],
                                               ymin = .data[[col(cols[1])]],
                                               ymax = .data[[col(cols[2])]])),
      line   = ggplot2::geom_line(ggplot2::aes(y = .data[[col(cols)]]))
    )
    out <- list(ggstylekit::series_layer(layer, role))
    if (geom == "line" && isTRUE(mid_points)) {
      out[[2]] <- ggstylekit::series_layer(
        ggplot2::geom_point(ggplot2::aes(y = .data[[col(cols)]]), na.rm = TRUE), role)
    }
    out
  })
  unlist(layers, recursive = FALSE)
}
