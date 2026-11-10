
#' Plot a legend for a visual predictive check (VPC)
#'
#' @param ci Numeric confidence level for simulation intervals (e.g., `0.90` for 90% CI).
#'    Should match argument passed to [plot_vpc_cont()]. Default is `0.90`.
#' @param pi prediction intervals plotted. Should match argument passed to [plot_vpc_cont()]. Default is c(0.05, 0.95).
#'    Ignored when `type = "cens"` (cens VPCs do not have prediction intervals).
#' @param loq Numeric scalar or vector of LLOQ values to label in the legend,
#'    or `NULL` to omit. One legend entry is drawn at the highest value,
#'    matching the single LLOQ line per panel, with the style's `loq_line`
#'    linetype and linewidth. Pass `stats$config$loq` from
#'    a [df_vpcstats()] result to mirror the reference lines drawn by
#'    [plot_build_vpc()].
#' @param show_loq_value Logical. If `TRUE` (default), each unique `loq` value
#'    gets its own legend entry labeled `"LLOQ = <value>"`. If `FALSE`, a
#'    single entry labeled `"LLOQ"` is drawn regardless of how many values
#'    `loq` holds. Ignored when `loq` is `NULL`.
#' @param style A [ggstylekit::style_spec()] controlling legend aesthetics.
#'    Defaults to [style_vpc()]; view the defaults by running `style_vpc()`
#'    with no arguments. Should match the `style` passed to [plot_vpc_cont()].
#'    The per-series maps supply each key's color, fill, shape, linetype, and
#'    linewidth; `legend.title.position` and `legend.title.hjust` place the
#'    legend titles. Other style fields do not apply to the legend panel.
#' @param type One of `"cont"` (default) or `"cens"`. Selects the labels and
#'    layer set the legend describes. Under `"cont"`, the central-tendency
#'    entries are labeled `"Obs Med"`, `"Sim Med"`, and `"Sim <ci>% CI Med"`
#'    and the pi entries (`"Obs <p1>th and <p2>th"`, `"Sim <p1>th - <p2>th"`,
#'    `"Sim <ci>% CI <p1>th and <p2>th"`) are included when their `shown`
#'    keys are on. Under `"cens"`, the three central-tendency labels are
#'    relabeled to `"Obs Prop BLQ"`, `"Sim Prop BLQ"`, `"Sim <ci>% CI Prop BLQ"`
#'    and all pi-related entries are suppressed regardless of `shown` (cens
#'    VPCs have no prediction interval).
#' @param ... Other arguments passed to [ggplot2::theme()], applied after the
#'    style so they override it.
#' @param lloq Deprecated alias of `loq`; warns. Do not pass both.
#'
#' @inheritParams plot_vpc_cont
#' @family vpc
#' @return a ggplot2 object
#' @export plot_vpc_legend
#'
#' @examples
#'plot_vpc_legend()
#'plot_vpc_legend(
#'pi = c(0.025, 0.975),
#'ci = 0.95,
#'  shown = plot_vpc_shown(obs_point = FALSE, obs_pi_line = TRUE,
#'  sim_pi_line = FALSE, sim_pi_area = FALSE, sim_pi_ci = TRUE,
#'  obs_median_line = TRUE,
#'  sim_median_line = FALSE, sim_median_ci = TRUE))
#'## Cens VPC legend
#'plot_vpc_legend(type = "cens", loq = 1)
#'## LLOQ entry without its value
#'plot_vpc_legend(loq = c(0.5, 1), show_loq_value = FALSE)

plot_vpc_legend <- function(ci = 0.90,
                        pi = c(0.05, 0.95),
                        shown = NULL,
                        loq = NULL,
                        show_loq_value = TRUE,
                        style = NULL,
                        type = c("cont", "cens"),
                        ...,
                        lloq = NULL){

  type <- match.arg(type)
  check_quantile_scalar(ci, "ci")
  check_quantile_pair(pi, "pi")
  if (!is.null(lloq)) {
    if (!is.null(loq)) {
      pmx_abort("Pass {.arg loq} only; {.arg lloq} is its deprecated alias.", "bad_arg")
    }
    pmx_warn("{.arg lloq} is deprecated; use {.arg loq}.", "deprecated")
    loq <- lloq
  }

  ## Legend aesthetics come from the style, per role; a cens VPC colours its
  ## obs points like the obs line, and its "Obs" key follows.
  vpcstyle <- resolve_style(style, style_vpc)
  if (type == "cens") vpcstyle <- vpc_cens_style(vpcstyle)

  #shown elements for legend based on settings in plot_vpc_cont
  nlist <- merge_element(shown, plot_vpc_shown())
  if (!isTRUE(nlist$loq_line)) loq <- NULL

  if (!is.null(loq)) check_numeric_strict(loq, "loq")
  check_boolean(show_loq_value, "show_loq_value")
  ## One key at the highest LLOQ, matching the single line drawn per panel.
  lloq_lab <- if (show_loq_value && !is.null(loq)) paste0("LLOQ = ", max(loq)) else "LLOQ"
  df_lloq <- if (!is.null(loq)) {
    data.frame(x = rep(NA_real_, length(lloq_lab)),
               y = rep(NA_real_, length(lloq_lab)),
               lloq_lab = factor(lloq_lab, levels = lloq_lab))
  } else NULL

  ## Keys come from the role table (vpc_roles.R). A role draws a key when it is
  ## shown and has a label for `type` (a cens VPC has no prediction-interval
  ## roles); the LLOQ draws when a `loq` is given.
  labels <- lapply(stats::setNames(nm = names(.vpc_roles)), vpc_role_label,
                   ci = ci, pi = pi, type = type, loq_lab = lloq_lab)
  drawn <- vapply(names(.vpc_roles), function(r) {
    if (r == "loq_line") !is.null(loq) else isTRUE(nlist[[r]]) && !is.null(labels[[r]])
  }, logical(1))

  ## Proxy geoms on an empty frame: each draws only its legend key.
  df <- data.frame(x=NA_real_, y=NA_real_)
  plot_blank <- ggplot2::ggplot(data = df, ggplot2::aes(.data$x, .data$y))
  key_layer <- function(role) {
    a <- series_aes(vpcstyle, role)
    lab <- labels[[role]]
    switch(.vpc_roles[[role]]$legend,
      shape = ggplot2::geom_point(ggplot2::aes(shape = !!lab),
                                  color = a$colour, size = a$size, na.rm = TRUE),
      linetype = if (role == "loq_line") {
        ggplot2::geom_line(data = df_lloq, ggplot2::aes(linetype = .data$lloq_lab),
                           color = a$colour, linewidth = a$linewidth, na.rm = TRUE)
      } else {
        ggplot2::geom_line(ggplot2::aes(linetype = !!lab),
                           color = a$colour, linewidth = a$linewidth, na.rm = TRUE)
      },
      fill = ggplot2::geom_rect(ggplot2::aes(xmin = .data$x, ymin = .data$y,
                                             xmax = .data$x, ymax = .data$y,
                                             fill = !!lab),
                                alpha = a$alpha, na.rm = TRUE)
    )
  }

  ## Manual-scale values keyed by label, in key order, for one legend scale.
  key_values <- function(scale, field) {
    vals <- NULL
    for (r in .vpc_legend_key_order) {
      if (!drawn[[r]] || .vpc_roles[[r]]$legend != scale) next
      vals <- c(vals, stats::setNames(series_aes(vpcstyle, r)[[field]], labels[[r]]))
    }
    vals
  }
  shape_vals    <- key_values("shape", "shape")
  linetype_vals <- key_values("linetype", "linetype")
  fill_vals     <- key_values("fill", "fill")

  layer_roles <- .vpc_legend_layer_order[drawn[.vpc_legend_layer_order]]
  plot <- plot_blank +
    lapply(layer_roles, key_layer) +
    {if (drawn[["obs_point"]]) ggplot2::scale_shape_manual(name = "Points",
                                                           breaks = names(shape_vals),
                                                           values = shape_vals)} +
    ggplot2::scale_linetype_manual(name = "Lines",
                                   breaks = names(linetype_vals),
                                   values = linetype_vals) +
    ggplot2::scale_fill_manual(name = "Intervals",
                               breaks = names(fill_vals),
                               values = fill_vals) +
    ggplot2::theme_void()+
    ggplot2::theme(legend.position = "inside",
                   legend.box = "horizontal",
                   legend.title = ggplot2::element_text(size = 10),
                   legend.text = ggplot2::element_text(size = 8)) +
    ## Legend-title placement/justification follow the style so the legend
    ## panel matches the plot it describes; `...` is applied last so explicit
    ## theme() overrides still win.
    legend_style_theme(vpcstyle) +
    ggplot2::theme(...) +
    ggplot2::guides(shape = ggplot2::guide_legend(order=1),
                    linetype = ggplot2::guide_legend(order=2),
                    fill = ggplot2::guide_legend(order=3))

  return(plot)
}


#' Internal helper: legend-title theme elements from a style_spec
#'
#' [plot_vpc_legend()] draws a standalone legend panel on `theme_void()`, so
#' the style's `theme` field does not apply. The two legend-title fields of
#' [ggstylekit::style_spec()] are mirrored here the way `ggstylekit` applies
#' them to a plot, so the legend and its plot agree.
#'
#' @param style A `ggstylekit_style_spec`.
#'
#' @return A ggplot2 theme carrying `legend.title.position` and/or the
#'   `legend.title` `hjust`; empty when neither field is set.
#' @keywords internal
legend_style_theme <- function(style) {
  args <- list()
  if (!is.null(style$legend.title.position)) {
    args$legend.title.position <- style$legend.title.position
  }
  if (!is.null(style$legend.title.hjust)) {
    hj <- style$legend.title.hjust
    if (is.character(hj)) hj <- c(left = 0, center = 0.5, right = 1)[[hj]]
    args$legend.title <- ggplot2::element_text(hjust = hj)
  }
  do.call(ggplot2::theme, args)
}
