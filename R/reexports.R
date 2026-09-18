# Re-exported ggstylekit verbs and the legend constructor. pmxhelpr's `plot_*()`
# functions return styled ggplot objects; these verbs are the documented way to
# adjust them after the fact, so we surface them under `library(pmxhelpr)` alone.
# `legend_spec()` joins them because the `legends` field of the `style_*()`
# presets accepts nothing else -- it has no pmxhelpr substitute. Helpers that do
# have one (`style_spec()`, `set_style()`, `series_layer()`, ...) are
# intentionally not re-exported: the `style_*()` presets are the intended entry
# point for building styles.

#' @importFrom ggstylekit restyle_plot
#' @export
ggstylekit::restyle_plot

#' @importFrom ggstylekit reveal
#' @export
ggstylekit::reveal

#' @importFrom ggstylekit combine_styled_plots
#' @export
ggstylekit::combine_styled_plots

#' @importFrom ggstylekit legend_spec
#' @export
ggstylekit::legend_spec
