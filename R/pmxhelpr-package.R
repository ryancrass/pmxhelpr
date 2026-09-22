#' pmxhelpr: Helper Functions for Pharmacometrics
#'
#' Helper functions to make pharmacometrics workflows more standardized, efficient, and
#' reproducible. Includes helper functions for exploratory data analysis, goodness-of-fit
#' diagnostics, ands visual predictive check model evaluation.
#'
#' @section Getting started:
#' Recommended entry points by task:
#'
#' * Exploratory PK / PK-PD analysis -- [plot_dvtime()], [plot_dvconc()]
#' * Goodness-of-fit diagnostics -- [plot_gof()]
#' * Visual predictive checks -- [plot_vpc_cont()], [df_vpcstats()]
#' * Dose-proportionality assessment -- [plot_doseprop()], [df_doseprop()]
#' * Plot styling -- `style_*()` presets returning a [ggstylekit::style_spec()]
#'   for the `style` argument; adjust finished plots with [restyle_plot()],
#'   [reveal()], and [combine_styled_plots()]
#'
#' Full narrative documentation with worked examples lives on the package
#' website at <https://ryancrass.github.io/pmxhelpr/>.
#'
#' @keywords internal
"_PACKAGE"
