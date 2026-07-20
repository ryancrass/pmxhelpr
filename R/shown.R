#' GOF layer visibility settings
#'
#' Constructor and factory for controlling which GOF overlay layers are displayed.
#' Call with no arguments to view defaults. Pass overrides to customize.
#'
#' @param obs Show observed data points/lines. Default is `TRUE`.
#' @param dv Show DV central tendency. Default is `TRUE`.
#' @param pred Show PRED central tendency. Default is `TRUE`.
#' @param ipred Show IPRED central tendency. Default is `TRUE`.
#'
#' @family goodness-of-fit
#' @return A named list of logicals
#' @export
#'
#' @examples
#' plot_gof_shown()
#' plot_gof_shown(pred = FALSE)
plot_gof_shown <- function(obs = NULL, dv = NULL, pred = NULL, ipred = NULL) {
  defaults <- list(obs = TRUE, dv = TRUE, pred = TRUE, ipred = TRUE)
  user <- compact(list(obs = obs, dv = dv, pred = pred, ipred = ipred))
  out <- defaults
  for (nm in names(user)) out[[nm]] <- user[[nm]]
  out
}


#' VPC layer visibility settings
#'
#' Constructor and factory for controlling which VPC layers are displayed.
#' Call with no arguments to view defaults. Pass overrides to customize.
#' Each element maps 1:1 with a [style_vpc()] role/series key.
#'
#' @param obs_point Show observed data points. Default is `TRUE`.
#' @param obs_pi_line Show observed quantile lines. Default is `TRUE`.
#' @param obs_median_line Show observed median line. Default is `TRUE`.
#' @param sim_pi_line Show simulated prediction interval lines. Default is `FALSE`.
#' @param sim_pi_ci Show simulated prediction interval CI ribbons. Default is `TRUE`.
#' @param sim_pi_area Show simulated prediction interval as shaded area. Default is `FALSE`.
#' @param sim_median_line Show simulated median line. Default is `FALSE`.
#' @param sim_median_ci Show simulated median CI ribbon. Default is `TRUE`.
#'
#' @family vpc
#' @return A named list of logicals
#' @export
#'
#' @examples
#' plot_vpc_shown()
#' plot_vpc_shown(obs_point = FALSE, sim_pi_line = TRUE)
plot_vpc_shown <- function(obs_point = NULL, obs_pi_line = NULL, obs_median_line = NULL,
                           sim_pi_line = NULL, sim_pi_ci = NULL, sim_pi_area = NULL,
                           sim_median_line = NULL, sim_median_ci = NULL) {
  defaults <- list(
    obs_point = TRUE, obs_pi_line = TRUE, obs_median_line = TRUE,
    sim_pi_line = FALSE, sim_pi_ci = TRUE, sim_pi_area = FALSE,
    sim_median_line = FALSE, sim_median_ci = TRUE
  )
  user <- compact(list(obs_point = obs_point, obs_pi_line = obs_pi_line,
                       obs_median_line = obs_median_line, sim_pi_line = sim_pi_line,
                       sim_pi_ci = sim_pi_ci, sim_pi_area = sim_pi_area,
                       sim_median_line = sim_median_line, sim_median_ci = sim_median_ci))
  out <- defaults
  for (nm in names(user)) out[[nm]] <- user[[nm]]
  out
}
