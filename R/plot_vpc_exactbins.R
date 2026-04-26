#' Plot a visual predictive check (VPC) with exact time bins
#'
#' @description  `plot_vpc_exactbins()` generates a VPC plot using exact time bins
#' and returns a `ggplot2` object.
#'
#' @param sim Input dataset. Must contain the following variables: `"ID"`, `"TIME"`
#' @param strat_var Stratification variable.
#'    Accepts bare names or strings. Currently, only a single stratifying variable is supported.
#' @param pcvpc logical for prediction correction. Default is `FALSE`.
#' @param loq Numeric value of the lower limit of quantification (LLOQ) for the assay.
#'    Specifying this argument implies that the observed variable`OBSDV` is missing (`NA`)
#'    and `MDV = 1` where < `loq` in `sim`.
#'    For standard VPCs (`pcvpc = FALSE`), all `MDV` values are set to 0 so that
#'    all observations (including BLQ) are included in summary statistics.
#'    Dashed horizontal line plotted at `loq` by default (controlled via `vpc_theme`).
#' @param min_bin_count Minimum number of quantifiable observations in exact bin for inclusion
#'    in binned plot layers. This argument drops small bins from summary statistic calculation
#'    but retains these observations in the observed data points.
#' @param show_rep Display number of replicates as a plot caption. Default is `TRUE`.
#' @param lower_bound Lower bound of the dependent variable for prediction correction. Default is `0`.
#'
#' @param shown Named list of logicals specifying which layers to include on the plot.
#'    Defaults can be obtained by calling `plot_vpc_shown()` with no arguments.
#'
#'    Defaults are:
#'    + Observed points: `obs_dv` = TRUE.
#'    + Observed quantiles: `obs_ci` = TRUE
#'    + Simulated inter-quantile range:`pi` = FALSE
#'    + Simulated inter-quantile area: `pi_as_area` = FALSE
#'    + Simulated Quantile CI: `pi_ci` = TRUE
#'    + Observed Median: `obs_median` = TRUE
#'    + Simulated Median: `sim_median` = FALSE
#'    + Simulated Median CI: `sim_median_ci` = TRUE
#'
#' @param vpc_theme Named list of aesthetic parameters for the plot.
#'    Defaults for `pmxhelpr` can be obtained by running `plot_vpc_theme` with no arguments.
#'
#' @param pi Numeric vector of length 2 specifying prediction interval quantiles. Default is `c(0.05, 0.95)`.
#' @param ci Numeric vector of length 2 specifying confidence interval quantiles. Default is `c(0.05, 0.95)`.
#' @param vpcstats Logical. If `TRUE`, return a list of computed VPC statistics instead of a plot. Default is `FALSE`.
#'
#' @inheritParams df_mrgsim_replicate
#' @inheritParams plot_dvtime
#'
#' @return A list containing calculated VPC information (when `vpcdb=TRUE`), or a ggplot2 object (default)
#' @export plot_vpc_exactbins
#'
#' @examples
#' model <- model_mread_load(model = "pkmodel")
#' data_sad_pk <- dplyr::filter(data_sad, CMT %in% c(1,2))
#' simout <- df_mrgsim_replicate(data = data_sad_pk, model = model, replicates = 100,
#' dv_var = ODV,
#' num_vars = c("CMT", "EVID", "MDV", "NTIME", "LLOQ", "WTBL", "FOOD"),
#' char_vars = c("USUBJID", "PART"),
#' irep_name = SIM)
#'
#' vpc_plot <- plot_vpc_exactbins(
#' sim = simout,
#' pcvpc = TRUE,
#' pi = c(0.05, 0.95),
#' ci = c(0.05, 0.95))

plot_vpc_exactbins <- function(sim,
                               time_vars = c(TIME = "TIME",
                                             NTIME = "NTIME"),
                               output_vars = c(PRED = "PRED",
                                               IPRED = "IPRED",
                                               SIMDV = "SIMDV",
                                               OBSDV = "OBSDV"),
                               strat_var = NULL,
                               pcvpc = FALSE,
                               loq = NULL,
                               irep_name = SIM,
                               min_bin_count=1,
                               show_rep = TRUE,
                               lower_bound = 0,
                               shown = NULL,
                               vpc_theme = NULL,
                               timeu = "hours",
                               n_breaks = 8,
                               pi = c(0.05, 0.95),
                               ci = c(0.05, 0.95),
                               vpcstats = FALSE)
{

  #NSE Stratification and Replicate Variables
  strat_var_str  <- capture_col(rlang::enquo(strat_var))
  irep_name_str  <- rlang::as_name(rlang::ensym(irep_name))

  #Define Time and Output Variables
  time_vars <- init_time_vars(time_vars)
  output_vars <- list_update(output_vars, c(PRED = "PRED",
                                            IPRED = "IPRED",
                                            SIMDV = "SIMDV",
                                            OBSDV = "OBSDV"))

  #Checks
  check_df(sim, "sim")
  check_varsindf(sim, time_vars[["TIME"]], "sim", "time_vars")
  check_varsindf(sim, time_vars[["NTIME"]], "sim", "time_vars")
  if(pcvpc == TRUE) {check_varsindf(sim, output_vars[["PRED"]], "sim", "output_vars")}
  check_varsindf(sim, output_vars[["SIMDV"]], "sim", "output_vars")
  check_varsindf(sim, output_vars[["OBSDV"]], "sim", "output_vars")
  check_varsindf(sim, "MDV", "sim", "MDV")
  check_varsindf(sim, strat_var_str, "sim", "strat_var")
  check_varsindf(sim, irep_name_str, "sim", "irep_name")
  if(!is.null(strat_var_str)) {check_factor(sim, strat_var_str, "strat_var")}
  if(!is.null(loq)) {check_numeric_strict(loq, "loq")}

  #Rename Output and Time Variables to Standard Names
  sim <- rename_time_vars(sim, time_vars, output_vars)

  ##Prediction-correct if pcvpc = TRUE
  if(isTRUE(pcvpc)) {
    sim <- sim |>
      dplyr::filter(MDV == 0) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c("NTIME", strat_var_str, "CMT")))) |>
      dplyr::mutate(DV = var_pc(OBSDV, PRED, lower_bound),
                    DVS = var_pc(SIMDV, PRED, lower_bound)) |>
      dplyr::ungroup()
  } else {
    sim <- sim |>
      dplyr::rename(DV=OBSDV,DVS=SIMDV) |>
      dplyr::mutate(MDV = ifelse(!is.null(loq), 0, MDV))
  }

  #Isolate the observed data
  obs <- sim |>
    dplyr::filter(.data[[irep_name_str]] == 1 & MDV == 0)

  ##Compute VPC Statistics
  vpcstat <- rlang::inject(df_vpcstats(
    sim = sim,
    pi = pi,
    ci = ci,
    strat_var = !!strat_var_str,
    sim_dv_var = DVS,
    obs_dv_var = DV,
    irep_name = !!irep_name_str,
    loq = loq
  ))

  ##Return database if requested
  if(isTRUE(vpcstats)) {
    return(vpcstat)
  }

  ##Set vpc aesthetics and theme
  show_vpc <- list_update(shown, plot_vpc_shown())
  vpctheme <- list_update(vpc_theme, plot_vpc_theme())

  #Determine Breaks
  xbreaks <- breaks_time(x = unique(vpcstat$NTIME), unit = timeu, n = n_breaks)

  ##Build VPC Plot
  plot <- rlang::inject(plot_vpc(
    vpcstats = dplyr::filter(vpcstat, nbin >= min_bin_count),
    strat_var = !!strat_var_str,
    shown = show_vpc,
    vpc_theme = vpctheme,
    loq = loq
  ))

  ##Overlay Observations if Requested
  if(isTRUE(show_vpc$obs_dv)){
    plot <- plot+
      ggplot2::geom_point(ggplot2::aes(y = DV, x = TIME),
                          data = obs, inherit.aes = FALSE,
                          shape = vpctheme$obs_shape,
                          alpha = vpctheme$obs_alpha,
                          size = vpctheme$obs_size,
                          color = vpctheme$obs_color)
  }


  ##Add subtitle with replicates
  if(isTRUE(show_rep)){
    plot <- plot+
      ggplot2::labs(caption = paste0("Replicates = ", max(sim[[irep_name_str]])))
  }


  ##Apply x-axis breaks, theme panel elements, and default labels
  plot <- plot +
    ggplot2::scale_x_continuous(breaks = xbreaks) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                   linewidth = 0.5,
                                                   color = "black"),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank()) +
    ggplot2::labs(y = "Concentration", x = "Time")


  return(plot)
}





#' VPC theme with pmxhelpr default aesthetics
#'
#' @param update list containing the plot elements to be updated.
#'    Run `plot_vpc_theme()` with no arguments to view defaults.
#' @return a `list`with vpc theme specifiers
#' @export plot_vpc_theme
#'
#' @examples
#' plot_vpc_theme()

plot_vpc_theme <- function(update = NULL) list_update(update, .vpc_defaults)





#' Default VPC show layer settings
#'
#' @param update list containing the plot elements to be updated.
#'    Run `plot_vpc_shown()` with no arguments to view defaults.
#' @return a `list`with vpc shown specifiers
#' @export plot_vpc_shown
#'
#' @examples
#' plot_vpc_shown()

plot_vpc_shown <- function(update = NULL) list_update(update, .vpc_shown_defaults)




