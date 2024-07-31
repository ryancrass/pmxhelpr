#' Plot a visual predictive check (VPC) with exact time bins
#'
#' @description  `vpc_plot_databins()` is a wrapper function for [vpc::vpc()]
#' that returns a `ggplot2` object.
#'
#' @param sim Input simulation dataset. Must contain the following variables: `"ID"`, `TIME`
#' @param pcvpc logical for prediction correction. Default is `FALSE`.
#' @param strat_vars Character vector of stratification variables passed to `stratify` argument
#'    of [vpc::vpc()]. Currently, maximum length = 2.
#' @param irep_name Name of replicate variable in `sim`. Must be a string. Default is `"SIM"`
#' @param min_bin_count Minimum number of quantifiable observations in exact bin for inclusion
#'    in binned plot layers. This argument drops small bins from summary statistic calculation
#'    but retains these observations in the observed data points.
#' @param ... Other arguments passed to [vpc::vpc()].
#'
#' @return A list containing calculated VPC information (when `vpcdb=TRUE`), or a ggplot2 object (default)
#' @export vpc_plot_databins
#'
#' @examples
#' model <- model_load(model = "model")
#' simout <- mrgsim_vpc(data = data_sad, model = model, replicates = 100,
#' output_vars = c(DV = "ODV"),
#' num_vars = c("CMT", "EVID", "MDV", "NTIME", "LLOQ", "WTBL", "FOOD"),
#' char_vars = c("USUBJID", "PART"),
#' irep_name = "SIM")
#'
#' vpc_plot <- vpc_plot_databins(
#' sim = simout,
#' pcvpc = TRUE,
#' pi = c(0.05, 0.95),
#' ci = c(0.05, 0.95),
#' log_y = TRUE)

vpc_plot_databins <- function(sim, pcvpc = FALSE, strat_vars=NULL,
                                irep_name = "SIM", min_bin_count=1,
                                ...)
  {

  ##Determine number of observations in each bin
  bin_count <- sim |>
    dplyr::filter(MDV == 0) |>
    dplyr::filter(!!dplyr::sym(irep_name) == 1) |>
    dplyr::group_by(NTIME, dplyr::all_of(strat_vars)) |>
    dplyr::summarize(n_in_bin = dplyr::n())

  ##Identify observed point data
  obs <- sim |>
    dplyr::filter(MDV==0) |>
    dplyr::filter(!!dplyr::sym(irep_name) == 1) |>
    dplyr::group_by(NTIME, dplyr::all_of(strat_vars)) |>
    dplyr::mutate(PREDBIN = stats::median(PRED),
                  PCOBSDV = OBSDV*(PREDBIN/PRED)) |>
    dplyr::left_join(bin_count) |>
    dplyr::ungroup()

  ##Identify observed summary data
  obs_sum <- obs |>
    dplyr::filter(n_in_bin >= min_bin_count)

  ##Identify simulated summary data
  sim_sum <- sim |>
    dplyr::left_join(bin_count) |>
    dplyr::filter(n_in_bin >= min_bin_count)

  ##Define Bins
  bins <- c(unique(obs_sum$NTIME), Inf) #Add Infinity to ensure last time point is in a unique bin

  ##VPC Function
  plot <- vpc::vpc(
    sim = sim_sum,
    obs = obs_sum,
    stratify = strat_vars,
    obs_cols = list("dv" = "OBSDV", "idv" = "NTIME", "pred" = "PRED"),
    sim_cols = list("dv" = "SIMDV", "idv" = "NTIME", "pred" = "PRED"),
    bins = bins,
    pred_corr = pcvpc,
    ...)

  ##Overlay Observations
  if(pcvpc == FALSE){
    plot <- plot+
      ggplot2::geom_point(ggplot2::aes(y = OBSDV, x = TIME), data = obs, inherit.aes = FALSE,
                 shape = 1, alpha = 0.5, size = 1)
  } else {
    plot <- plot+
      ggplot2::geom_point(ggplot2::aes(y = PCOBSDV, x = TIME), data = obs, inherit.aes = FALSE,
                 shape = 1, alpha = 0.5, size = 1)
  }

  return(plot)
}
