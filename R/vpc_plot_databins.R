
#' plot_vpc_obs_actual
#'
#' @param sim simulation output data.frame (currently requires variable TIME))
#' @param pcvpc logical for prediction correction (default = FALSE)
#' @param strat_vars character vector (max length = 2) of stratifying variables
#' @param irep_name name of the iteration variable (default = SIM)
#' @param min_bin_count minimum number of quantifiable observations per bin for plotting.
#' Drops small bins from summary statistic calculation but retains in observed data points.
#' @param ... other arguments passed to vpc::vpc
#'
#' @return ggplot2 object
#' @export
#'
#' @examples #need examples
plot_vpc_obs_actual <- function(sim, pcvpc = FALSE, strat_vars=NULL,
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
