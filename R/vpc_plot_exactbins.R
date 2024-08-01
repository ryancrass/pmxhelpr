#' Plot a visual predictive check (VPC) with exact time bins
#'
#' @description  `vpc_plot_exactbins()` is a wrapper function for [vpc::vpc()]
#' that returns a `ggplot2` object.
#'
#' @param sim Input dataset. Must contain the following variables: `"ID"`, `TIME`
#' @param pcvpc logical for prediction correction. Default is `FALSE`.
#' @param strat_vars Character vector of stratification variables passed to `stratify` argument
#'    of [vpc::vpc()]. Currently, maximum length = 2.
#' @param min_bin_count Minimum number of quantifiable observations in exact bin for inclusion
#'    in binned plot layers. This argument drops small bins from summary statistic calculation
#'    but retains these observations in the observed data points.
#' @param conc_label Label for the concentration axis.
#' @param time_label Label for the time axis.
#' @param ... Other arguments passed to [vpc::vpc()].
#'
#' @inheritParams mrgsim_vpc
#'
#' @return A list containing calculated VPC information (when `vpcdb=TRUE`), or a ggplot2 object (default)
#' @export vpc_plot_exactbins
#'
#' @examples
#' model <- model_load(model = "model")
#' simout <- mrgsim_vpc(data = data_sad, model = model, replicates = 100,
#' output_vars = c(DV = "ODV"),
#' num_vars = c("CMT", "EVID", "MDV", "NTIME", "LLOQ", "WTBL", "FOOD"),
#' char_vars = c("USUBJID", "PART"),
#' irep_name = "SIM")
#'
#' vpc_plot <- vpc_plot_exactbins(
#' sim = simout,
#' pcvpc = TRUE,
#' pi = c(0.05, 0.95),
#' ci = c(0.05, 0.95),
#' log_y = TRUE)

vpc_plot_exactbins <- function(sim,
                              pcvpc = FALSE,
                              time_vars = c(TIME = "TIME",
                                            NTIME = "NTIME"),
                              output_vars = c(PRED = "PRED",
                                              IPRED = "IPRED",
                                              SIMDV = "SIMDV",
                                              OBSDV = "OBSDV"),
                              strat_vars=NULL,
                              irep_name = "SIM",
                              min_bin_count=1,
                              conc_label = "Concentration",
                              time_label = "Time",
                              ...)
  {

  ##Data Rename
  sim <- sim |>
    dplyr::rename((dplyr::any_of(c(output_vars, time_vars))))

  ##Observed Data
  obs <- sim |>
    dplyr::filter(!!dplyr::sym(irep_name) == 1) |>
    df_calc_predcorr(strat_vars = strat_vars, output_vars = c(DV = "OBSDV")) |>
    dplyr::rename(OBSDV = DV, PCOBSDV = PCDV)

  ##Determine number of observations in each bin
  bin_count <- nobs_bin(obs, bin_var = "NTIME", strat_vars = strat_vars)

  ##Identify observed summary data
  obs_sum <- dplyr::left_join(obs, bin_count) |>
    dplyr::filter(n_obs >= min_bin_count)

  ##Identify simulated summary data
  sim_sum <- dplyr::left_join(sim, bin_count) |>
    dplyr::filter(n_obs >= min_bin_count)

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

  ##Update Plot Aesthetics
  conc_label <- ifelse(pcvpc == TRUE, paste0("PRED-corrected ", conc_label), conc_label)
  plot <- plot +
    ggplot2::labs(y = conc_label, x = time_label)

  return(plot)
}





#' Count the non-missing observations in each exact bin
#'
#' @description
#' `nobs_bin()` is a helper function to count the number of missing
#'    and non-missing observations in exact bins.
#'
#' @param data Input dataset.
#' @param bin_var Binning variable. Default is `"NTIME"`.
#' @param strat_vars Stratifying variables. Must be a character vector.
#'    Default is `"CMT"`.
#'
#' @return A data.frame containing one row per unique combination of
#'    `bin_var` and `strat_vars` and new variables `n_obs`, a count
#'    of non-missing observations, and `n_miss`, a count of missing observations.
#' @export nobs_bin
#'
#' @examples
#' nobs_bin(data_sad)
#'
nobs_bin <- function(data,
                     bin_var = "NTIME",
                     strat_vars = "CMT"){
  bin_count <- data |>
    dplyr::filter(EVID == 0) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(bin_var, strat_vars)))) |>
    dplyr::summarize(n_obs = sum(MDV==0),
                     n_miss = sum(MDV==1)) |>
    dplyr::ungroup()

  return(bin_count)
}



#' Perform prediction-correction
#'
#' @description
#' `df_calc_predcorr` is a helper function to perform prediction-correction
#'    of observed or simulated depedent variables.
#'
#' @param data Input dataset
#' @param bin_var Exact binning variable. Default is `"NTIME"`.
#' @param strat_vars Stratifying variables. Default is `"CMT"`.
#' @inheritParams mrgsim_vpc
#'
#' @return A data.frame containing one row per unique combination of
#'    `bin_var` and `strat_vars` and new variable `PCDV` containing
#'    prediction-corrected observations.
#' @export df_calc_predcorr
#'
#' @examples
#' model <- model_load(model = "model")
#' data <- df_add_pred(data_sad, model)
#' simout <- df_calc_predcorr(data, output_vars = c(DV = "ODV"))
#'
df_calc_predcorr <- function(data,
                             bin_var = "NTIME",
                             strat_vars = "CMT",
                             output_vars = c(PRED = "PRED",
                                             IPRED = "IPRED",
                                             DV = "DV")) {
  data <- data |>
    dplyr::rename(dplyr::all_of(output_vars)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(bin_var, strat_vars)))) |>
    dplyr::mutate(PREDBIN = stats::median(PRED),
                  PCDV = DV*(PREDBIN/PRED))
  return(data)
}

