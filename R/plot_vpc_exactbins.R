#' Plot a visual predictive check (VPC) with exact time bins
#'
#' @description  `plot_vpc_exactbins()` is a wrapper function for [vpc::vpc()]
#' that returns a `ggplot2` object.
#'
#' @param sim Input dataset. Must contain the following variables: `"ID"`, `TIME`
#' @param pcvpc logical for prediction correction. Default is `FALSE`.
#' @param strat_var Character string of stratification variable passed to `stratify` argument
#'    of [vpc::vpc()]. Currently, only a single stratifying variable is supported.
#' @param min_bin_count Minimum number of quantifiable observations in exact bin for inclusion
#'    in binned plot layers. This argument drops small bins from summary statistic calculation
#'    but retains these observations in the observed data points.
#' @param show_rep Display number of replicates as a plot caption. Default is `TRUE`.
#' @inheritParams df_pcdv
#' @param ... Other arguments passed to [vpc::vpc()].
#'
#' @inheritParams df_mrgsim_replicate
#'
#' @return A list containing calculated VPC information (when `vpcdb=TRUE`), or a ggplot2 object (default)
#' @export plot_vpc_exactbins
#'
#' @examples
#' model <- model_mread_load(model = "model")
#' simout <- df_mrgsim_replicate(data = data_sad, model = model, replicates = 100,
#' output_vars = c(DV = "ODV"),
#' num_vars = c("CMT", "EVID", "MDV", "NTIME", "LLOQ", "WTBL", "FOOD"),
#' char_vars = c("USUBJID", "PART"),
#' irep_name = "SIM")
#'
#' vpc_plot <- plot_vpc_exactbins(
#' sim = simout,
#' pcvpc = TRUE,
#' pi = c(0.05, 0.95),
#' ci = c(0.05, 0.95),
#' log_y = TRUE)

plot_vpc_exactbins <- function(sim,
                              pcvpc = FALSE,
                              time_vars = c(TIME = "TIME",
                                            NTIME = "NTIME"),
                              output_vars = c(PRED = "PRED",
                                              IPRED = "IPRED",
                                              SIMDV = "SIMDV",
                                              OBSDV = "OBSDV"),
                              strat_var=NULL,
                              irep_name = "SIM",
                              min_bin_count=1,
                              lower_bound = 0,
                              show_rep = TRUE,
                              ...)
  {

  #Checks
  check_df(sim)
  check_varsindf(sim, time_vars[["TIME"]])
  check_varsindf(sim, time_vars[["NTIME"]])
  check_varsindf(sim, output_vars[["SIMDV"]])
  check_varsindf(sim, output_vars[["OBSDV"]])
  check_varsindf(sim, strat_var)
  check_varsindf(sim, irep_name)
  if(!is.null(strat_var)) {check_factor(sim, strat_var)}

  ##Data Rename
  sim <- sim |>
    dplyr::rename((dplyr::any_of(c(output_vars, time_vars))))

  ##Observed Data
  obs <- sim |>
    dplyr::filter(!!dplyr::sym(irep_name) == 1) |>
    df_pcdv(strat_vars = strat_var, output_vars = c(DV = "OBSDV", PRED = "PRED"),
            lower_bound = lower_bound) |>
    dplyr::rename(OBSDV = DV, PCOBSDV = PCDV)

  ##Determine number of observations in each bin
  bin_count <- df_nobsbin(obs, bin_var = "NTIME", strat_vars = strat_var)

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
    sim = as.data.frame(sim_sum),
    obs = as.data.frame(obs_sum),
    stratify = strat_var,
    obs_cols = list("dv" = "OBSDV", "idv" = "NTIME", "pred" = "PRED"),
    sim_cols = list("dv" = "SIMDV", "idv" = "NTIME", "pred" = "PRED"),
    bins = bins,
    pred_corr = pcvpc,
    pred_corr_lower_bnd = lower_bound,
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

  ##Add Subtitle with Replicates
  if(show_rep == TRUE){
    plot <- plot+
      ggplot2::labs(caption = paste0("Replicates = ", max(sim[[irep_name]])))
  }

  return(plot)
}





#' Count the non-missing observations in each exact bin
#'
#' @description
#' `df_nobsbin()` is a helper function to count the number of missing
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
#' @export df_nobsbin
#'
#' @examples
#' df_nobsbin(data_sad)
#'
df_nobsbin <- function(data,
                     bin_var = "NTIME",
                     strat_vars = "CMT"){
  check_df(data)
  check_varsindf(data, bin_var)
  check_varsindf(data, strat_vars)

  bin_count <- data |>
    dplyr::filter(EVID == 0) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(bin_var, strat_vars)))) |>
    dplyr::summarize(n_obs = sum(MDV==0),
                     n_miss = sum(MDV==1)) |>
    dplyr::ungroup()

  return(bin_count)
}



#' Perform prediction-correction of the dependent variable
#'
#' @description
#' `df_pcdv` is a helper function to perform prediction-correction
#'    of observed or simulated depedent variables.
#'
#' @param data Input dataset
#' @param bin_var Exact binning variable. Default is `"NTIME"`.
#' @param strat_vars Stratifying variables. Default is `"CMT"`.
#' @param lower_bound Lower bound of the dependent variable for prediction correction. Default is `0`.
#' @inheritParams df_mrgsim_replicate
#'
#' @return A data.frame containing one row per unique combination of
#'    `bin_var` and `strat_vars` and new variable `PCDV` containing
#'    prediction-corrected observations.
#' @export df_pcdv
#'
#' @examples
#' model <- model_mread_load(model = "model")
#' data <- df_addpred(data_sad, model)
#' simout <- df_pcdv(data, output_vars = c(DV = "ODV", PRED = "PRED"))
#'
df_pcdv <- function(data,
                    bin_var = "NTIME",
                    strat_vars = "CMT",
                    output_vars = c(PRED = "PRED",
                                    IPRED = "IPRED",
                                    DV = "DV"),
                    lower_bound = 0) {
  check_df(data)
  check_varsindf(data, bin_var)
  check_varsindf(data, strat_vars)
  check_varsindf(data, output_vars[["PRED"]])
  check_varsindf(data, output_vars[["DV"]])

  data <- data |>
    dplyr::rename(dplyr::all_of(output_vars)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(bin_var, strat_vars)))) |>
    dplyr::mutate(PREDBIN = stats::median(PRED),
                  PCDV = lower_bound + (DV-lower_bound)*((PREDBIN-lower_bound)/(PRED-lower_bound))) |>
    dplyr::ungroup()

  return(data)
}
