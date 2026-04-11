#' Plot a visual predictive check (VPC) with exact time bins
#'
#' @description  `plot_vpc_exactbins()` is a wrapper function for [vpc::vpc()]
#' that returns a `ggplot2` object.
#'
#' @param sim Input dataset. Must contain the following variables: `"ID"`, `"TIME"`
#' @param pcvpc logical for prediction correction. Default is `FALSE`.
#' @param loq Numeric value of the lower limit of quantification (LLOQ) for the assay. Passed to `lloq` argument
#'    of [vpc::vpc()]. Specifying this argument implies that `OBSDV` is missing in `sim` where < LLOQ.
#' @param strat_var Stratification variable passed to `stratify` argument
#'    of [vpc::vpc()]. Accepts bare names or strings. Currently, only a single stratifying variable is supported.
#' @param min_bin_count Minimum number of quantifiable observations in exact bin for inclusion
#'    in binned plot layers. This argument drops small bins from summary statistic calculation
#'    but retains these observations in the observed data points.
#' @param show_rep Display number of replicates as a plot caption. Default is `TRUE`.
#' @param shown Named list of logicals specifying which layers to include on the plot. Passed to `show` argument of [vpc::vpc()].
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
#' @param theme Named list of aesthetic parameters for the plot. Passed to `vpc_theme` argument of [vpc::vpc()].
#'    Defaults for `pmxhelpr`dev can be obtained by running `plot_vpc_theme` with no arguments.
#'    Defaults for `vpc` can be obtained by running [vpc::new_vpc_theme()] with no arguments.
#'
#' @inheritParams df_pcdv
#' @param ... Other arguments passed to [vpc::vpc()].
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
                               pcvpc = FALSE,
                               time_vars = c(TIME = "TIME",
                                             NTIME = "NTIME"),
                               output_vars = c(PRED = "PRED",
                                               IPRED = "IPRED",
                                               SIMDV = "SIMDV",
                                               OBSDV = "OBSDV"),
                               loq = NULL,
                               strat_var = NULL,
                               irep_name = SIM,
                               min_bin_count=1,
                               show_rep = TRUE,
                               lower_bound = 0,
                               shown = NULL,
                               theme = NULL,
                               timeu = "hours",
                               n_breaks = 8,
                               ...)
{

  strat_var_str  <- capture_col(rlang::enquo(strat_var))
  irep_name_str  <- rlang::as_name(rlang::ensym(irep_name))

  #Update Lists
  time_vars <- init_time_vars(time_vars)

  output_vars <- list_update(output_vars, c(PRED = "PRED",
                                            IPRED = "IPRED",
                                            SIMDV = "SIMDV",
                                            OBSDV = "OBSDV"))

  #Checks
  check_df(sim)
  check_varsindf(sim, time_vars[["TIME"]])
  check_varsindf(sim, time_vars[["NTIME"]])
  if(pcvpc == TRUE) {check_varsindf(sim, output_vars[["PRED"]])}
  check_varsindf(sim, output_vars[["SIMDV"]])
  check_varsindf(sim, output_vars[["OBSDV"]])
  check_varsindf(sim, "MDV")
  check_varsindf(sim, strat_var_str)
  check_varsindf(sim, irep_name_str)
  if(!is.null(strat_var_str)) {check_factor(sim, strat_var_str)}
  if(!is.null(loq)) {check_numeric_strict(loq)}

  ##Set vpc aesthetics shown ensuring that observed points are not plotted by vpc::vpc()
  show_vpc <- list_update(shown,
                       list(obs_dv = TRUE, obs_ci = TRUE,
                            pi = FALSE, pi_as_area = FALSE, pi_ci = TRUE,
                            obs_median = TRUE, sim_median =FALSE, sim_median_ci = TRUE))
  shown_obs <- show_vpc
  show_vpc$obs_dv <- FALSE

  #aesthetics for legend based on settings in vpc::new_vpc_theme
  vpctheme <- list_update(theme, plot_vpc_theme())

  #Handle Output and Time Variables
  sim <- rename_time_vars(sim, time_vars, output_vars)

  ##Ensure stratification variable is a not an ordered factor (will cause vpc::add_stratification() to fail)
  if(!is.null(strat_var_str)){
    sim <- sim |>
      dplyr::mutate(!!strat_var_str := factor(.data[[strat_var_str]], ordered = FALSE))
  }

  #Set MDV to zero for appropriate censoring if not pred-corrected
  if(pcvpc==FALSE & !is.null(loq)){
    sim$MDV <- 0
  }

  ##Observed Data
  obs <- sim |>
    dplyr::filter(.data[[irep_name_str]] == 1 & MDV == 0) |>
    df_pcdv(strat_vars = strat_var_str, dvpred_vars = c(DV = "OBSDV", PRED = "PRED"),
            lower_bound = lower_bound) |>
    dplyr::rename(OBSDV = DV, PCOBSDV = PCDV)

  ##Determine number of observations in each bin
  bin_count <- df_nobsbin(obs, bin_var = "NTIME", strat_vars = strat_var_str)

  ##Identify observed summary data
  obs_sum <- dplyr::left_join(obs, bin_count) |>
    dplyr::filter(n_obs >= min_bin_count) |>
    dplyr::filter(MDV == 0)

  ##Identify simulated summary data
  sim_sum <- dplyr::left_join(sim, bin_count) |>
    dplyr::filter(n_obs >= min_bin_count)|>
    dplyr::filter(MDV == 0)

  ##Define Bins
  bins <- c(unique(obs_sum$NTIME), Inf) #Add Infinity to ensure last time point is in a unique bin

  #Determine breaks
  xbreaks <- breaks_time(x = unique(obs$NTIME), unit = timeu, n = n_breaks)

  ##VPC Function
  plot <- vpc::vpc(
    sim = as.data.frame(sim_sum),
    obs = as.data.frame(obs_sum),
    stratify = strat_var_str,
    obs_cols = list("dv" = "OBSDV", "idv" = "NTIME", "pred" = "PRED"),
    sim_cols = list("dv" = "SIMDV", "idv" = "NTIME", "pred" = "PRED"),
    bins = bins,
    pred_corr = pcvpc,
    pred_corr_lower_bnd = lower_bound,
    lloq = loq,
    show = show_vpc,
    vpc_theme = vpctheme,
    ...)

  #If to capture vpcdb = TRUE passed to vpc::vpc
  if(!ggplot2::is_ggplot(plot)) {
    return(plot) #if not ggplot object, must be a list containing vpc information with vpcdb=TRUE
  } else {
    ##Overlay Observations if Requested
    if(shown_obs$obs_dv == TRUE & pcvpc == FALSE){
      plot <- plot+
        ggplot2::geom_point(ggplot2::aes(y = OBSDV, x = TIME), data = obs, inherit.aes = FALSE,
                            shape = vpctheme$obs_shape,
                            alpha = vpctheme$obs_alpha,
                            size = vpctheme$obs_size,
                            color = vpctheme$obs_color)
    }

    if(shown_obs$obs_dv == TRUE & pcvpc == TRUE) {
      plot <- plot+
        ggplot2::geom_point(ggplot2::aes(y = PCOBSDV, x = TIME), data = obs, inherit.aes = FALSE,
                            shape = vpctheme$obs_shape,
                            alpha = vpctheme$obs_alpha,
                            size = vpctheme$obs_size,
                            color = vpctheme$obs_color)
    }

    ##Add Subtitle with Replicates
    if(show_rep == TRUE){
      plot <- plot+
        ggplot2::labs(caption = paste0("Replicates = ", max(sim[[irep_name_str]])))
    }

    #Add x-axis breaks and theme
    plot <- plot +
      ggplot2::scale_x_continuous(breaks = xbreaks) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                     linewidth = 0.5,
                                                     color = "black"),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.grid.major.x = ggplot2::element_blank())


    return(plot)
  }
}





#' Customized VPC theme with pmxhelpr default aesthetics
#'
#' @param update list containing the plot elements to be updated.
#'    Run `plot_vpc_theme()` with no arguments to view defaults.
#' @return a `list`with vpc theme specifiers
#' @export plot_vpc_theme
#'
#' @examples
#' plot_vpc_theme()
#' new_theme <- plot_vpc_theme(update = vpc::new_vpc_theme()) #restores vpc package defaults


plot_vpc_theme <- function(update = NULL){
  defaults_list <- list(
    obs_color = "#0000FF",

    obs_median_color = "#FF0000",
    obs_ci_color = "#0000FF",

    sim_median_fill = "#FF0000",
    sim_pi_fill = "#0000FF"
  )

  default_theme <- list_update(defaults_list, vpc::new_vpc_theme())
  list_update(update, default_theme)
}
