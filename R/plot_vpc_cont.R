#' Plot a visual predictive check (VPC) for continuous data with exact time bins
#'
#' @description  `plot_vpc_cont()` generates a VPC plot using exact time bins
#' and returns a `ggplot2` object.
#'
#' @param sim Input dataset. Must contain the following variables: `"ID"`, `"TIME"`
#' @param strat_var Stratification variable.
#'    Accepts bare names or strings. Currently, only a single stratifying variable is supported.
#' @param pcvpc logical for prediction correction. Default is `FALSE`.
#' @param loq Numeric value of the lower limit of quantification (LLOQ) for the assay.
#'    If `NULL` (default) and an `LLOQ` column is present in `sim`, the unique LLOQ value
#'    is inherited from that column automatically.
#'    Ignored when `pcvpc = TRUE` (LLOQ is not meaningful on the prediction-corrected scale).
#'    Specifying this argument implies that the observed variable `OBSDV` is missing (`NA`)
#'    and `MDV = 1` where < `loq` in `sim`.
#'    For standard VPCs (`pcvpc = FALSE`), all `MDV` values are set to 0 so that
#'    all observations (including BLQ) are included in summary statistics.
#'    Dashed horizontal line plotted at `loq` by default (controlled via `vpc_theme`).
#' @param min_bin_count Minimum number of quantifiable observations in exact bin for inclusion
#'    in binned plot layers. This argument drops small bins from summary statistic calculation
#'    but retains these observations in the observed data points.
#' @param show_rep Display number of replicates as a plot caption. Default is `TRUE`.
#'
#' @param shown Layer visibility settings created by [plot_vpc_shown()].
#'    Defaults can be viewed by running `plot_vpc_shown()` with no arguments.
#'
#' @param vpc_theme Named list of aesthetic parameters for the plot.
#'    Defaults for `pmxhelpr` can be obtained by running `plot_vpc_theme` with no arguments.
#'
#' @param pi Numeric vector of length 2 specifying prediction interval quantiles. Default is `c(0.05, 0.95)`.
#' @param ci Numeric vector of length 2 specifying confidence interval quantiles. Default is `c(0.05, 0.95)`.
#' @param vpcstats Logical. If `TRUE`, return a list of computed VPC statistics instead of a plot. Default is `FALSE`.
#'
#' @param time_var Column containing the actual time variable in `sim`.
#'    Accepts bare names or strings. Default is `TIME`.
#' @param ntime_var Column containing the nominal time variable in `sim`.
#'    Accepts bare names or strings. Default is `NTIME`.
#' @param pred_var Column containing population predictions in `sim`.
#'    Accepts bare names or strings. Default is `PRED`.
#' @param ipred_var Column containing individual predictions in `sim`.
#'    Accepts bare names or strings. Default is `IPRED`.
#' @param sim_dv_var Column containing simulated DV in `sim`.
#'    Accepts bare names or strings. Default is `SIMDV`.
#' @param obs_dv_var Column containing observed DV in `sim`.
#'    Accepts bare names or strings. Default is `OBSDV`.
#' @param irep_name Name of replicate variable in `sim`. Accepts bare names or strings. Default is `SIM`.
#' @inheritParams plot_dvtime
#' @inheritParams var_pc
#'
#' @return A `ggplot2` object (default), or a `data.frame` of VPC summary
#'    statistics from [df_vpcstats()] when `vpcstats = TRUE`.
#' @export plot_vpc_cont
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
#' vpc_plot <- plot_vpc_cont(
#' sim = simout,
#' pcvpc = TRUE,
#' pi = c(0.05, 0.95),
#' ci = c(0.05, 0.95))

plot_vpc_cont <- function(sim,
                               time_var = TIME,
                               ntime_var = NTIME,
                               pred_var = PRED,
                               ipred_var = IPRED,
                               sim_dv_var = SIMDV,
                               obs_dv_var = OBSDV,
                               strat_var = NULL,
                               pcvpc = FALSE,
                               loq = NULL,
                               irep_name = SIM,
                               min_bin_count=1,
                               show_rep = TRUE,
                               lower_bound = 0,
                               shown = NULL,
                               vpc_theme = NULL,
                               pi = c(0.05, 0.95),
                               ci = c(0.05, 0.95),
                               vpcstats = FALSE)
{

  time_var_str    <- resolve_var(rlang::enquo(time_var))
  ntime_var_str   <- resolve_var(rlang::enquo(ntime_var))
  pred_var_str    <- resolve_var(rlang::enquo(pred_var))
  ipred_var_str   <- resolve_var(rlang::enquo(ipred_var))
  sim_dv_var_str  <- resolve_var(rlang::enquo(sim_dv_var))
  obs_dv_var_str  <- resolve_var(rlang::enquo(obs_dv_var))
  strat_var_str   <- resolve_var(rlang::enquo(strat_var), nullable = TRUE)
  irep_name_str   <- resolve_var(rlang::enquo(irep_name))

  #Inherit LLOQ from sim column if not explicitly provided
  if (is.null(loq) && "LLOQ" %in% colnames(sim)) {
    lloq_vals <- unique(sim$LLOQ[sim[[irep_name_str]] == 1 & !is.na(sim$LLOQ)])
    if (length(lloq_vals) == 1L) loq <- lloq_vals
  }

  #Block loq handling when prediction-corrected — LLOQ is on the original scale
  if (isTRUE(pcvpc)) loq <- NULL

  #Preprocess: validate, rename, prediction-correct or BLQ-handle
  sim <- df_vpcpreprocess(sim, time_var_str, ntime_var_str,
                          pred_var_str, ipred_var_str,
                          sim_dv_var_str, obs_dv_var_str,
                          strat_var_str, pcvpc, lower_bound, loq)

  #Post-preprocess checks
  check_varsindf(sim, irep_name_str, "sim", "irep_name")
  if(!is.null(loq)) {check_numeric_strict(loq, "loq")}

  #Isolate observed data for plot overlay
  obs <- sim |>
    dplyr::filter(.data[[irep_name_str]] == 1 & MDV == 0)

  ##Compute VPC Statistics
  bin_var <- "BIN_MID"
  vpcstat <- df_vpcstats(sim, pi, ci, bin_var, strat_var_str, irep_name_str, loq)

  ##Return database if requested
  if(isTRUE(vpcstats)) {
    return(vpcstat)
  }

  ##Set vpc aesthetics and theme
  show_vpc <- merge_element(shown, plot_vpc_shown())
  vpctheme <- merge_theme(vpc_theme, plot_vpc_theme())

  ##Build VPC Plot
  plot <- vpc_build_plot(
    vpcstats = dplyr::filter(vpcstat, nbin >= min_bin_count),
    bin_var = bin_var,
    strat_var_str = strat_var_str,
    shown = show_vpc,
    vpc_theme = vpctheme,
    loq = loq
  )

  ##Overlay Observations if Requested
  if(isTRUE(show_vpc$obs_dv)){
    plot <- plot+
      ggplot2::geom_point(ggplot2::aes(y = OBSDV, x = TIME),
                          data = obs, inherit.aes = FALSE,
                          shape = vpctheme$obs$shape,
                          alpha = vpctheme$obs$alpha,
                          size = vpctheme$obs$size,
                          color = vpctheme$obs$color)
  }


  ##Add subtitle with replicates
  if(isTRUE(show_rep)){
    plot <- plot+
      ggplot2::labs(caption = paste0("Replicates = ", max(sim[[irep_name_str]])))
  }


  ##Apply theme panel elements
  plot <- plot +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                   linewidth = 0.5,
                                                   color = "black"),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank())


  return(plot)
}












