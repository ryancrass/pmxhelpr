#' Plot a dependent variable versus time
#'
#' @param data Input dataset.
#' @param timeu Character string specifying units for the time variable.
#'    Passed to `var_timebreaks` and assigned to default x-axis label.
#'    Options include:
#'    + "hours" (default)
#'    + "days"
#'    + "weeks"
#'    + "months"
#' @param n_breaks Number of breaks requested for x-axis. Default is 8.
#' @param col_var Column to map to the color aesthetic. Accepts bare names or strings. Default is `NULL`.
#' @param grp_var Column to map to the group aesthetic. Accepts bare names or strings. Default is `ID`.
#' @param dose_var Column to use in dosenormalization when `dosenorm` = TRUE.
#'   Accepts bare names or strings. Default is `DOSE`.
#' @param loq Numeric value of the lower limit of quantification (LLOQ) for the assay.
#'  Must be coercible to a numeric if specified. Can be `NULL` if variable `LLOQ` is present in `data`
#'  Specifying this argument implies that `DV` is missing in `data` where < LLOQ.
#' @param loq_method Method for handling data below the lower limit of quantification (BLQ) in the plot.
#'
#'   Options are:
#'
#'     + `0` : No handling. Plot input dataset `DV` vs `TIME` as is. (default)
#'     + `1` : Impute all BLQ data at `TIME` <= 0 to 0 and all BLQ data at `TIME` > 0 to 1/2 x `loq`.
#'        Useful for plotting concentration-time data with some data BLQ on the linear scale
#'     + `2` : Impute all BLQ data at `TIME` <= 0 to 1/2 x `loq` and all BLQ data at `TIME` > 0 to 1/2 x `loq`.
#'        Useful for plotting concentration-time data with some data BLQ on the log scale where 0 cannot be displayed
#'
#' @param cent Character string specifying the central tendency measure to plot.
#'
#'  Options are:
#'
#'    + Mean only: `"mean"` (default)
#'    + Mean +/- Standard Deviation (upper and lower error bar): `"mean_sdl"`
#'    + Mean + Standard Deviation (upper error bar only): `"mean_sdl_upper"`
#'    + Median only: `"median"`
#'    + Median +/- Interquartile Range: `median_iqr`
#'    + None: `"none"`
#'
#' @param obs_dv Logical indicating if observed data points should be shown. Default is `TRUE`.
#' @param grp_dv Logical indicating if observed data points should be connected within a group (i.e., spaghetti plot).
#'    Default is `FALSE`.
#' @param dosenorm logical indicating if observed data points should be dose normalized. Default is `FALSE`,
#'    Requires variable specified in `dose_var` to be present in `data`
#' @param cfb Logical indicating if dependent variable is a change from baseline.
#'    Plots a reference line at y = cfb_baseline. Default is `FALSE`.
#' @param cfb_base Value for y-intercept when cfb = `TRUE`. Default is 0.
#' @param ylab Character string specifing the y-axis label: Default is `"Concentration"`.
#' @param log_y Logical indicator for log10 transformation of the y-axis.
#' @param show_caption Logical indicating if a caption should be show describing the data plotted
#' @inheritParams df_mrgsim_replicate
#' @param theme Theme object created by [plot_dvtime_theme()].
#'    Defaults can be viewed by running `plot_dvtime_theme()` with no arguments.
#'    Default error bar width is 2.5% of maximum `NTIME`.
#'
#' @return A `ggplot2` plot object
#'
#' @export plot_dvtime
#'
#' @examples
#'data_sad_pk <- dplyr::filter(data_sad, CMT %in% c(1,2))
#'data <- dplyr::mutate(data_sad_pk, Dose = var_addn(DOSE, ID, sep = "mg"))
#'plot_dvtime(data, dv_var = ODV, cent = "median", col_var = Dose)
#'

plot_dvtime <- function(data,
                        dv_var = DV,
                        time_vars = c(TIME = "TIME",
                                      NTIME = "NTIME"),
                        timeu = "hours",
                        col_var = NULL,
                        grp_var = ID,
                        dose_var = DOSE,
                        loq = NULL,
                        loq_method = 0,
                        cent = "mean",
                        obs_dv = TRUE,
                        grp_dv = FALSE,
                        dosenorm = FALSE,
                        cfb = FALSE,
                        cfb_base = 0,
                        ylab = "Concentration",
                        log_y = FALSE,
                        show_caption = TRUE,
                        n_breaks = 8,
                        theme = NULL){

  dv_var_str   <- resolve_var(rlang::enquo(dv_var))
  grp_var_str  <- resolve_var(rlang::enquo(grp_var))
  dose_var_str <- resolve_var(rlang::enquo(dose_var))
  col_var_str  <- resolve_var(rlang::enquo(col_var), nullable = TRUE)

  prep <- df_prep_dvtime(
    data, time_vars,
    output_vars = c(DV = dv_var_str),
    timeu = timeu, loq = loq, loq_method = loq_method,
    dose_var_str = if (dosenorm) dose_var_str,
    col_var_str = col_var_str,
    grp_dv = grp_dv, grp_var_str = grp_var_str,
    dosenorm = dosenorm,
    cfb = cfb, cfb_base = cfb_base
  )
  data <- prep$data
  lloq <- prep$lloq

  env <- prep_plot_env(data, cent, log_y, obs_dv, grp_dv,
                       timeu, n_breaks, theme, plot_dvtime_theme)
  caption   <- env$caption
  xbreaks   <- env$xbreaks
  plottheme <- env$plottheme
  width     <- env$width

  #Remove EVID!=0
  data <- dplyr::filter(data, EVID==0)

###Plot

  #Initialize Plot Aesthetics
  if(is.null(col_var_str)) {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = TIME, y=DV))
  } else {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = TIME, y=DV, color = .data[[col_var_str]]))
  }

  plot <- plot +
    ggplot2::labs(x=paste0("Time (", timeu, ")"), y=ylab) +
    ggplot2::scale_x_continuous(breaks = xbreaks) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank())

  #Reference Lines: Y=cfb_base (cfb = TRUE) or Y=LLOQ (loq_method = 1,2)
  plot <- add_cfb_layers(plot, cfb, cfb_base, plottheme)

  blq <- add_blq_layers(plot, caption, loq_method, loq = lloq, dosenorm, plottheme, show_legend = TRUE)
  plot <- blq$plot
  caption <- blq$caption

  #Show Observed Data Points / Connect within Group
  plot <- add_obs_layers(plot, obs_dv, grp_dv, grp_var_str, plottheme)

  #Plot Central Tendency (points, lines, error bars)
  plot <- add_cent_layers(plot, cent, "DV", plottheme, width)

  #Log Transform
  if(log_y == TRUE) plot <- plot + ggplot2::scale_y_log10(guide = "axis_logticks")

  #Caption
  if(show_caption == TRUE) plot <- plot + ggplot2::labs(caption = caption)

  return(plot)
}





#' Define a caption for `plot_dvtime`
#'
#' @inheritParams plot_dvtime
#'
#' @return a `character` string containing the plot caption
#' @export caption_dvtime
#' @keywords internal
#'
#' @examples
#' caption_dvtime(cent = "mean")
#' caption_dvtime(cent = "mean", log_y = TRUE)

caption_dvtime <- function(cent, log_y = FALSE, obs_dv = TRUE, grp_dv = FALSE){

  cent_labels <- list(
    mean           = c(linear = "mean",                    log = "geometric mean"),
    mean_sdl       = c(linear = "mean + SD error bars",    log = "geo. mean + geo. SD error bars"),
    mean_sdl_upper = c(linear = "mean + SD error bars",    log = "geo. mean + geo. SD error bars"),
    median         = c(linear = "median",                  log = "median"),
    median_iqr     = c(linear = "median + IQR error bars", log = "median + IQR error bars"),
    none           = c(linear = "",                        log = "")
  )

  obs_labels <- list(
    "TRUE.FALSE"  = "Open circles are observations",
    "FALSE.TRUE"  = "Thin lines connect observations within an individual",
    "TRUE.TRUE"   = "Thin lines connect observations (open circles) within an individual",
    "FALSE.FALSE" = ""
  )

  scale <- if(log_y) "log" else "linear"
  cap1 <- cent_labels[[cent]][[scale]]
  cap2 <- obs_labels[[paste(obs_dv, grp_dv, sep = ".")]]

  if(cent == "none") cap2
  else paste0("Solid circles and thick lines are the ", cap1, "\n", cap2)
}






#' Wrapper function for plot_dvtime to plot two dependent variables paneled together
#'
#' @param dv_var1 Column containing observations for the top panel (DV1). Accepts bare names or strings.
#'    Default is `DV`.
#' @param dv_var2 Column containing observations for the bottom panel (DV2). Accepts bare names or strings.
#'    Default is `DV`.
#' @param dvid_var Column to identify each observation type. Accepts bare names or strings.
#' @param dvid_val1 Value of variable specified in `dvid_var` to identify observations for the top panel (DV1).
#' @param dvid_val2 Value of variable specified in `dvid_var` to identify observations for the bottom panel (DV2).
#' @param ylab1 Character string specifying the y-axis label for the top panel (DV1): Default is `"Concentration"`.
#' @param ylab2 Character string specifying the y-axis label for the bottom panel (DV2): Default is `"Response"`.
#' @param log_y1 Logical indicator for log10 transformation of the y-axis for the top panel (DV1). Default is `FALSE`.
#' @param log_y2 Logical indicator for log10 transformation of the y-axis for the bottom panel (DV2). Default is `FALSE`.
#' @param show_caption1 Logical indicating if a caption should be show describing the data plotted for the top panel (DV1).
#' @param show_caption2 Logical indicating if a caption should be show describing the data plotted for the bottom panel (DV2).
#' @param onelegend Logical indicator if the plot legends should be collected into a single legend. Default is `FALSE`.
#' @inheritParams plot_dvtime
#'
#' @return A `ggplot2` plot object
#'
#' @export plot_dvtime_dual
#'
#' @examples
#'data <- dplyr::mutate(data_sad, Dose = var_addn(DOSE, ID, sep = "mg"))
#'plot_dvtime_dual(data, dv_var1 = ODV, dv_var2 = ODV, col_var = Dose)
#'

plot_dvtime_dual <- function(data,
                             dv_var1 = DV,
                             dv_var2 = DV,
                             dvid_var = CMT,
                             dvid_val1 = 2,
                             dvid_val2 = 3,
                             time_vars = c(TIME = "TIME",
                                           NTIME = "NTIME"),
                             timeu = "hours",
                             col_var = NULL,
                             grp_var = ID,
                             dose_var = DOSE,
                             loq = NULL,
                             loq_method = 0,
                             cent = "mean",
                             obs_dv = TRUE,
                             grp_dv = FALSE,
                             dosenorm = FALSE,
                             cfb = FALSE,
                             cfb_base=0,
                             ylab1 = "Concentration",
                             ylab2 = "Response",
                             log_y1 = FALSE,
                             log_y2 = FALSE,
                             show_caption1 = TRUE,
                             show_caption2 = TRUE,
                             n_breaks = 8,
                             onelegend = TRUE,
                             theme = NULL){

  dv_var1_str  <- resolve_var(rlang::enquo(dv_var1))
  dv_var2_str  <- resolve_var(rlang::enquo(dv_var2))
  dvid_var_str <- resolve_var(rlang::enquo(dvid_var))
  grp_var_str  <- resolve_var(rlang::enquo(grp_var))
  dose_var_str <- resolve_var(rlang::enquo(dose_var))
  col_var_str  <- resolve_var(rlang::enquo(col_var), nullable = TRUE)

  data_dv1 <- dplyr::filter(data, .data[[dvid_var_str]]==dvid_val1)
  data_dv2 <- dplyr::filter(data, .data[[dvid_var_str]]==dvid_val2)
  if(nrow(data_dv1) == 0) {rlang::abort(message = paste0("No rows in `data` where `", dvid_var_str, "` == ", dvid_val1))}
  if(nrow(data_dv2) == 0) {rlang::abort(message = paste0("No rows in `data` where `", dvid_var_str, "` == ", dvid_val2))}

  plot_dv1 <- pmxhelpr::plot_dvtime(
    data = data_dv1,
    dv_var = dv_var1_str,
    time_vars = time_vars,
    timeu = timeu,
    col_var = col_var_str,
    grp_var = grp_var_str,
    dose_var = dose_var_str,
    loq = loq,
    loq_method = loq_method,
    cent = cent,
    obs_dv = obs_dv,
    grp_dv = grp_dv,
    dosenorm = dosenorm,
    cfb = FALSE,
    ylab = ylab1,
    log_y = log_y1,
    show_caption = show_caption1,
    n_breaks = n_breaks,
    theme = theme
  )

  plot_dv2 <- pmxhelpr::plot_dvtime(
    data = data_dv2,
    dv_var = dv_var2_str,
    time_vars = time_vars,
    timeu = timeu,
    col_var = col_var_str,
    grp_var = grp_var_str,
    dose_var = dose_var_str,
    loq_method = 0,
    cent = cent,
    obs_dv = obs_dv,
    grp_dv = grp_dv,
    dosenorm = FALSE,
    cfb = cfb,
    cfb_base = cfb_base,
    ylab = ylab2,
    log_y = log_y2,
    show_caption = show_caption2,
    n_breaks = n_breaks,
    theme = theme
  )

  plot <- patchwork::wrap_plots(plot_dv1, plot_dv2) + patchwork::plot_layout(ncol = 1, nrow = 2)
  if(onelegend == TRUE) {plot <- plot + patchwork::plot_layout(guides = "collect")}
  return(plot)
}
