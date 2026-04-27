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
#'data <- df_addn(dplyr::mutate(data_sad_pk, Dose = DOSE), grp_var = Dose, sep = "mg")
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

  prep <- prep_dvtime_data(
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

  #Reference Lines: Y=0 (cfb = TRUE) or Y=LLOQ (loq_method = 1,2)
  if(cfb == TRUE) plot <- plot + ggplot2::geom_hline(yintercept = as.numeric(cfb_base),
                                                     linewidth = plottheme$ref$linewidth,
                                                     linetype = plottheme$ref$linetype,
                                                     alpha = plottheme$ref$alpha)

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






