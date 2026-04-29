#' Plot a dependent variable versus time
#'
#' @param data Input dataset.
#' @param dv_var Column containing the dependent variable. Accepts bare names or strings. Default is `DV`.
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
#' @param log_y Logical indicator for log10 transformation of the y-axis. Also controls whether
#'    the caption reports arithmetic or geometric mean when `show_caption = TRUE`.
#' @param show_caption Logical indicating if a caption should be show describing the data plotted
#' @param time_var Column containing the actual time variable.
#'    Accepts bare names or strings. Default is `TIME`.
#' @param ntime_var Column containing the nominal time variable.
#'    Accepts bare names or strings. Default is `NTIME`.
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
                        time_var = TIME,
                        ntime_var = NTIME,
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
                        log_y = FALSE,
                        show_caption = TRUE,
                        theme = NULL){

  dv_var_str    <- resolve_var(rlang::enquo(dv_var))
  time_var_str  <- resolve_var(rlang::enquo(time_var))
  ntime_var_str <- resolve_var(rlang::enquo(ntime_var))
  grp_var_str   <- resolve_var(rlang::enquo(grp_var))
  dose_var_str  <- resolve_var(rlang::enquo(dose_var))
  col_var_str   <- resolve_var(rlang::enquo(col_var), nullable = TRUE)

  prep <- df_prep_dvtime(
    data, time_var_str, ntime_var_str,
    dv_var_str = dv_var_str,
    loq = loq, loq_method = loq_method,
    dose_var_str = if (dosenorm) dose_var_str,
    col_var_str = col_var_str,
    grp_dv = grp_dv, grp_var_str = grp_var_str,
    dosenorm = dosenorm,
    cfb = cfb, cfb_base = cfb_base
  )
  data <- prep$data
  lloq <- prep$lloq

  env <- prep_plot_env(data, cent, log_y, obs_dv, grp_dv, theme, plot_dvtime_theme)
  caption   <- env$caption
  plottheme <- env$plottheme
  width     <- env$width

  #Remove EVID!=0
  data <- dplyr::filter(data, EVID==0)

###Plot

  #Initialize Plot
  plot <- init_plot(data, "TIME", "DV", col_var_str)

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
  if(isTRUE(log_y)) plot <- plot + ggplot2::scale_y_log10(guide = "axis_logticks")

  #Caption
  if(isTRUE(show_caption)) plot <- plot + ggplot2::labs(caption = caption)

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



