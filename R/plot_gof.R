#' Plot population overlay goodness-of-fit (GOF) plots
#'
#' Creates a population overlay plot showing central tendency lines for
#' observed (DV), population predicted (PRED), and individual predicted (IPRED)
#' values. Colors and aesthetics for each variable are controlled through the
#' `theme` argument via [plot_gof_theme()]. Use the `shown` argument to
#' selectively hide variables.
#'
#' @param dv_var Column containing the dependent variable (DV).
#'    Accepts bare names or strings. Default is `DV`.
#' @param pred_var Column containing population predictions (PRED).
#'    Accepts bare names or strings. Default is `PRED`.
#' @param ipred_var Column containing individual predictions (IPRED).
#'    Accepts bare names or strings. Default is `IPRED`.
#' @param shown Layer visibility settings created by [plot_gof_shown()].
#'    Defaults can be viewed by running `plot_gof_shown()` with no arguments.
#' @param loq Numeric value of the lower limit of quantification (LLOQ).
#'    BLQ imputation behavior on the DV / prediction layers is controlled by
#'    `blq_mode` and `loq_method`. Default is `NULL`.
#' @param loq_method Method for handling BLQ data. See [plot_dvtime()] for
#'    option details. Default is `0` (no imputation).
#' @param blq_mode One of `"obs"` (default) or `"all"`. Controls which layers
#'    receive BLQ imputation: `"obs"` imputes the observed `DV` layer only,
#'    leaving `PRED` / `IPRED` untouched (mirrors [plot_dvtime()]); `"all"`
#'    additionally imputes the prediction layers, useful when the GOF visual
#'    should mirror an estimation engine that censored predictions to LLOQ.
#'    Has no effect when `loq_method = 0`.
#' @inheritParams plot_dvtime
#' @param theme Theme object created by [plot_gof_theme()].
#'    Defaults can be viewed by running `plot_gof_theme()` with no arguments.
#'    Default error bar width is 2.5% of maximum `NTIME`.
#'
#' @family goodness-of-fit
#' @return A `ggplot2` plot object
#'
#' @export plot_gof
#'
#' @examples
#'plot_gof(data_sad_pkfit, dv_var = ODV, dosenorm = TRUE)
#'
plot_gof <- function(data,
                        dv_var = DV,
                        pred_var = PRED,
                        ipred_var = IPRED,
                        time_var = TIME,
                        ntime_var = NTIME,
                        shown = NULL,
                        id_var = NULL,
                        dose_var = DOSE,
                        loq = NULL,
                        loq_method = 0,
                        blq_mode = c("obs", "all"),
                        cent = c("mean", "mean_sdl", "mean_sdl_upper",
                                 "median", "median_iqr", "none"),
                        dosenorm = FALSE,
                        ref = NULL,
                        log_y = FALSE,
                        show_caption = TRUE,
                        theme = NULL){

  cent <- match.arg(cent)
  blq_mode <- match.arg(blq_mode)

  dv_var_str    <- resolve_var(rlang::enquo(dv_var))
  pred_var_str  <- resolve_var(rlang::enquo(pred_var))
  ipred_var_str <- resolve_var(rlang::enquo(ipred_var))
  time_var_str  <- resolve_var(rlang::enquo(time_var))
  ntime_var_str <- resolve_var(rlang::enquo(ntime_var))
  id_var_str    <- resolve_var(rlang::enquo(id_var), nullable = TRUE)
  dose_var_str  <- resolve_var(rlang::enquo(dose_var))

  if (!missing(dose_var) && !isTRUE(dosenorm)) {
    warning("`dose_var` is ignored when `dosenorm = FALSE`", call. = FALSE)
  }

  prep <- df_prep_dvtime(
    data, time_var_str, ntime_var_str,
    dv_var_str = dv_var_str,
    pred_var_str = pred_var_str,
    ipred_var_str = ipred_var_str,
    loq = loq, loq_method = loq_method,
    dose_var_str = if (dosenorm) dose_var_str,
    id_var_str = id_var_str,
    dosenorm = dosenorm,
    ref = ref,
    blq_mode = blq_mode
  )
  data <- prep$data
  lloq <- prep$lloq

  env <- prep_plot_env(data, cent, log_y, theme, plot_gof_theme)
  caption   <- env$caption
  plottheme <- env$plottheme
  width     <- env$width

  #Determine which variables to show
  shown <- merge_element(shown, plot_gof_shown())
  shown_names <- names(shown)[unlist(shown)]
  active <- toupper(shown_names)
  if (length(active) == 0L) {
    rlang::warn("`shown` has no active elements; the resulting GOF plot will have no overlay layers")
  }

  #Derive output colors from theme
  color_map <- c(OBS = plottheme$obs_point$color,
                 DV = plottheme$cent_color$dv,
                 IPRED = plottheme$cent_color$ipred,
                 PRED = plottheme$cent_color$pred)
  output_colors <- color_map[active]


###Plot

  #Initialize Plot
  plot <- init_plot(data, "TIME", "DV") +
    ggplot2::labs(color = "Legend")

  #Reference Lines
  plot <- add_ref_layers(plot, ref, plottheme$ref_line)

  blq <- add_blq_layers(plot, caption, loq_method, loq = lloq, dosenorm, plottheme$loq_line, show_legend = TRUE)
  plot <- blq$plot
  caption <- blq$caption

  #Show Observed Data Points / Connect within Group
  if ("OBS" %in% active) {
    plot <- add_obs_layers_manual(plot, id_var_str, plottheme$obs_point, plottheme$obs_line, color_aes = "OBS")
  }

  #Plot Central Tendency (points, lines, error bars)
  if ("DV" %in% active) {
    plot <- add_cent_layers_manual(plot, cent, "DV", plottheme$cent_point, plottheme$cent_line, plottheme$cent_errorbar, width, color_aes = "DV")
  }
  if ("IPRED" %in% active) {
    plot <- add_cent_layers_manual(plot, cent, "IPRED", plottheme$cent_point, plottheme$cent_line, plottheme$cent_errorbar, width, color_aes = "IPRED", show_errorbars = FALSE)
  }
  if ("PRED" %in% active) {
    plot <- add_cent_layers_manual(plot, cent, "PRED", plottheme$cent_point, plottheme$cent_line, plottheme$cent_errorbar, width, color_aes = "PRED", show_errorbars = FALSE)
  }

  #Log Transform
  if(isTRUE(log_y)) plot <- plot + ggplot2::scale_y_log10(guide = "axis_logticks")

  #Define Manual Legend
  legend_order <- c("OBS", "DV", "IPRED", "PRED")
  plot <- plot +
    ggplot2::scale_color_manual(values = output_colors,
                                breaks = legend_order[legend_order %in% active])

  #Caption
  if(isTRUE(show_caption)) plot <- plot + ggplot2::labs(caption = caption)

  return(plot)
}
