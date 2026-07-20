#' Plot a dependent variable versus concentration
#'
#' Unlike [plot_dvtime()] and [plot_gof()], this function does not filter dose
#' rows internally. Pre-filter the input to observation rows (typically by
#' `CMT` or `EVID == 0`) before calling — see the example below.
#'
#' @param data Input dataset. Must contain only observation rows (no dose
#'    records). Filter by `CMT` or `EVID == 0` before passing.
#' @param idv_var Independent variable column. Accepts bare names or strings. Default is `CONC`.
#' @param col_trend Logical indicating if the variable specified in `col_var` should be used to stratify trend lines
#' @param loess Logical indicating if a loess smoother fit should be shown. Default is `TRUE`
#' @param se_loess Logical indicating if the standard error should be shown for the loess fit. Default is `FALSE`
#' @param linear Logical indicating if a linear regression fit should be shown. Default is `FALSE`.
#' @param se_linear Logical indicating if the standard error should be shown for the linear fit. Default is `FALSE`
#' @param log_y Logical indicator for log10 transformation of the y-axis.
#' @param show_caption Logical indicating if a caption should be shown describing the data plotted
#' @param style A [ggstylekit::style_spec()] controlling plot aesthetics.
#'    Defaults to [style_dvconc()]; view the defaults by running `style_dvconc()`
#'    with no arguments. Customize by passing `style = style_dvconc(...)`, or
#'    restyle the returned plot with [ggstylekit::restyle_plot()].
#' @param ... Additional arguments passed to `geom_smooth()`
#' @inheritParams plot_dvtime
#'
#' @family exploratory analysis
#' @return A `ggplot2` plot object
#'
#' @export plot_dvconc
#'
#' @examples
#'data_sad_pd <- dplyr::filter(data_sad, CMT ==3)
#'data <- dplyr::mutate(data_sad_pd, Dose = var_addn(DOSE, ID, sep = "mg"))
#'plot_dvconc(data, dv_var = ODV, idv_var = CONC, col_var = Dose, col_trend = FALSE)
#'

plot_dvconc <- function(data,
                        dv_var = "DV",
                        idv_var = "CONC",
                        col_var = NULL,
                        col_trend = FALSE,
                        loess = TRUE,
                        linear = FALSE,
                        se_loess = FALSE,
                        se_linear = FALSE,
                        ref = NULL,
                        log_y = FALSE,
                        show_caption = TRUE,
                        style = NULL,
                        ...){

  dv_var_str  <- resolve_var(rlang::enquo(dv_var))
  idv_var_str <- resolve_var(rlang::enquo(idv_var))
  col_var_str <- resolve_var(rlang::enquo(col_var), nullable = TRUE)

  if (!is.null(col_var_str) && !isTRUE(col_trend)) {
    warning("`col_var` colors observations but trend lines are not stratified. Set `col_trend = TRUE` to stratify trend lines by color.", call. = FALSE)
  }

  #Checks
  check_df(data, "data")
  check_varsindf(data, dv_var_str, "data", "dv_var")
  check_varsindf(data, idv_var_str, "data", "idv_var")
  check_varsindf(data, col_var_str, "data", "col_var")
  if(!is.null(col_var_str)) {check_factor(data, col_var_str, "col_var")}

  ##Handle DV and IDV Variables
  data <- dplyr::rename(data, dplyr::any_of(c(DV = dv_var_str, IDV = idv_var_str)))

  ##Coerce Color Variable to a Factor
  if(!is.null(col_var_str)){data[[col_var_str]] <- factor(data[[col_var_str]])}

  #Determine Caption
  caption <- caption_dvconc(ref, loess, linear, se_loess, se_linear)

  #Resolve style (log_y arg drives the y axis)
  plotstyle <- if (is.null(style)) style_dvconc() else style
  plotstyle <- ggstylekit::set_style(plotstyle, logy = isTRUE(log_y))


###Plot

  # Initialize plot. Color is mapped globally only when trends are stratified
  # (`col_trend`); otherwise `col_var` colors observations at the layer level so
  # trend lines stay unstratified.
  base_aes <- if (isTRUE(col_trend) && !is.null(col_var_str)) {
    ggplot2::aes(x = .data[["IDV"]], y = .data[["DV"]],
                 color = .data[[col_var_str]], group = .data[[col_var_str]])
  } else {
    ggplot2::aes(x = .data[["IDV"]], y = .data[["DV"]])
  }
  plot <- ggplot2::ggplot(data, base_aes)

  #Reference Line
  plot <- add_ref_layer_style(plot, ref)

  #Plot Trend Lines
  plot <- add_trend_layers_style(plot, "loess", loess, se_loess, "loess",
                                 col_var_str, col_trend, ...)
  plot <- add_trend_layers_style(plot, "lm", linear, se_linear, "linear",
                                 col_var_str, col_trend)

  #Add observations (color at the layer level unless trends are stratified)
  plot <- add_obs_layers_style(plot, id_var_str = NULL,
                               col_var_str = if (isTRUE(col_trend)) NULL else col_var_str)

  #Caption
  if(isTRUE(show_caption)) plot <- plot + ggplot2::labs(caption = caption)

  ggstylekit::style_plot(plot, plotstyle)
}





#' Define a caption for `plot_dvconc`
#'
#' @inheritParams plot_dvconc
#'
#' @return a `character` string containing the plot caption
#' @keywords internal
#' @noRd

caption_dvconc <- function(ref, loess, linear, se_loess, se_linear){

  ref_lab <- if (!is.null(ref)) paste0("\n Reference line at y = ", ref) else ""

  fit_labels <- list(
    "FALSE.FALSE.FALSE.FALSE" = "",
    "TRUE.FALSE.FALSE.FALSE"  = "\n LOESS fit overlaid",
    "TRUE.FALSE.TRUE.FALSE"   = "\n LOESS fit overlaid with 95% CI",
    "FALSE.TRUE.FALSE.FALSE"  = "\n Linear fit overlaid",
    "FALSE.TRUE.FALSE.TRUE"   = "\n Linear fit overlaid with 95% CIs",
    "TRUE.TRUE.FALSE.FALSE"   = "\n LOESS and linear fits overlaid",
    "TRUE.TRUE.TRUE.TRUE"     = "\n LOESS and linear fits overlaid with 95% CIs",
    "TRUE.TRUE.TRUE.FALSE"    = "\n LOESS fit with 95% CI and linear fit overlaid",
    "TRUE.TRUE.FALSE.TRUE"    = "\n LOESS fit and linear fit with 95% CI overlaid"
  )

  key <- paste(loess, linear, se_loess, se_linear, sep = ".")
  fit_lab <- fit_labels[[key]]

  paste("Points are observations", ref_lab, fit_lab)
}






