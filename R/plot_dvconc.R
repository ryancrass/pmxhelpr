#' Plot a dependent variable versus concentration
#'
#' @param data Input dataset.
#' @param idv_var Independent variable column. Accepts bare names or strings. Default is `CONC`.
#' @param col_trend Logical indicating if the variable specified in `col_var` should be used to stratify trend lines
#' @param loess Logical indicating if a loess smoother fit should be shown. Default is `TRUE`
#' @param se_loess Logical indicating if the standard error should be shown for the loess fit. Default is `FALSE`
#' @param linear Logical indicating if a linear regression fit should be shown. Default is `FALSE`.
#' @param se_linear Logical indicating if the standard error should be shown for the linear fit. Default is `FALSE`
#' @param log_y Logical indicator for log10 transformation of the y-axis.
#' @param show_caption Logical indicating if a caption should be show describing the data plotted
#' @param theme Theme object created by [plot_dvconc_theme()].
#'    Defaults can be viewed by running `plot_dvconc_theme()` with no arguments.
#' @param ... Additional arguments passed to `geom_smooth()`
#' @inheritParams plot_dvtime
#'
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
                        dv_var = DV,
                        idv_var = CONC,
                        col_var = NULL,
                        col_trend = FALSE,
                        loess = TRUE,
                        linear = FALSE,
                        se_loess = FALSE,
                        se_linear = FALSE,
                        cfb = FALSE,
                        cfb_base = 0,
                        log_y = FALSE,
                        show_caption = TRUE,
                        theme = NULL,
                        ...){

  dv_var_str  <- resolve_var(rlang::enquo(dv_var))
  idv_var_str <- resolve_var(rlang::enquo(idv_var))
  col_var_str <- resolve_var(rlang::enquo(col_var), nullable = TRUE)

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
  caption <- caption_dvconc(cfb, loess, linear, se_loess, se_linear)

  #Determine aesthetics
  plottheme <- merge_theme(theme, plot_dvconc_theme())


###Plot

  #Initialize Plot
  if(!isTRUE(col_trend)) {
    plot <- init_plot(data, "IDV", "DV")
  } else {
    plot <- init_plot(data, "IDV", "DV", col_var_str) +
      ggplot2::aes(group = .data[[col_var_str]])
  }

  #Reference Lines: Y=cfb_base (cfb = TRUE)
  plot <- add_cfb_layers(plot, cfb, cfb_base, plottheme)


  #Plot Trend Lines
  plot <- add_trend_layers(plot, "loess", loess, se_loess, plottheme,
                           col_var_str, col_trend, ...)
  plot <- add_trend_layers(plot, "lm", linear, se_linear, plottheme,
                           col_var_str, col_trend)

  #Add observations
  plot <- add_obs_point_layer(plot, plottheme, col_var_str)

  #Log Transform
  if(isTRUE(log_y)) plot <- plot + ggplot2::scale_y_log10(guide = "axis_logticks")

  #Caption
  if(isTRUE(show_caption)) plot <- plot + ggplot2::labs(caption = caption)

  return(plot)
}





#' Define a caption for `plot_dvconc`
#'
#' @inheritParams plot_dvconc
#'
#' @return a `character` string containing the plot caption
#' @export caption_dvconc
#' @keywords internal
#'
#' @examples
#' caption_dvconc(cfb=FALSE, loess = TRUE, linear = FALSE, se_loess = FALSE, se_linear = FALSE)

caption_dvconc <- function(cfb, loess, linear, se_loess, se_linear){

  cfb_lab <- "\n Reference line indicates the null response (no change from baseline)"

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

  paste("Points are observations",
        ifelse(cfb == TRUE, cfb_lab, ""),
        fit_lab)
}






