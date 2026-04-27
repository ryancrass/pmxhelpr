#' Plot a dependent variable versus concentration
#'
#' @param data Input dataset.
#' @param idv_var Independent variable column. Accepts bare names or strings. Default is `CONC`.
#' @param col_trend Logical indicating if the variable specified in `col_var` should be used to stratify trend lines
#' @param loess Logical indicating if a loess smoother fit should be shown. Default is `TRUE`
#' @param se_loess Logical indicating if the standard error should be shown for the loess fit. Default is `FALSE`
#' @param linear Logical indicating if a linear regression fit should be shown. Default is `FALSE`.
#' @param se_linear Logical indicating if the standard error should be shown for the linear fit. Default is `FALSE`
#' @param ylab Character string specifying the y-axis label: Default is `"Response"`.
#' @param xlab Character string specifying the x-axis label: Default is `"Drug Concentration"`
#' @param log_x Logical indicator for log10 transformation of the x-axis.
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
#'data <- df_addn(dplyr::mutate(data_sad_pd, Dose = DOSE), grp_var = Dose, sep = "mg")
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
                        ylab = "Response",
                        xlab = "Drug Concentration",
                        log_y = FALSE,
                        log_x = FALSE,
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
  caption <- dvconc_caption(cfb, loess, linear, se_loess, se_linear)

  #Determine aesthetics
  plottheme <- merge_theme(theme, plot_dvconc_theme())


###Plot

  #Initialize Plot Aesthetics
  if(col_trend == FALSE) {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x=IDV, y=DV))
  } else {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x=IDV, y=DV,
                                               color = .data[[col_var_str]], group = .data[[col_var_str]]))
  }

  plot <- plot +
    ggplot2::labs(x=xlab, y=ylab) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank())

  #Reference Lines: Y=cfb_base (cfb = TRUE)
  if(cfb == TRUE) plot <- plot + ggplot2::geom_hline(yintercept = as.numeric(cfb_base),
                                                     linewidth = plottheme$ref$linewidth,
                                                     linetype = plottheme$ref$linetype,
                                                     alpha = plottheme$ref$alpha)


  #Plot Trend Lines
  if(col_trend == FALSE) {
    if(loess == TRUE) plot <- plot + ggplot2::geom_smooth(method = "loess", se = se_loess,
                                                          linewidth = plottheme$loess$linewidth,
                                                          linetype = plottheme$loess$linetype,
                                                          color = plottheme$loess$color,
                                                          fill = plottheme$loess$se_color,
                                                          alpha = plottheme$loess$se_alpha,
                                                          ...)

    if(linear == TRUE) plot <- plot + ggplot2::geom_smooth(method = "lm", se = se_linear,
                                                           linewidth = plottheme$linear$linewidth,
                                                           linetype = plottheme$linear$linetype,
                                                           color = plottheme$linear$color,
                                                           fill = plottheme$linear$se_color,
                                                           alpha = plottheme$linear$se_alpha)
  } else {
    if(loess == TRUE) plot <- plot + ggplot2::geom_smooth(ggplot2::aes(x=IDV, y=DV,
                                                              color = .data[[col_var_str]],
                                                              fill = .data[[col_var_str]]),
                                                          method = "loess", se = se_loess,
                                                          linewidth = plottheme$loess$linewidth,
                                                          linetype = plottheme$loess$linetype,
                                                          alpha = plottheme$loess$se_alpha,
                                                          ...)

    if(linear == TRUE) plot <- plot + ggplot2::geom_smooth(ggplot2::aes(x=IDV, y=DV,
                                                               color = .data[[col_var_str]],
                                                               fill = .data[[col_var_str]]),
                                                               method = "lm", se = se_linear,
                                                           linewidth = plottheme$linear$linewidth,
                                                           linetype = plottheme$linear$linetype,
                                                           alpha = plottheme$linear$se_alpha)
  }

  #Add observations
  if(is.null(col_var_str)){
    plot <- plot +
      ggplot2::geom_point(ggplot2::aes(x=IDV, y=DV),
                          shape=plottheme$obs$shape,
                          size=plottheme$obs$size,
                          alpha = plottheme$obs$alpha)
  } else {
    plot <- plot +
      ggplot2::geom_point(ggplot2::aes(x=IDV, y=DV, color = .data[[col_var_str]]),
                          shape=plottheme$obs$shape,
                          size=plottheme$obs$size,
                          alpha = plottheme$obs$alpha)
  }


  #Log Transform
  if(log_y == TRUE) plot <- plot + ggplot2::scale_y_log10(guide = "axis_logticks")
  if(log_x == TRUE) plot <- plot + ggplot2::scale_x_log10(guide = "axis_logticks")

  #Caption
  if(show_caption == TRUE) plot <- plot + ggplot2::labs(caption = caption)

  return(plot)
}





#' Define a caption for `plot_dvconc`
#'
#' @inheritParams plot_dvconc
#'
#' @return a `character` string containing the plot caption
#' @export dvconc_caption
#' @keywords internal
#'
#' @examples
#' dvconc_caption(cfb=FALSE, loess = TRUE, linear = FALSE, se_loess = FALSE, se_linear = FALSE)

dvconc_caption <- function(cfb, loess, linear, se_loess, se_linear){

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






