#' Plot a dependent variable versus concentration
#'
#' @param data Input dataset.
#' @param idv_var Independent variable. Default is `"CONC"`.
#' @param col_var Character string of the name of the variable to map to the color aesthetic.
#' @param loess Logical indicating if a loess smoother fit should be shown. Default is `TRUE`
#' @param se_loess Logical indicating if the standard error should be shown for the loess fit. Default is `FALSE`
#' @param linear Logical indicating if a linear regression fit should be shown. Default is `FALSE`.
#' @param se_linear Logical indicating if the standard error should be shown for the linear fit. Default is `FALSE`
#' @param cfb Logical indicating if dependent variable is a change from baseline.
#'    Plots a reference line at y = 0. Default is `FALSE`.
#' @param ylab Character string specifying the y-axis label: Default is `"Response"`.
#' @param xlab Character string specifying the x-axis label: Default is `"Drug Concentration"`
#' @param log_x Logical indicator for log10 transformation of the x-axis.
#' @param log_y Logical indicator for log10 transformation of the y-axis.
#' @param show_caption Logical indicating if a caption should be show describing the data plotted
#' @param theme Named list of aesthetic parameters to be supplied to the plot.
#'    Defaults can be viewed by running `plot_dvconc_theme()` with no arguments.
#'
#' @return A `ggplot2` plot object
#'
#' @export plot_dvconc
#'
#' @examples
#'data <- dplyr::mutate(data_sad, Dose = factor(DOSE))
#'plot_dvconc(data, dv_var = "ODV", col_var = "Dose")
#'

plot_dvconc <- function(data,
                        dv_var = "DV",
                        idv_var = "CONC",
                        col_var = NULL,
                        loess = TRUE,
                        linear = FALSE,
                        se_loess = FALSE,
                        se_linear = FALSE,
                        cfb = FALSE,
                        ylab = "Response",
                        xlab = "Drug Concentration",
                        log_y = FALSE,
                        log_x = FALSE,
                        show_caption = TRUE,
                        theme = NULL){


  #Checks
  check_df(data)
  check_varsindf(data, dv_var)
  check_varsindf(data, idv_var)
  check_varsindf(data, col_var)
  if(!is.null(col_var)) {check_factor(data, col_var)}

  ##Handle DV and IDV Variables
  data <- dplyr::rename(data, dplyr::any_of(c(DV = dv_var, CONC = idv_var)))

  ##Coerce Color Variable to a Factor
  if(!is.null(col_var)){data[[col_var]] <- factor(data[[col_var]])}

  #Determine Caption
  caption <- dvconc_caption(cfb, loess, linear, se_loess, se_linear)

  #Determine aesthetics
  plottheme <- list_update(theme, plot_dvconc_theme())


###Plot

  #Initialize Plot Aesthetics
  if(is.null(col_var)) {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x=CONC, y=DV))
  } else {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x=CONC, y=DV, color = !!dplyr::sym(col_var)))
  }

  plot <- plot +
    ggplot2::labs(x=xlab, y=ylab) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank())

  #Add observations
  plot <- plot +
    ggplot2::geom_point(shape=plottheme$shape_point_obs,
                        size=plottheme$size_point_obs,
                        alpha = plottheme$alpha_point_obs)

  #Reference Lines: Y=0 (cfb = TRUE)
  if(cfb == TRUE) plot <- plot + ggplot2::geom_hline(yintercept = 0,
                                                     linewidth = plottheme$linewidth_ref,
                                                     linetype = plottheme$linetype_ref,
                                                     alpha = plottheme$alpha_line_ref)


  #Plot Trend Lines
  if(loess == TRUE) plot <- plot + ggplot2::geom_smooth(ggplot2::aes(x=CONC, y=DV),
                                                       method = "loess", se = se_loess,
                                                       linewidth = plottheme$linewidth_cent,
                                                       linetype = plottheme$linetype_cent,
                                                       alpha = plottheme$alpha_line_cent)

  if(linear == TRUE) plot <- plot + ggplot2::geom_smooth(ggplot2::aes(x=CONC, y=DV),
                                                       method = "lm", se = se_linear,
                                                       linewidth = plottheme$linewidth_cent,
                                                       linetype = plottheme$linetype_cent,
                                                       alpha = plottheme$alpha_line_cent)

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
#' @export dvtime_caption
#'
#' @examples
#' dvtime_caption(cfb=FALSE, loess = TRUE, linear = FALSE, se_loess = FALSE, se_linear = FALSE)

dvconc_caption <- function(cfb, loess, linear, se_loess, se_linear){

  cfb_lab <- "\n Reference line indicates the null (no change from baseline)"

  capdf <- data.frame(
    loess = rep(c(FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)),
    linear = rep(c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)),
    se_loess = rep(c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE)),
    se_linear = rep(c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE)),
    label = c("",
              "\n LOESS fit overlaid","\n LOESS fit overlaid with 95% CI",
              "\n Linear fit overlaid","\n Linear fit overlaid with 95% CI",
              "\n LOESS and Linear fits overlaid","\n LOESS and Linear fits overlaid with 95% CI",
              "\n LOESS fit with 95% CI and linear fit overlaid","\n LOESS fit and Linear fit with 95% CI overlaid")
  )

  caption <- paste("Points are observations.",
                   ifelse(cfb==TRUE, cfb_lab, ""),
                   capdf$label[capdf$loess==loess &
                                 capdf$linear==linear &
                                 capdf$se_loess==se_loess &
                                 capdf$se_linear == se_linear]
)
  return(caption)
}


#' Customized Response versus drug concentration theme with pmxhelpr default aesthetics
#'
#' @param update list containing the plot elements to be updated.
#'    Run `plot_dvconc_theme()` with no arguments to view defaults.
#' @return a named list `list`
#' @export plot_dvconc_theme
#'
#' @examples
#' plot_dvconc_theme()
#' new_theme <- plot_conc_theme(update = list(linewidth_ref = 1))


plot_dvconc_theme <- function(update = NULL){
  defaults_list <- list(
    linewidth_ref = 0.5,
    linetype_ref = 2,
    alpha_line_ref = 1,

    shape_point_obs = 1,
    size_point_obs = 0.75,
    alpha_point_obs = 0.5,
    linewidth_obs = 0.5,
    linetype_obs = 1,
    alpha_line_obs = 0.5,

    linewidth_cent = 0.75,
    linetype_cent = 1,
    alpha_line_cent = 1
  )

  default_theme <- defaults_list
  theme <- list_update(update, default_theme)
  return(theme)
}

