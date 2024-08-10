

plot_ctsum <- function(data,
                       ctvars = c(TIME = "TIME",DV = "DV", NTIME = "NTIME"),
                       sumfun = "mean",
                       col_var = "none",
                       facet_vars = "none",
                       cfb = FALSE,
                       #points = TRUE, #Not yet implemented
                       log_y = FALSE,
                       xlab = "Time",
                       ylab = "Concentration"){

  facet_formula <-
    if(facet_vars[1] == "none") {
      as.formula(paste("~", "`All Subjects`"))
      } else if (length(facet_vars) == 1){
        as.formula(paste("~", facet_vars))
        } else  {
          as.formula(paste(facet_vars, collapse = " ~ "))
          }

  data <- data |>
    dplyr::rename(dplyr::any_of(ctvars)) |>
    dplyr::mutate(col_var_f = factor(!!sym(col_var)),
                  `All Subjects` = "All Subjects")

  if(col_var == "none") data$col_var_f <- "All Subjects"

  plot <- ggplot(data, aes(y = DV, x = TIME, col = col_var_f)) +
    ggplot2::geom_point(shape=1, size=3) +
    ggplot2::stat_summary(aes(y = DV, x = NTIME, col = col_var_f), fun = gsub("_sdl", "", sumfun), geom= "point")+
    ggplot2::stat_summary(aes(y = DV, x = NTIME, col = col_var_f), fun = gsub("_sdl", "", sumfun), geom= "line", linewidth=1.1)+
    ggplot2::facet_wrap(facets = facet_formula)+
    ggplot2::labs(x=xlab, y=ylab, color = ifelse(is.null(col_var), NA,col_var))

  if(sumfun == "mean_sdl") plot <- plot +
    ggplot2::stat_summary(aes(y = DV, x = NTIME, col = col_var_f),
                          fun.data = sumfun, fun.args = list(mult = 1), geom= "errorbar", linewidth=1.1)

  if(log_y==TRUE) plot <- plot +
      ggplot2::scale_y_log10()

  if(cfb == TRUE) plot <- plot +
      ggplot2::geom_hline(yintercept = 0, linewidth = 1, linetype = "dashed")

  return(plot)
}
