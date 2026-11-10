#' Internal: resolve the LOQ source onto a row-aligned `LOQ` column
#'
#' A supplied `loq` fills `LOQ` for every row; otherwise an `LLOQ` column is
#' inherited per row (with a message); otherwise `data` is returned unchanged.
#' Callers validate `loq` themselves.
#'
#' @param data Input data.
#' @param loq Numeric scalar, or `NULL`.
#' @return `data`, with `LOQ` when a source exists.
#' @noRd
resolve_loq <- function(data, loq) {
  if (!is.null(loq)) {
    data$LOQ <- loq
  } else if ("LLOQ" %in% colnames(data)) {
    data$LOQ <- data$LLOQ
    pmx_inform("Inheriting per-row {.arg loq} from {.var LLOQ} column in {.arg data}.",
               "loq_inherit")
  }
  data
}


#' Internal: LLOQ reference lines, one per panel at the highest LLOQ
#'
#' With several LLOQ values the line is drawn at the highest (the most
#' censoring); imputation and censoring stay per row.
#'
#' @param values Numeric LLOQ values for a single (global) line.
#' @param data,by A data.frame with a `LOQ` column and the column(s) `by` that
#'    define the panels, for one line per panel instead.
#' @return A data.frame with `yintercept` (and the `by` columns).
#' @noRd
loq_lines <- function(values = NULL, data = NULL, by = NULL) {
  if (is.null(by)) return(data.frame(yintercept = max(values, na.rm = TRUE)))
  data[!is.na(data$LOQ), c(by, "LOQ"), drop = FALSE] |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::summarise(yintercept = max(.data$LOQ), .groups = "drop") |>
    as.data.frame()
}


#' Internal: the LLOQ reference line layer
#'
#' Fixed aesthetics come from the style's `"loq_line"` series. With
#' `legend_label`, linetype is mapped to that label so the line joins a manual
#' linetype legend (the caller adds the scale).
#'
#' @param lines Output of `loq_lines()`.
#' @param style A `ggstylekit_style_spec`.
#' @param legend_label Legend key label, or `NULL` for no legend entry.
#' @return A `geom_hline()` layer.
#' @noRd
loq_line_layer <- function(lines, style, legend_label = NULL) {
  el <- series_aes(style, "loq_line")
  if (is.null(legend_label)) {
    mapping <- ggplot2::aes(yintercept = .data$yintercept)
    linetype <- el$linetype
  } else {
    lines$.loq_label <- legend_label
    mapping <- ggplot2::aes(yintercept = .data$yintercept, linetype = .data$.loq_label)
    linetype <- NULL
  }
  args <- compact(list(
    data = lines, mapping = mapping, inherit.aes = FALSE,
    linetype = linetype, linewidth = el$linewidth, alpha = el$alpha,
    color = el$colour))
  do.call(ggplot2::geom_hline, args)
}
