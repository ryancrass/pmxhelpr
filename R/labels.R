#' Internal Helper: Numeric sort key for a human-written display label
#'
#' @description Reads the first number out of each label so level labels can be
#'    ordered by value rather than lexicographically. The sort key behind
#'    `order_col_factor()`, which orders both colour/stratification levels and
#'    the forest covariate level rows ([df_forest()]).
#'
#'    This is a **sort key**, not a number parser, and is named accordingly: it
#'    reads `"1e3"` as `1`, `"1,000 mg"` as `1`, and `"2.5.1"` as `2.5`. Its uses
#'    only need a monotone ordering of human-written labels, so these are
#'    acceptable; do not "fix" it into a full parser.
#'
#'    A `-` counts as a minus sign only where a sign can legally appear: at the
#'    start of the label, or after whitespace or an open paren. Anywhere else it
#'    is a hyphen joining a compound (`"Weight-50 kg"`) or a range
#'    (`"50-60 kg"`), and reading it as a sign would sort the label as negative.
#'
#' @param x Character vector of labels.
#'
#' @return Numeric vector the length of `x`. **Contract:** `NA_real_` where no
#'    number can be read, and for `NA` input -- never a sentinel value.
#'    `order_col_factor()` gates on `NA` to decide whether to sort numerically
#'    at all, so substituting `Inf` or `0` would silently change behaviour.
#' @keywords internal
#' @noRd
label_sort_num <- function(x) {
  flat <- gsub("([^[:space:](])-", "\\1 ", x)
  suppressWarnings(as.numeric(
    sub("^[^0-9-]*(-?[0-9]*\\.?[0-9]+).*$", "\\1", flat)
  ))
}


#' Internal Helper: Coerce a color/stratification variable to an ordered factor
#'
#' @description Since ggstylekit (>= 0.4.0) drives discrete legends and per-series
#'    value assignment from a factor's declared level order, `col_var` needs a
#'    sensible order before it is mapped. The rule is:
#'
#'    + Existing factor: levels preserved (drop unused). Use [var_addn()] or a
#'       pre-releveled factor to control order deliberately.
#'    + Numeric: `factor()` default, i.e. ascending by magnitude.
#'    + Character where every label carries a number (e.g. dose labels like
#'       `"200 mg"`, or `"Dose 5 mg"`): ordered numerically by that number,
#'       sign included, so `"5 mg"` sorts before `"20 mg"` before `"200 mg"`
#'       rather than lexicographically. The number need not lead the label.
#'       If two labels share a number (e.g. `"Mild (60-89)"`,
#'       `"Moderate (30-59)"`, `"Severe (<30)"`), the order is ambiguous and
#'       first-appearance order is kept.
#'    + Any other character (including mixed numeric / non-numeric labels such as
#'       `c("Placebo", "5 mg")`, where placement is ambiguous): first-appearance
#'       order. Never lexicographic.
#'
#' @param x A vector to coerce to a factor.
#'
#' @return A factor with levels ordered per the rule above.
#' @keywords internal
#' @noRd
order_col_factor <- function(x) {
  if (is.factor(x))  return(factor(x))
  if (is.numeric(x)) return(factor(x))
  lev <- unique(as.character(x))
  num <- label_sort_num(lev)
  if (!anyNA(num) && !anyDuplicated(num)) {
    lev <- lev[order(num, seq_along(lev))]
  }
  factor(x, levels = lev)
}


#' Internal helper: format numbers to a fixed count of significant digits
#'
#' Pads trailing zeros so every value in a rendered label carries the same
#' number of significant digits (`1.00`, `0.800`, `23300`), unlike `signif()`
#' followed by coercion, which prints `1`, `0.8`, `23300` and falls back to
#' scientific notation for small magnitudes. Used to build the forest
#' label columns ([df_forest()]) and the dose-proportionality `PowerCI` label
#' ([df_doseprop()]).
#'
#' The decimal count of each value is set by `anchor`: `sigdigits`
#' significant digits of `anchor` fix the decimal places, and `x` is rounded
#' to them. The default (`anchor = x`) gives every value its own
#' `sigdigits` significant digits. A shared anchor gives every value the same
#' decimal places, so a column of labels aligns on the decimal point
#' (`2.32`, `0.90` rather than `2.32`, `0.903`).
#'
#' @param x Numeric vector.
#' @param sigdigits Integer scalar; significant digits to keep.
#' @param anchor Numeric vector recycled to the length of `x`; the value(s)
#'    whose `sigdigits` significant digits set the decimal places. A zero or
#'    non-finite anchor falls back to `sigdigits - 1` decimal places.
#'    Default `x`.
#'
#' @return Character vector the length of `x`; `NA` stays `NA`.
#' @keywords internal
fmt_sig <- function(x, sigdigits, anchor = x) {
  anchor <- rep_len(abs(anchor), length(x))
  ## signif() first, so an anchor that rounds up a magnitude (0.9996 -> 1.00)
  ## takes the decimals of the rounded value.
  mag <- floor(log10(signif(anchor, sigdigits)))
  mag[!is.finite(mag)] <- 0
  d <- sigdigits - 1L - mag
  ## `+ 0` turns a rounded negative zero into a positive one ("-0.00" -> "0.00").
  out <- sprintf("%.*f", as.integer(pmax(d, 0)), round(x, d) + 0)
  out[is.na(x)] <- NA_character_
  out
}
