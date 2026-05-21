# Internal helper: Prepare the plot environment for DV vs time family plots

Generates caption text, merges theme overrides with defaults, and
computes error bar width.

## Usage

``` r
prep_plot_env(data, cent, log_y, theme, theme_fn)
```

## Arguments

- data:

  data.frame containing processed data with `NTIME` column.

- cent:

  Character string specifying central tendency measure.

- log_y:

  Logical indicating log-scale y-axis.

- theme:

  User-supplied theme overrides (named list), or `NULL`.

- theme_fn:

  Theme factory function (e.g., `plot_dvtime_theme`).

## Value

A named list with elements `caption`, `plottheme`, and `width`.

## Examples

``` r
data <- dplyr::rename(dplyr::filter(data_sad, CMT %in% c(1,2)), DV = ODV)
env <- pmxhelpr:::prep_plot_env(data, cent = "mean", log_y = FALSE,
  theme = NULL, theme_fn = plot_dvtime_theme)
```
