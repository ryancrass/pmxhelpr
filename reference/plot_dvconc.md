# Plot a dependent variable versus concentration

Plot a dependent variable versus concentration

## Usage

``` r
plot_dvconc(
  data,
  dv_var = "DV",
  idv_var = "CONC",
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
  ...
)
```

## Arguments

- data:

  Input dataset.

- dv_var:

  Character name of the DV variable in `data`.

- idv_var:

  Independent variable. Default is `"CONC"`.

- col_var:

  Character string of the name of the variable to map to the color
  aesthetic.

- col_trend:

  Logical indicating if the variable specified in `col_var` should be
  used to stratify trend lines

- loess:

  Logical indicating if a loess smoother fit should be shown. Default is
  `TRUE`

- linear:

  Logical indicating if a linear regression fit should be shown. Default
  is `FALSE`.

- se_loess:

  Logical indicating if the standard error should be shown for the loess
  fit. Default is `FALSE`

- se_linear:

  Logical indicating if the standard error should be shown for the
  linear fit. Default is `FALSE`

- cfb:

  Logical indicating if dependent variable is a change from baseline.
  Plots a reference line at y = cfb_baseline. Default is `FALSE`.

- cfb_base:

  Value for y-intercept when cfb = `TRUE`. Default is 0.

- ylab:

  Character string specifying the y-axis label: Default is `"Response"`.

- xlab:

  Character string specifying the x-axis label: Default is
  `"Drug Concentration"`

- log_y:

  Logical indicator for log10 transformation of the y-axis.

- log_x:

  Logical indicator for log10 transformation of the x-axis.

- show_caption:

  Logical indicating if a caption should be show describing the data
  plotted

- theme:

  Named list of aesthetic parameters to be supplied to the plot.
  Defaults can be viewed by running
  [`plot_dvconc_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvconc_theme.md)
  with no arguments.

- ...:

  Additional arguments passed to
  [`geom_smooth()`](https://ggplot2.tidyverse.org/reference/geom_smooth.html)

## Value

A `ggplot2` plot object

## Examples

``` r
data <- df_addn(dplyr::mutate(data_sad_pd, Dose=DOSE), grp_var="Dose", sep="mg")
#> Joining with `by = join_by(Dose)`
plot_dvconc(data, dv_var = "ODV", idv_var = "CONC", col_var = "Dose", col_trend = FALSE)
#> `geom_smooth()` using formula = 'y ~ x'
#> Warning: Removed 720 rows containing non-finite outside the scale range
#> (`stat_smooth()`).
#> Warning: Removed 720 rows containing missing values or values outside the scale range
#> (`geom_point()`).

```
