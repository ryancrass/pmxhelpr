# Plot a dependent variable versus time

Plot a dependent variable versus time

## Usage

``` r
plot_dvtime(
  data,
  dv_var = "DV",
  time_vars = c(TIME = "TIME", NTIME = "NTIME"),
  timeu = "hours",
  col_var = NULL,
  grp_var = "ID",
  dose_var = "DOSE",
  loq = NULL,
  loq_method = 0,
  cent = "mean",
  obs_dv = TRUE,
  grp_dv = FALSE,
  dosenorm = FALSE,
  cfb = FALSE,
  cfb_base = 0,
  ylab = "Concentration",
  log_y = FALSE,
  show_caption = TRUE,
  n_breaks = 8,
  theme = NULL
)
```

## Arguments

- data:

  Input dataset.

- dv_var:

  Character name of the DV variable in `data`.

- time_vars:

  Names of actual and nominal time variables. Must be named character
  vector. Defaults is: c(`TIME`=`"TIME"`, `NTIME`=`"NTIME"`).

- timeu:

  Character string specifying units for the time variable. Passed to
  `breaks_time` and assigned to default x-axis label. Options include:

  - "hours" (default)

  - "days"

  - "weeks"

  - "months"

- col_var:

  Character string of the name of the variable to map to the color
  aesthetic.

- grp_var:

  Character string of the variable to map to the group aesthetic.
  Default is `"ID"`

- dose_var:

  Character string of the variable to use in dosenormalization when
  `dosenorm` = TRUE. Default is `"DOSE"`.

- loq:

  Numeric value of the lower limit of quantification (LLOQ) for the
  assay. Must be coercible to a numeric if specified. Can be `NULL` if
  variable `LLOQ` is present in `data` Specifying this argument implies
  that `DV` is missing in `data` where \< LLOQ.

- loq_method:

  Method for handling data below the lower limit of quantification (BLQ)
  in the plot.

  Options are:

      + `0` : No handling. Plot input dataset `DV` vs `TIME` as is. (default)
      + `1` : Impute all BLQ data at `TIME` <= 0 to 0 and all BLQ data at `TIME` > 0 to 1/2 x `loq`.
         Useful for plotting concentration-time data with some data BLQ on the linear scale
      + `2` : Impute all BLQ data at `TIME` <= 0 to 1/2 x `loq` and all BLQ data at `TIME` > 0 to 1/2 x `loq`.
         Useful for plotting concentration-time data with some data BLQ on the log scale where 0 cannot be displayed

- cent:

  Character string specifying the central tendency measure to plot.

  Options are:

  - Mean only: `"mean"` (default)

  - Mean +/- Standard Deviation (upper and lower error bar):
    `"mean_sdl"`

  - Mean + Standard Deviation (upper error bar only): `"mean_sdl_upper"`

  - Median only: `"median"`

  - Median +/- Interquartile Range: `median_iqr`

  - None: `"none"`

- obs_dv:

  Logical indicating if observed data points should be shown. Default is
  `TRUE`.

- grp_dv:

  Logical indicating if observed data points should be connected within
  a group (i.e., spaghetti plot). Default is `FALSE`.

- dosenorm:

  logical indicating if observed data points should be dose normalized.
  Default is `FALSE`, Requires variable specified in `dose_var` to be
  present in `data`

- cfb:

  Logical indicating if dependent variable is a change from baseline.
  Plots a reference line at y = cfb_baseline. Default is `FALSE`.

- cfb_base:

  Value for y-intercept when cfb = `TRUE`. Default is 0.

- ylab:

  Character string specifing the y-axis label: Default is
  `"Concentration"`.

- log_y:

  Logical indicator for log10 transformation of the y-axis.

- show_caption:

  Logical indicating if a caption should be show describing the data
  plotted

- n_breaks:

  Number of breaks requested for x-axis. Default is 8.

- theme:

  Named list of aesthetic parameters to be supplied to the plot.
  Defaults can be viewed by running
  [`plot_dvtime_theme()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvtime_theme.md)
  with no arguments. Default `width_errorbar` is 2.5% of maximum
  `NTIME`.

## Value

A `ggplot2` plot object

## Examples

``` r
data <- df_addn(dplyr::mutate(data_sad,Dose=DOSE), grp_var="Dose", sep = "mg")
#> Joining with `by = join_by(Dose)`
plot_dvtime(data, dv_var = "ODV", cent = "median", col_var = "Dose")
#> Warning: Removed 169 rows containing non-finite outside the scale range
#> (`stat_summary()`).
#> Warning: Removed 169 rows containing non-finite outside the scale range
#> (`stat_summary()`).
#> Warning: Removed 169 rows containing missing values or values outside the scale range
#> (`geom_point()`).

```
