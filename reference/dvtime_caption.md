# Define a caption for `plot_dvtime`

Define a caption for `plot_dvtime`

## Usage

``` r
dvtime_caption(cent, log_y = FALSE, obs_dv = TRUE, grp_dv = FALSE)
```

## Arguments

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

- log_y:

  Logical indicator for log10 transformation of the y-axis.

- obs_dv:

  Logical indicating if observed data points should be shown. Default is
  `TRUE`.

- grp_dv:

  Logical indicating if observed data points should be connected within
  a group (i.e., spaghetti plot). Default is `FALSE`.

## Value

a `character` string containing the plot caption

## Examples

``` r
dvtime_caption(cent = "mean")
#> [1] "Solid circles and thick lines are the mean\nOpen circles are observations"
dvtime_caption(cent = "mean", log_y = TRUE)
#> [1] "Solid circles and thick lines are the geometric mean\nOpen circles are observations"
```
