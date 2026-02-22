# Define a caption for `plot_dvconc`

Define a caption for `plot_dvconc`

## Usage

``` r
dvconc_caption(cfb, loess, linear, se_loess, se_linear)
```

## Arguments

- cfb:

  Logical indicating if dependent variable is a change from baseline.
  Plots a reference line at y = cfb_baseline. Default is `FALSE`.

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

## Value

a `character` string containing the plot caption

## Examples

``` r
dvconc_caption(cfb=FALSE, loess = TRUE, linear = FALSE, se_loess = FALSE, se_linear = FALSE)
#> [1] "Points are observations  \n LOESS fit overlaid"
```
