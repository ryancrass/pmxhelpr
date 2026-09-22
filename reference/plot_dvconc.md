# Plot a dependent variable versus concentration

Unlike
[`plot_dvtime()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvtime.md)
and
[`plot_gof()`](https://ryancrass.github.io/pmxhelpr/reference/plot_gof.md),
this function does not filter dose rows internally. Pre-filter the input
to observation rows (typically by `CMT` or `EVID == 0`) before calling —
see the example below.

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
  ref = NULL,
  log_y = FALSE,
  show_caption = TRUE,
  style = NULL,
  ...
)
```

## Arguments

- data:

  Input dataset. Must contain only observation rows (no dose records).
  Filter by `CMT` or `EVID == 0` before passing.

- dv_var:

  Column containing the dependent variable. Accepts bare names or
  strings. Default is `DV`.

- idv_var:

  Independent variable column. Accepts bare names or strings. Default is
  `CONC`.

- col_var:

  Column to map to the color aesthetic. Accepts bare names or strings.
  Default is `NULL`.

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

- ref:

  Numeric y-intercept for a horizontal reference line, or `NULL` for no
  reference line. For example, `ref = 0` draws a baseline reference for
  change-from-baseline data.

- log_y:

  Logical indicator for log10 transformation of the y-axis.

- show_caption:

  Logical indicating if a caption should be shown describing the data
  plotted

- style:

  A
  [`ggstylekit::style_spec()`](https://rdrr.io/pkg/ggstylekit/man/style_spec.html)
  controlling plot aesthetics. Defaults to
  [`style_dvconc()`](https://ryancrass.github.io/pmxhelpr/reference/style_dvconc.md);
  view the defaults by running
  [`style_dvconc()`](https://ryancrass.github.io/pmxhelpr/reference/style_dvconc.md)
  with no arguments. Customize by passing `style = style_dvconc(...)`,
  or restyle the returned plot with
  [`restyle_plot()`](https://rdrr.io/pkg/ggstylekit/man/restyle_plot.html).

- ...:

  Additional arguments passed to
  [`geom_smooth()`](https://ggplot2.tidyverse.org/reference/geom_smooth.html)

## Value

A `ggplot2` plot object

## See also

Other exploratory analysis:
[`plot_dvtime()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvtime.md),
[`style_dvconc()`](https://ryancrass.github.io/pmxhelpr/reference/style_dvconc.md),
[`style_dvtime()`](https://ryancrass.github.io/pmxhelpr/reference/style_dvtime.md)

## Examples

``` r
data_sad_pd <- dplyr::filter(data_sad, CMT ==3)
data <- dplyr::mutate(data_sad_pd, Dose = var_addn(DOSE, ID, sep = "mg"))
plot_dvconc(data, dv_var = ODV, idv_var = CONC, col_var = Dose, col_trend = FALSE)
#> Warning: `col_var` colors observations but trend lines are not stratified. Set `col_trend = TRUE` to stratify trend lines by color.
#> `geom_smooth()` using formula = 'y ~ x'
#> `geom_smooth()` using formula = 'y ~ x'

```
