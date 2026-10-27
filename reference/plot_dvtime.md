# Plot a dependent variable versus time

Plot a dependent variable versus time

## Usage

``` r
plot_dvtime(
  data,
  dv_var = "DV",
  time_var = "TIME",
  ntime_var = "NTIME",
  col_var = NULL,
  id_var = NULL,
  dose_var = "DOSE",
  loq = NULL,
  loq_method = 0,
  show_loq_value = TRUE,
  cent = c("mean", "mean_sdl", "mean_sdl_upper", "median", "median_iqr", "none"),
  show_obs = TRUE,
  dosenorm = FALSE,
  ref = NULL,
  log_y = FALSE,
  show_caption = TRUE,
  style = NULL
)
```

## Arguments

- data:

  Input dataset.

- dv_var:

  Column containing the dependent variable. Accepts bare names or
  strings. Default is `DV`.

- time_var:

  Column containing the actual time variable. Accepts bare names or
  strings. Default is `TIME`.

- ntime_var:

  Column containing the nominal time variable. Accepts bare names or
  strings. Default is `NTIME`.

- col_var:

  Column to map to the color aesthetic. Accepts bare names or strings.
  Default is `NULL`.

- id_var:

  Column to group observations for spaghetti lines. Accepts bare names
  or strings. Default is `NULL` (no spaghetti lines). Specifying a
  column (e.g., `id_var = ID`) enables spaghetti lines connecting
  observations within each level of the variable.

- dose_var:

  Column to use in dosenormalization when `dosenorm` = TRUE. Accepts
  bare names or strings. Default is `DOSE`.

- loq:

  Numeric value of the lower limit of quantification (LLOQ) for the
  assay. Must be coercible to a numeric if specified. Can be `NULL` if
  variable `LLOQ` is present in `data`, in which case each row is
  imputed at its own `LLOQ` and the LLOQ line is drawn at the highest
  value. Specifying this argument implies that `DV` is missing in `data`
  where \< LLOQ. Has no effect with `loq_method = 0` (the default),
  which emits a message.

- loq_method:

  Method for handling data below the lower limit of quantification (BLQ)
  in the plot.

  Options are:

      + No handling: `0` or `"none"`, Plot input dataset `DV` vs `TIME` as is. (default)
      + Impute Post-dose: `1` or `"postdose"`, Impute all BLQ data at `TIME` <= 0 to 0 and all BLQ data at `TIME` > 0 to 1/2 x `loq`.
         Useful for plotting concentration-time data with some data BLQ on the linear scale
      + Impute All: `2` or `"all"`,Impute all BLQ data to 1/2 x `loq`.
         Useful for plotting concentration-time data with some data BLQ on the log scale where 0 cannot be displayed

- show_loq_value:

  Logical. If `TRUE` (default), the LLOQ line's legend entry is titled
  `"LLOQ"` and keyed by its value. If `FALSE`, the title is dropped and
  the entry is labeled `"LLOQ"` without the value. Applies only when the
  LLOQ line is drawn (`loq_method` of `1` or `2`, `dosenorm = FALSE`).

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

- show_obs:

  Logical indicating if observed data points should be shown. Default is
  `TRUE`. `FALSE` omits the point layer entirely, so hidden points do
  not affect axis limits (unlike setting `alphas = c(obs_point = 0)` in
  the style). Spaghetti lines are controlled separately by `id_var`.

- dosenorm:

  logical indicating if observed data points should be dose normalized.
  Default is `FALSE`, Requires variable specified in `dose_var` to be
  present in `data`

- ref:

  Numeric y-intercept for a horizontal reference line, or `NULL` for no
  reference line. For example, `ref = 0` draws a baseline reference for
  change-from-baseline data.

- log_y:

  Logical indicator for log10 transformation of the y-axis. Also
  controls whether the caption reports arithmetic or geometric mean when
  `show_caption = TRUE`.

- show_caption:

  Logical indicating if a caption should be shown describing the data
  plotted. A `caption` set in the style is drawn instead, whatever this
  value.

- style:

  A
  [`ggstylekit::style_spec()`](https://rdrr.io/pkg/ggstylekit/man/style_spec.html)
  controlling plot aesthetics. Defaults to
  [`style_dvtime()`](https://ryancrass.github.io/pmxhelpr/reference/style_dvtime.md);
  view the defaults by running
  [`style_dvtime()`](https://ryancrass.github.io/pmxhelpr/reference/style_dvtime.md)
  with no arguments. Customize by passing `style = style_dvtime(...)`.
  The error bar cap width is the style's `errorbar_width` field; when
  unset it defaults to 2.5% of the maximum nominal time (`ntime_var`).

## Value

A `ggplot2` plot object

## See also

Other exploratory analysis:
[`plot_dvconc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvconc.md),
[`style_dvconc()`](https://ryancrass.github.io/pmxhelpr/reference/style_dvconc.md),
[`style_dvtime()`](https://ryancrass.github.io/pmxhelpr/reference/style_dvtime.md)

## Examples

``` r
data_sad_pk <- dplyr::filter(data_sad, CMT %in% c(1,2))
data <- dplyr::mutate(data_sad_pk, Dose = var_addn(DOSE, ID, sep = "mg"))
plot_dvtime(data, dv_var = ODV, cent = "median", col_var = Dose)
#> Warning: Removed 178 rows containing non-finite outside the scale range
#> (`stat_summary()`).
#> Warning: Removed 178 rows containing non-finite outside the scale range
#> (`stat_summary()`).
#> Warning: Removed 178 rows containing missing values or values outside the scale range
#> (`geom_point()`).

```
