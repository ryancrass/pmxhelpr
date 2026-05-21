# Internal helper: Prepare data for plotting of a dependent variable versus time

Validates inputs, renames time and output variables to internal
standards, applies BLQ imputation, and optionally applies dose
normalization.

## Usage

``` r
df_prep_dvtime(
  data,
  time_var_str = "TIME",
  ntime_var_str = "NTIME",
  dv_var_str = "DV",
  pred_var_str = NULL,
  ipred_var_str = NULL,
  dose_var_str = NULL,
  col_var_str = NULL,
  id_var_str = NULL,
  loq = NULL,
  loq_method = 0,
  dosenorm = FALSE,
  ref = NULL,
  blq_mode = c("obs", "all")
)
```

## Arguments

- data:

  Input data.frame containing pharmacometric data.

- time_var_str:

  String name of the actual time column in `data`.

- ntime_var_str:

  String name of the nominal time column in `data`.

- dv_var_str:

  String name of the dependent variable column in `data`.

- pred_var_str:

  String name of the population prediction column in `data`, or `NULL`.

- ipred_var_str:

  String name of the individual prediction column in `data`, or `NULL`.

- dose_var_str:

  String specifying the dose column name, or `NULL`.

- col_var_str:

  String specifying the color variable column name, or `NULL`.

- id_var_str:

  String specifying the group column name for spaghetti lines, or
  `NULL`.

- loq:

  Numeric value of LLOQ, or `NULL`.

- loq_method:

  Integer (0, 1, or 2) specifying BLQ handling method.

- dosenorm:

  Logical indicating if dose normalization should be applied.

- ref:

  Numeric y-intercept for a horizontal reference line, or `NULL` for
  none.

- blq_mode:

  One of `"obs"` (default) or `"all"`. Controls which columns receive
  BLQ imputation: `"obs"` imputes the renamed `DV` only; `"all"`
  additionally imputes any `PRED` / `IPRED` columns that were renamed
  via `pred_var_str` / `ipred_var_str`. Has no effect when
  `loq_method = 0`.

## Value

A named list with elements `data` (processed data.frame) and `lloq`
(numeric LLOQ value).

## Examples

``` r
data <- dplyr::rename(dplyr::filter(data_sad, CMT %in% c(1,2)), DV = ODV)
prep <- pmxhelpr:::df_prep_dvtime(data, time_var_str = "TIME", ntime_var_str = "NTIME")
head(prep$data)
#> # A tibble: 6 × 25
#>      ID  TIME NTIME  NDAY  DOSE   AMT  EVID    DV    LDV   CFB  CONC  LINE   CMT
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1     1  0      0       1    10    NA     0 NA    NA        NA    NA     1     2
#> 2     1  0.48   0.5     1    10    NA     0 NA    NA        NA    NA     3     2
#> 3     1  0.81   1       1    10    NA     0  2.02  0.703    NA    NA     4     2
#> 4     1  1.49   1.5     1    10    NA     0  4.02  1.39     NA    NA     5     2
#> 5     1  2.11   2       1    10    NA     0  3.5   1.25     NA    NA     6     2
#> 6     1  3.05   3       1    10    NA     0  7.18  1.97     NA    NA     7     2
#> # ℹ 12 more variables: MDV <dbl>, BLQ <dbl>, LLOQ <dbl>, FOOD <dbl>,
#> #   SEXF <dbl>, RACE <dbl>, AGEBL <int>, WTBL <dbl>, SCRBL <dbl>, CRCLBL <dbl>,
#> #   USUBJID <chr>, PART <chr>
```
