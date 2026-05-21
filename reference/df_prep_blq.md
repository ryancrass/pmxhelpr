# Internal helper: Apply BLQ censoring rules to data

Internal helper: Apply BLQ censoring rules to data

## Usage

``` r
df_prep_blq(data, loq, loq_method, pred_vars = NULL)
```

## Arguments

- data:

  data.frame containing data to censor

- loq:

  Numeric value of the lower limit of quantification (LLOQ) for the
  assay. Must be coercible to a numeric if specified. Can be `NULL` if
  variable `LLOQ` is present in `data` Specifying this argument implies
  that `DV` is missing in `data` where \< LLOQ.

- loq_method:

  Method for handling data below the lower limit of quantification (BLQ)
  in the plot.

  Options are:

      + No handling: `0` or `"none"`, Plot input dataset `DV` vs `TIME` as is. (default)
      + Impute Post-dose: `1` or `"postdose"`, Impute all BLQ data at `TIME` <= 0 to 0 and all BLQ data at `TIME` > 0 to 1/2 x `loq`.
         Useful for plotting concentration-time data with some data BLQ on the linear scale
      + Impute All: `2` or `"all"`,Impute all BLQ data to 1/2 x `loq`.
         Useful for plotting concentration-time data with some data BLQ on the log scale where 0 cannot be displayed

- pred_vars:

  Character vector of prediction variable names (e.g., `"PRED"`,
  `"IPRED"`) to apply threshold-based BLQ imputation. Default is `NULL`.

## Value

A data.frame with BLQ censoring applied

## Examples

``` r
data <- dplyr::rename(dplyr::filter(data_sad, CMT %in% c(1,2)), DV = ODV)
data_loq1 <- pmxhelpr:::df_prep_blq(data, loq = 10, loq_method = 1)
data_loq2 <- pmxhelpr:::df_prep_blq(data, loq = 10, loq_method = 2)
```
