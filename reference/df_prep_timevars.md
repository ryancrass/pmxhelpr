# Internal helper: Standardize time variable names in a data.frame

Renames columns in `data` to the standardized `TIME` and `NTIME` names.
When both time variables map to the same source column, the column is
renamed to `NTIME` and `TIME` is created as a copy (or vice versa if the
source is already named `TIME`).

## Usage

``` r
df_prep_timevars(data, time_var_str, ntime_var_str)
```

## Arguments

- data:

  Input data.frame.

- time_var_str:

  String name of the actual time column in `data`.

- ntime_var_str:

  String name of the nominal time column in `data`.

## Value

A data.frame with standardized `TIME` and `NTIME` columns

## Examples

``` r
data <- dplyr::rename(data_sad, NTFD = NTIME)
c("TIME", "NTIME") %in% colnames(data)
#> [1]  TRUE FALSE
data_std <- pmxhelpr:::df_prep_timevars(data, "TIME", "NTFD")
c("TIME", "NTIME") %in% colnames(data_std)
#> [1] TRUE TRUE
```
