# Perform prediction-correction of the dependent variable

`df_pcdv` is a helper function to perform prediction-correction of
observed or simulated depedent variables.

## Usage

``` r
df_pcdv(
  data,
  bin_var = "NTIME",
  strat_vars = NULL,
  dvpred_vars = c(PRED = "PRED", DV = "DV"),
  lower_bound = 0
)
```

## Arguments

- data:

  Input dataset

- bin_var:

  Exact binning variable. Default is `"NTIME"`.

- strat_vars:

  Stratifying variables. Default is `NULL`.

- dvpred_vars:

  Names of variables for the dependent variable and population model
  prediction. Must be named character vector. Defaults are `"PRED"` and
  `"DV"`.

- lower_bound:

  Lower bound of the dependent variable for prediction correction.
  Default is `0`.

## Value

A data.frame containing one row per unique combination of `bin_var` and
`strat_vars` and new variable `PCDV` containing prediction-corrected
observations.

## Examples

``` r
model <- model_mread_load(model = "model")
#> Loading model from cache.
data <- df_addpred(data_sad, model)
simout <- df_pcdv(data, dvpred_vars = c(DV = "ODV", PRED = "PRED"))
```
