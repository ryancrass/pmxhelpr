# Apply prediction correction

Apply prediction correction

## Usage

``` r
var_predcorr(dv_var, pred_var, lower_bound = 0)
```

## Arguments

- dv_var:

  Vector containing the dependent variable (DV)

- pred_var:

  Vector containing population predictions (PRED)

- lower_bound:

  Lower bound for prediction correction formula.

## Value

A numeric vector of prediction-corrected values of `dv_var`

## Details

The bin-median `predbin` is computed over the full `pred_var` vector
passed in. Callers must group their data by a binning variable (e.g.
nominal time, optionally stratified by compartment or covariates) before
invoking `var_predcorr()` so that `predbin` is the median across the
bin's observations, not the entire dataset.

## See also

Other vectorized helpers:
[`var_addn()`](https://ryancrass.github.io/pmxhelpr/reference/var_addn.md),
[`var_dosenorm()`](https://ryancrass.github.io/pmxhelpr/reference/var_dosenorm.md)

## Examples

``` r
pkmodel <- model_mread_load(model = "pkmodel")
#> Loading model from cache.
data <- df_mrgsim_addpred(
  data = dplyr::filter(data_sad, CMT != 3),
  model = pkmodel)
data <- data |>
  dplyr::filter(EVID == 0) |>
  dplyr::group_by(NTIME) |>
  dplyr::mutate(PCDV = var_predcorr(ODV, PRED)) |>
  dplyr::ungroup()
```
