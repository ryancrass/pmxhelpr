# Internal Helper: Apply dose-normalization to a variable

Internal Helper: Apply dose-normalization to a variable

## Usage

``` r
var_dosenorm(dv_var, dose_var)
```

## Arguments

- dv_var:

  Vector containing the dependent variable (DV)

- dose_var:

  Vector containing dose

## Value

A numeric vector of dose-normalized values of `dv_var`

## See also

Other vectorized helpers:
[`var_addn()`](https://ryancrass.github.io/pmxhelpr/reference/var_addn.md),
[`var_predcorr()`](https://ryancrass.github.io/pmxhelpr/reference/var_predcorr.md)

## Examples

``` r
data <- dplyr::mutate(data_sad, DNDV = var_dosenorm(ODV, DOSE))
```
