# Compute estimate table for log-log regression

Compute estimate table for log-log regression

## Usage

``` r
df_loglog(fit, method = "normal", ci = 0.9, sigdigits = 3)
```

## Arguments

- fit:

  `lm` model object for the log-log regression

- method:

  character string specifying the distribution to be used to derived the
  confidence interval. Options are "normal" (default) and "tdist"

- ci:

  confidence interval to be calculated. Options 0.90 (default) and 0.95

- sigdigits:

  number of significant digits for rounding

## Value

`data.frame`

## Examples

``` r
mod_auc <- mod_loglog(dplyr::filter(data_sad_nca, PPTESTCD == "aucinf.obs"))
df_loglog(mod_auc)
#>   Intercept StandardError  CI Power   LCL  UCL Proportional
#> 1      4.04        0.0663 90% 0.997 0.888 1.11         TRUE
#>                            PowerCI    Interpretation
#> 1 Power: 0.997 (90% CI 0.888-1.11) Dose-proportional

mod_cmax <- mod_loglog(dplyr::filter(data_sad_nca, PPTESTCD == "cmax"))
df_loglog(mod_cmax)
#>   Intercept StandardError  CI Power   LCL  UCL Proportional
#> 1      1.09        0.0616 90%  1.07 0.967 1.17         TRUE
#>                           PowerCI    Interpretation
#> 1 Power: 1.07 (90% CI 0.967-1.17) Dose-proportional
```
