# Perform a log-log Regression

Perform a log-log Regression

## Usage

``` r
mod_loglog(data, exp_var = "PPORRES", dose_var = "DOSE")
```

## Arguments

- data:

  Input dataset for log-log regression. Default expected format is
  output from
  [`PKNCA::pk.nca()`](http://humanpred.github.io/pknca/reference/pk.nca.md)
  (i.e., SDTM PP formatting)

- exp_var:

  Character string specifying the variable in `data` containing the
  exposure metric (dependent variable) Default is "PPORRES".

- dose_var:

  Character string specifying the variable in `data` containing the dose
  (independent variable) Default is "DOSE".

## Value

`lm` object

## Examples

``` r
mod_auc <- mod_loglog(dplyr::filter(data_sad_nca, PPTESTCD == "aucinf.obs"))
summary(mod_auc)
#> 
#> Call:
#> stats::lm(formula = form, data = data)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -0.80124 -0.29492 -0.03507  0.14386  1.24984 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  4.04173    0.30499   13.25  5.5e-15 ***
#> log(DOSE)    0.99657    0.06629   15.03  < 2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.4606 on 34 degrees of freedom
#> Multiple R-squared:  0.8692, Adjusted R-squared:  0.8654 
#> F-statistic:   226 on 1 and 34 DF,  p-value: < 2.2e-16
#> 

mod_cmax <- mod_loglog(dplyr::filter(data_sad_nca, PPTESTCD == "cmax"))
summary(mod_cmax)
#> 
#> Call:
#> stats::lm(formula = form, data = data)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -0.95350 -0.31769  0.00194  0.26984  0.98285 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)   1.0917     0.2834   3.852 0.000494 ***
#> log(DOSE)     1.0680     0.0616  17.337  < 2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.428 on 34 degrees of freedom
#> Multiple R-squared:  0.8984, Adjusted R-squared:  0.8954 
#> F-statistic: 300.6 on 1 and 34 DF,  p-value: < 2.2e-16
#> 
```
