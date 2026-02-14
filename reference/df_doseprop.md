# Compute and tabulate estimates for log-log regression

Compute and tabulate estimates for log-log regression

## Usage

``` r
df_doseprop(
  data,
  metrics,
  metric_var = "PPTESTCD",
  exp_var = "PPORRES",
  dose_var = "DOSE",
  method = "normal",
  ci = 0.95,
  sigdigits = 3
)
```

## Arguments

- data:

  Input dataset for log-log regression. Default expected format is
  output from
  [`PKNCA::pk.nca()`](http://humanpred.github.io/pknca/reference/pk.nca.md)
  (i.e., SDTM PP formatting)

- metrics:

  character vector of exposure metrics in `data` to plot

- metric_var:

  character string of variable in `data` containing the values provided
  in `metrics`. Default is "PPTESTCD".

- exp_var:

  Character string specifying the variable in `data` containing the
  exposure metric (dependent variable) Default is "PPORRES".

- dose_var:

  Character string specifying the variable in `data` containing the dose
  (independent variable) Default is "DOSE".

- method:

  character string specifying the distribution to be used to derived the
  confidence interval. Options are "normal" (default) and "tdist"

- ci:

  confidence interval to be calculated. Options are 0.95 (default) and
  0.90

- sigdigits:

  number of significant digits for rounding

## Value

`data.frame`

## Examples

``` r
df_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"))
#>   Intercept StandardError  CI Power   LCL  UCL Proportional
#> 1      4.04        0.0663 95% 0.997 0.867 1.13         TRUE
#> 2      1.09        0.0616 95% 1.070 0.947 1.19         TRUE
#>                            PowerCI    Interpretation   PPTESTCD
#> 1 Power: 0.997 (95% CI 0.867-1.13) Dose-proportional aucinf.obs
#> 2  Power: 1.07 (95% CI 0.947-1.19) Dose-proportional       cmax
```
