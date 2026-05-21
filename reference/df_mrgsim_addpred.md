# Add population predictions (`PRED`) to a data.frame

`df_mrgsim_addpred()` is a wrapper function for
[`mrgsolve::mrgsim_df()`](https://mrgsolve.org/docs/reference/mrgsim.html)
and
[`mrgsolve::zero_re()`](https://mrgsolve.org/docs/reference/zero_re.html)
that returns a data.frame with the addition of a new variable (`PRED`).

## Usage

``` r
df_mrgsim_addpred(data, model, output_var = "IPRED", ...)
```

## Arguments

- data:

  Input dataset.

- model:

  `mrgsolve` model object.

- output_var:

  Name of output from `model` to be captured as `PRED` after removing
  random effects with
  [`mrgsolve::zero_re()`](https://mrgsolve.org/docs/reference/zero_re.html).
  Accepts bare names or strings. Default is `IPRED`.

- ...:

  Additional arguments passed to
  [`mrgsolve::mrgsim_df()`](https://mrgsolve.org/docs/reference/mrgsim.html).

## Value

A data.frame with the same number of rows as `data` and on additional
numeric variable `PRED`.

## See also

Other mrgsolve wrappers:
[`df_mrgsim_replicate()`](https://ryancrass.github.io/pmxhelpr/reference/df_mrgsim_replicate.md),
[`model_mread_load()`](https://ryancrass.github.io/pmxhelpr/reference/model_mread_load.md)

## Examples

``` r
pkmodel <- model_mread_load(model = "pkmodel")
#> Building pkmodel_cpp ... 
#> done.
data <- df_mrgsim_addpred(data = dplyr::filter(data_sad, CMT != 3), model = pkmodel)
```
