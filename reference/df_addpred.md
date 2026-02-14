# Add population predictions (`PRED`) to a data.frame

`df_addpred()` is a wrapper function for
[`mrgsolve::mrgsim_df()`](https://mrgsolve.org/docs/reference/mrgsim.html)
and
[`mrgsolve::zero_re()`](https://mrgsolve.org/docs/reference/zero_re.html)
that returns a data.frame with the addition of a new variable (`PRED`).

## Usage

``` r
df_addpred(data, model, output_var = "IPRED", ...)
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
  Default is `"IPRED"`.

- ...:

  Additional arguments passed to
  [`mrgsolve::mrgsim_df()`](https://mrgsolve.org/docs/reference/mrgsim.html).

## Value

A data.frame with the same number of rows as `data` and on additional
numeric variable `PRED`.

## Examples

``` r
model <- model_mread_load(model = "model")
#> Building model_cpp ... 
#> done.
data <- df_addpred(data = data_sad, model = model)
```
