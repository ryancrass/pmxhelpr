# Add population predictions (`PRED`) to a data.frame

`df_mrgsim_addpred()` is a wrapper function for
[`mrgsolve::mrgsim_df()`](https://mrgsolve.org/docs/reference/mrgsim.html)
and
[`mrgsolve::zero_re()`](https://mrgsolve.org/docs/reference/zero_re.html)
that returns a data.frame with the addition of a new variable (`PRED`).

## Usage

``` r
df_mrgsim_addpred(data, model, ipred_output = "IPRED", ..., output_var)
```

## Arguments

- data:

  Input dataset.

- model:

  `mrgsolve` model object.

- ipred_output:

  Name of the individual prediction output captured by `model`. It is
  simulated with random effects zeroed
  ([`mrgsolve::zero_re()`](https://mrgsolve.org/docs/reference/zero_re.html))
  and returned as `PRED`. Accepts bare names or strings. Default is
  `IPRED`.

- ...:

  Additional arguments passed to
  [`mrgsolve::mrgsim_df()`](https://mrgsolve.org/docs/reference/mrgsim.html).
  A `carry_out` is merged with `ipred_output`. Arguments that change the
  number of output rows (e.g. `obsonly`) are an error, since `PRED` is
  matched to `data` by row.

- output_var:

  **\[deprecated\]** Use `ipred_output`.

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
#> Loading model from cache.
data <- df_mrgsim_addpred(data = dplyr::filter(data_sad, CMT != 3), model = pkmodel)
```
