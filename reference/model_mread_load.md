# Load an mrgsolve model file from the internal model library

`model_mread_load()` returns an mrgsolve model object from the internal
model library in `pmxhelpr`

## Usage

``` r
model_mread_load(model, ...)
```

## Arguments

- model:

  Model file name. Must be a string.

- ...:

  Additional arguments passed to
  [`mrgsolve::mread_cache()`](https://mrgsolve.org/docs/reference/mread.html).

## Value

An `mrgsolve` model object.

## See also

Other mrgsolve wrappers:
[`df_mrgsim_addpred()`](https://ryancrass.github.io/pmxhelpr/reference/df_mrgsim_addpred.md),
[`df_mrgsim_replicate()`](https://ryancrass.github.io/pmxhelpr/reference/df_mrgsim_replicate.md)

## Examples

``` r
model <- model_mread_load("pkmodel")
#> Loading model from cache.
```
