# Validate a `pmx_stats` container

Checks that `x` is a well-formed `pmx_stats` container and returns it
invisibly, or aborts with an error of class `pmxhelpr_error_bad_stats`
naming what is wrong. Use it after editing a container (e.g. `x$stats`)
or building one by hand with
[`pmx_stats()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_stats.md);
[`is_pmx_stats()`](https://ryancrass.github.io/pmxhelpr/reference/is_pmx_stats.md)
with `strict = TRUE` runs the same checks but returns `TRUE`/`FALSE`.

The generic dispatches on the container class:

- `vpc_stats`: the bin, count and quantile columns of `stats`; the
  `OBSDV`/`PC_OBSDV`/`TIME` columns of `obs`; the `n_replicates`, `ci`,
  `pi`, `loq` and `strat_var` config keys; and the `sim_prop_blq_*`
  columns when `config$loq` is set.

- `doseprop_stats`: the power-model columns of `stats`, an `obs` frame
  holding the `config` metric and dose columns, and the
  `metric_name_var`, `metric_value_var`, `dose_var` and `ci` config
  keys.

- `forest_stats`: the estimate, label, `is_ref` and `layout` columns of
  `stats` with their types and values, the `config` grouping columns,
  and valid `config$ci` and `config$sigdigits`.

- Any other object: `x` must carry the `pmx_stats` class and have a
  `stats` data.frame, an `obs` data.frame or `NULL`, and a `config`
  list. Packages defining their own `pmx_stats` subclass can add a
  method.

## Usage

``` r
# S3 method for class 'doseprop_stats'
validate_pmx_stats(
  x,
  ...,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
)

# S3 method for class 'forest_stats'
validate_pmx_stats(
  x,
  ...,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
)

validate_pmx_stats(
  x,
  ...,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
)

# Default S3 method
validate_pmx_stats(
  x,
  ...,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
)

# S3 method for class 'vpc_stats'
validate_pmx_stats(
  x,
  ...,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
)
```

## Arguments

- x:

  Object to validate.

- ...:

  Passed to methods; unused by the methods in pmxhelpr.

- arg, call:

  The argument name and call reported in errors.

## Value

`invisible(x)` on success.

## See also

Other pmx stats class:
[`is_pmx_stats()`](https://ryancrass.github.io/pmxhelpr/reference/is_pmx_stats.md),
[`pmx_stats()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_stats.md)

## Examples

``` r
fs <- df_forest(data_sad_pkforest)
validate_pmx_stats(fs)
fs$stats$est <- as.character(fs$stats$est)
try(validate_pmx_stats(fs))
#> Error in eval(expr, envir) : 
#>   `fs$stats` column `est` must be numeric.
```
