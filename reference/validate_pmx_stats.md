# Validate a `pmx_stats` container's structural shape

Internal helper. Asserts that `x` carries the `pmx_stats` class and
exposes the `stats` / `obs` / `config` slots in the expected shape.
Subclass validators delegate to this function for the structural checks
before applying their own column / config-key checks.

## Usage

``` r
validate_pmx_stats(x)
```

## Arguments

- x:

  Object to validate.

## Value

`invisible(x)` on success.
