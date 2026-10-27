# Validate a `forest_stats` object

Internal helper. Asserts that `x` carries the `forest_stats` class,
contains the canonical stats columns, and carries the config keys
downstream consumers (notably
[`plot_build_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_forest.md))
depend on. Aborts with a clear message on failure; returns `x` invisibly
on success.

## Usage

``` r
validate_forest_stats(x)
```

## Arguments

- x:

  Object to validate.

## Value

`invisible(x)` on success.
