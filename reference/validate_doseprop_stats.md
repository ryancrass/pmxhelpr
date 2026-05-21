# Validate a `doseprop_stats` object

Internal helper. Asserts that `x` carries the `doseprop_stats` class,
contains the per-metric stats columns, and carries the attributes
downstream consumers (notably
[`plot_build_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_doseprop.md))
depend on. Aborts with a clear message on failure; returns `x` invisibly
on success.

## Usage

``` r
validate_doseprop_stats(x)
```

## Arguments

- x:

  Object to validate.

## Value

`invisible(x)` on success.
