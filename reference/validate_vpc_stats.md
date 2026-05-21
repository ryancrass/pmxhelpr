# Validate a `vpc_stats` object

Internal helper. Asserts that `x` carries the `vpc_stats` class,
contains the `stats` and `obs` data.frames, and that each carries the
column groups downstream consumers (notably
[`plot_build_vpc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_vpc.md))
depend on. Aborts with a clear message on failure; returns `x` invisibly
on success.

## Usage

``` r
validate_vpc_stats(x)
```

## Arguments

- x:

  Object to validate.

## Value

`invisible(x)` on success.
