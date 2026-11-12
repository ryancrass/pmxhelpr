# Print method for `pmx_stats`

Generic fallback print for `pmx_stats` containers. Subclasses
(`vpc_stats`, `doseprop_stats`, `forest_stats`) provide their own richer
print methods; this one is reached only for other subclasses built with
[`pmx_stats()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_stats.md).

## Usage

``` r
# S3 method for class 'pmx_stats'
print(x, ...)
```

## Arguments

- x:

  A `pmx_stats` object.

- ...:

  Currently unused.

## Value

`invisible(x)`.
