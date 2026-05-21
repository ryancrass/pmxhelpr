# Summary method for `pmx_stats`

Generic fallback summary for `pmx_stats` containers; delegates to
[`print.pmx_stats()`](https://ryancrass.github.io/pmxhelpr/reference/print.pmx_stats.md).
Subclasses provide their own summary methods.

## Usage

``` r
# S3 method for class 'pmx_stats'
summary(object, ...)
```

## Arguments

- object:

  A `pmx_stats` object.

- ...:

  Currently unused.

## Value

`invisible(object)`.
