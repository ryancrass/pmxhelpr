# Summary method for `pmx_stats`

Generic fallback summary for `pmx_stats` containers, reached only for
subclasses without a method of their own. It prints the same text as
[`print.pmx_stats()`](https://ryancrass.github.io/pmxhelpr/reference/print.pmx_stats.md).

## Usage

``` r
# S3 method for class 'pmx_stats'
summary(object, ...)

# S3 method for class 'summary.pmx_stats'
print(x, ...)
```

## Arguments

- object:

  A `pmx_stats` object.

- ...:

  Currently unused.

- x:

  A `summary.pmx_stats` object.

## Value

A `summary.pmx_stats` object: a list with `header` (the class tag, `dim`
of `stats`, `obs_rows`, and the full `config`) and `classes` (the
container's class vector).
