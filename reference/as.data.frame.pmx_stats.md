# Coerce a `pmx_stats` object to a data.frame

Returns the `stats` slot as a plain `data.frame`. Use `x$obs` separately
for the observation overlay and `x$config` for the run configuration.

## Usage

``` r
# S3 method for class 'pmx_stats'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  A `pmx_stats` object.

- row.names, optional:

  Standard `as.data.frame` arguments (unused).

- ...:

  Currently unused.

## Value

A `data.frame` (the `stats` slot).
