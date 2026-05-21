# Print method for `pmx_stats`

Generic fallback print for `pmx_stats` containers. Subclasses
(`vpc_stats`, `doseprop_stats`) provide their own richer print methods;
this one is reached only by direct construction of the base class.

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
