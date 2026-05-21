# Internal Helper: Replace -Inf with NA_real\_

Vectorized helper that converts `-Inf` entries (and only `-Inf`, not
`+Inf`) to `NA_real_`. Used to clean BLQ-encoded values in VPC summary
statistics and observation frames before plotting.

## Usage

``` r
var_infna(x)
```

## Arguments

- x:

  Numeric vector.

## Value

Numeric vector the same length as `x`, with `-Inf` replaced by
`NA_real_`.

## Examples

``` r
pmxhelpr:::var_infna(c(1, -Inf, 3, NA))
#> [1]  1 NA  3 NA
```
