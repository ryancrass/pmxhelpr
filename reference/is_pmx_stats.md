# Test whether an object is a `pmx_stats` container

Predicate that returns `TRUE` if `x` carries the `"pmx_stats"` class.
This is the shared base class for
[`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)
and
[`df_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/df_doseprop.md)
outputs; use the subclass-specific predicates
[`is_vpc_stats()`](https://ryancrass.github.io/pmxhelpr/reference/is_vpc_stats.md)
and
[`is_doseprop_stats()`](https://ryancrass.github.io/pmxhelpr/reference/is_doseprop_stats.md)
when you need to discriminate between pipelines.

## Usage

``` r
is_pmx_stats(x, strict = FALSE)
```

## Arguments

- x:

  Object to test.

- strict:

  Logical. When `TRUE`, additionally runs
  [`validate_pmx_stats()`](https://ryancrass.github.io/pmxhelpr/reference/validate_pmx_stats.md)
  and returns `FALSE` on validation failure. Default `FALSE` (class-tag
  check only, cheap).

## Value

Logical scalar.

## See also

Other pmx stats class:
[`pmx_stats()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_stats.md)
