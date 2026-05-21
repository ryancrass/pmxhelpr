# Internal Helper: Encode below-the-limit-of-quantification (BLQ) values as -Inf

Vectorized helper used in VPC pre-processing. Replaces BLQ-flagged
positions of `x` with `-Inf`. A position is BLQ when any of the
following hold: `is.na(x)`, `!is.null(loq) && x < loq`, or
`!is.null(mdv) && mdv == 1`. Returns the modified vector — quantile
computation is performed downstream.

## Usage

``` r
var_loqcens(x, loq = NULL, mdv = NULL)
```

## Arguments

- x:

  Numeric vector to encode.

- loq:

  Numeric scalar or vector of length `length(x)` giving the lower limit
  of quantification, or `NULL` (no `< loq` trigger).

- mdv:

  Integer/logical vector of length `length(x)` flagging missing-DV rows
  (NONMEM convention `MDV == 1`), or `NULL` (no MDV trigger).

## Value

Numeric vector the same length as `x`, with BLQ positions set to `-Inf`.

## Examples

``` r
pmxhelpr:::var_loqcens(c(1, 2, 5, NA, 10), loq = 3, mdv = c(0, 0, 0, 0, 1))
#> [1] -Inf -Inf    5 -Inf -Inf
pmxhelpr:::var_loqcens(c(1, 2, 5), loq = NULL, mdv = c(0, 1, 0))
#> [1]    1 -Inf    5
```
