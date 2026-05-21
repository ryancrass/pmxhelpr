# Internal helper: Remove NULL entries from a list

Internal helper: Remove NULL entries from a list

## Usage

``` r
compact(x)
```

## Arguments

- x:

  A named list potentially containing NULL values.

## Value

A list with all NULL entries removed

## Examples

``` r
pmxhelpr:::compact(list(a = 1, b = NULL, c = 3))
#> $a
#> [1] 1
#> 
#> $c
#> [1] 3
#> 
```
