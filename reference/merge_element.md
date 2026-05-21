# Internal helper: Merge user element overrides into a complete default element

Iterates over names in the user-supplied element and overwrites matching
fields in the default. Warns on unrecognized field names.

## Usage

``` r
merge_element(user, default)
```

## Arguments

- user:

  User-supplied element with partial overrides, or `NULL`.

- default:

  Complete default element.

## Value

A merged element with the same class as `default`

## Examples

``` r
defaults <- pmx_point(shape = 1, size = 0.75, alpha = 0.5)
pmxhelpr:::merge_element(pmx_point(size = 2), defaults)
#> <pmx_point>
#>   shape = 1, size = 2, alpha = 0.5
```
