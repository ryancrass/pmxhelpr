# Internal helper: Merge user overrides into a complete default named list

Iterates over names in the user-supplied list and overwrites matching
entries in the default. Warns on unrecognized names. Used to merge the
layer visibility lists from
[`plot_gof_shown()`](https://ryancrass.github.io/pmxhelpr/reference/plot_gof_shown.md)
/
[`plot_vpc_shown()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_shown.md)
over their defaults.

## Usage

``` r
merge_element(user, default)
```

## Arguments

- user:

  User-supplied list with partial overrides, or `NULL`.

- default:

  Complete default list.

## Value

A merged list with the same class as `default`

## Examples

``` r
pmxhelpr:::merge_element(list(obs = FALSE), plot_gof_shown())
#> $obs
#> [1] FALSE
#> 
#> $dv
#> [1] TRUE
#> 
#> $pred
#> [1] TRUE
#> 
#> $ipred
#> [1] TRUE
#> 
```
