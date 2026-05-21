# Internal helper: Merge user theme overrides into a complete default theme

Iterates over groups in the user-supplied theme and merges each group
element-by-element into the default theme using
[`merge_element()`](https://ryancrass.github.io/pmxhelpr/reference/merge_element.md).
When a user-supplied entry is a
[`pmx_style()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_style.md)
object and the default theme contains matching `_point` and `_line`
sub-keys, the style fields are applied to both sub-elements.

## Usage

``` r
merge_theme(user, default)
```

## Arguments

- user:

  User-supplied theme with partial group overrides, or `NULL`.

- default:

  Complete default theme.

## Value

A merged theme list

## Examples

``` r
defaults <- plot_dvtime_theme()
pmxhelpr:::merge_theme(list(obs_point = pmx_point(size = 2)), defaults)
#> <plot_dvtime_theme>
#>   obs_point     <pmx_point>: shape = 1, size = 2, alpha = 0.5
#>   obs_line      <pmx_line>: linewidth = 0.5, linetype = 1, alpha = 0.5
#>   cent_point    <pmx_point>: shape = 16, size = 1.25, alpha = 0
#>   cent_line     <pmx_line>: linewidth = 0.75, linetype = 1, alpha = 1
#>   cent_errorbar <pmx_errorbar>: linewidth = 0.75, linetype = 1, alpha = 1, width = NULL
#>   ref_line      <pmx_line>: linewidth = 0.5, linetype = 2, alpha = 1
#>   loq_line      <pmx_line>: linewidth = 0.5, linetype = 2, alpha = 1
```
