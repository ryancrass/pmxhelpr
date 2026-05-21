# Combine two pmx theme elements

Overlay one `pmx_element` onto another of the same subclass. Each set
field in `b` overrides the matching field in `a`; fields unique to `a`
are unchanged. Useful for layering partial overrides on a base element:
`pmx_point(size = 2) + pmx_point(color = "red")` returns
`pmx_point(size = 2, color = "red")`.

Cross-subclass combinations (e.g. `pmx_style + pmx_point`) are
intentionally disallowed — `pmx_style` is a theme-level shortcut applied
at theme construction time, not a sibling element type.

## Usage

``` r
# S3 method for class 'pmx_element'
a + b
```

## Arguments

- a:

  A `pmx_element` object (left side, the "base").

- b:

  A `pmx_element` object of the same subclass as `a` (right side, the
  "override"), or `NULL`. When `NULL`, `a` is returned unchanged.

## Value

A `pmx_element` with the same class as `a`.

## Examples

``` r
pmx_point(size = 2) + pmx_point(color = "red")
#> <pmx_point>
#>   size = 2, color = red
pmx_line(linewidth = 1) + NULL
#> <pmx_line>
#>   linewidth = 1
```
