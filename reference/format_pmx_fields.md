# Internal helper: format pmx element fields as a comma-separated string

Renders the set fields of a `pmx_element` (or any list-of-named-fields)
as `name = value` pairs, joined with `, `. Used by both
[`print.pmx_element()`](https://ryancrass.github.io/pmxhelpr/reference/print.pmx_element.md)
and
[`print.pmx_theme()`](https://ryancrass.github.io/pmxhelpr/reference/print.pmx_theme.md).

## Usage

``` r
format_pmx_fields(x)
```

## Arguments

- x:

  A list of named fields.

## Value

A character scalar.
