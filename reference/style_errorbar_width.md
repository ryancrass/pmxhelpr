# Internal helper: default the error bar cap width from the data

Shared by
[`plot_dvtime()`](https://ryancrass.github.io/pmxhelpr/reference/plot_dvtime.md)
and
[`plot_gof()`](https://ryancrass.github.io/pmxhelpr/reference/plot_gof.md).
The cap width is the `errorbar_width` field of
[`ggstylekit::style_spec()`](https://rdrr.io/pkg/ggstylekit/man/style_spec.html),
set through `style_dvtime(errorbar_width = ...)` /
`style_gof(errorbar_width = ...)`. The presets leave it unset because
the useful default is a data-scale quantity: when `style$errorbar_width`
is `NULL`, this back-fills 2.5% of the maximum `NTIME` in `data`. A
width set in the style always wins. When `NTIME` is absent or all `NA`
the field stays unset and ggplot2's default width applies (`NA` is not a
valid `errorbar_width`).

## Usage

``` r
style_errorbar_width(style, data)
```

## Arguments

- style:

  A `ggstylekit_style_spec`.

- data:

  The plot data, checked for an `NTIME` column.

## Value

`style`, with `errorbar_width` filled when it was unset.
