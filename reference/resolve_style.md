# Internal helper: resolve and validate the `style` argument

Every plot builder accepts `style = NULL` (use the family preset) or a
[`ggstylekit::style_spec()`](https://rdrr.io/pkg/ggstylekit/man/style_spec.html)
object. This helper applies the default and aborts early, with a
pmxhelpr-branded message, when `style` is anything else; without it, a
bad `style` fails deep inside `ggstylekit` or, for
[`plot_vpc_legend()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_legend.md),
with an uninformative R error.

## Usage

``` r
resolve_style(style, default)
```

## Arguments

- style:

  `NULL` or a `ggstylekit_style_spec` object.

- default:

  A function returning the family preset (e.g.
  [style_dvtime](https://ryancrass.github.io/pmxhelpr/reference/style_dvtime.md)),
  called only when `style` is `NULL`.

## Value

A `ggstylekit_style_spec` object.
