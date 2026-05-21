# Internal helper: Build a ggplot2 layer with conditional color

Wraps [`do.call()`](https://rdrr.io/r/base/do.call.html) and includes
`color` in the argument list only when color is not mapped via aes and a
non-NULL value is provided.

## Usage

``` r
build_layer(layer_fn, args, color = NULL, color_mapped = FALSE)
```

## Arguments

- layer_fn:

  The geom/stat function (e.g.,
  [`ggplot2::geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html))

- args:

  Named list of arguments to pass

- color:

  Fixed color value, or `NULL`

- color_mapped:

  Logical: is color already mapped via aes?

## Value

A ggplot2 layer object
