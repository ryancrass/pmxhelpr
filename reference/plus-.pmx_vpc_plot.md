# Add a layer to a `pmx_vpc_plot` with a facet warning

S3 method for `+` applied to objects returned by
[`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md),
[`plot_vpc_cens()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cens.md),
and
[`plot_build_vpc()`](https://ryancrass.github.io/pmxhelpr/reference/plot_build_vpc.md).
Delegates to the standard ggplot2 `+` operation for all layer types.
When `e2` is a
[`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
or
[`ggplot2::facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html)
specification, a warning is issued because the VPC summary statistics
are pre-computed and will not reflect the added stratification. Use
`strat_var` in
[`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md),
[`plot_vpc_cens()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cens.md),
or
[`df_vpcstats()`](https://ryancrass.github.io/pmxhelpr/reference/df_vpcstats.md)
to stratify both statistics and panels.

Facets inside a [`list()`](https://rdrr.io/r/base/list.html) are caught
the same way.
[`restyle_plot()`](https://rdrr.io/pkg/ggstylekit/man/restyle_plot.html)
and [`reveal()`](https://rdrr.io/pkg/ggstylekit/man/reveal.html) keep
the `pmx_vpc_plot` class, and `reveal(as = "facet")` and
`restyle_plot(facet = )` warn like `+ facet_*()`.

The `pmx_vpc_plot` class is preserved on the result so that subsequent
`+` calls are also intercepted.

## Usage

``` r
# S3 method for class 'pmx_vpc_plot'
e1 + e2

# S3 method for class 'pmx_vpc_plot'
restyle_plot(p, ...)

# S3 method for class 'pmx_vpc_plot'
reveal(p, col, ..., as = NULL, on = NULL)
```

## Arguments

- e1:

  A `pmx_vpc_plot` object.

- e2:

  A ggplot2 layer, scale, theme, facet, or other addable object.

- p:

  A `pmx_vpc_plot` object.

- col, as, on, ...:

  Passed to the ggstylekit
  [`reveal()`](https://rdrr.io/pkg/ggstylekit/man/reveal.html) or
  [`restyle_plot()`](https://rdrr.io/pkg/ggstylekit/man/restyle_plot.html)
  method.

## Value

A `pmx_vpc_plot` object.
