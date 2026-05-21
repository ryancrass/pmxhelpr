# Add a layer to a `pmx_vpc_plot` with a facet warning

S3 method for `+` applied to objects returned by
[`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md)
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
[`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md)
to stratify both statistics and panels.

The `pmx_vpc_plot` class is preserved on the result so that subsequent
`+` calls are also intercepted.

## Usage

``` r
# S3 method for class 'pmx_vpc_plot'
e1 + e2
```

## Arguments

- e1:

  A `pmx_vpc_plot` object.

- e2:

  A ggplot2 layer, scale, theme, facet, or other addable object.

## Value

A `pmx_vpc_plot` object.
