# Internal helper: add a horizontal reference line (style pattern)

Internal helper: add a horizontal reference line (style pattern)

## Usage

``` r
add_ref_layer_style(plot, ref)
```

## Arguments

- plot:

  ggplot object to modify.

- ref:

  Numeric y-intercept, or `NULL` for no line.

## Value

The (possibly modified) ggplot object, with the reference line tagged as
the `"ref_line"` series for
[`ggstylekit::style_plot()`](https://rdrr.io/pkg/ggstylekit/man/style_plot.html).
