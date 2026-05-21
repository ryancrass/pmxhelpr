# Internal helper: Add horizontal reference line

Conditionally adds a horizontal reference line at the specified
y-intercept. Draws the line when `ref` is non-NULL.

## Usage

``` r
add_ref_layers(plot, ref, ref_el)
```

## Arguments

- plot:

  ggplot object to modify

- ref:

  Numeric y-intercept for the reference line, or `NULL` for no line.

- ref_el:

  A `pmx_line` element with reference line aesthetics.

## Value

The (possibly modified) ggplot object
