# Add LOQ reference line and caption to a plot

Conditionally adds an horizontal reference line at the LLOQ and appends
BLQ imputation method text to the plot caption. Under `dosenorm = TRUE`
the horizontal LLOQ line is suppressed (the line has no meaning on the
dose-normalized scale), but the caption is still appended because the
upstream BLQ imputation in
[`df_prep_blq()`](https://ryancrass.github.io/pmxhelpr/reference/df_prep_blq.md)
runs before dose normalization.

## Usage

``` r
add_blq_layers(
  plot,
  caption,
  loq_method,
  loq,
  dosenorm,
  loq_el,
  show_legend = FALSE
)
```

## Arguments

- plot:

  ggplot object

- caption:

  character, current caption string

- loq_method:

  numeric, 0/1/2

- loq:

  numeric, lower limit of quantification value

- dosenorm:

  logical, whether dose normalization is active

- loq_el:

  A `pmx_line` element with LOQ line aesthetics.

- show_legend:

  logical, whether to add LLOQ to the linetype legend

## Value

A named list with elements `plot` (modified ggplot) and `caption`
(modified string)
