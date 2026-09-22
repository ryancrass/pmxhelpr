# Internal helper: pmxhelpr house ggplot2 theme

The shared base theme applied to every pmxhelpr plot via each preset's
`style_spec(theme = ...)` field:
[`theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
with the minor and vertical major gridlines blanked. The VPC family
passes `white_panel = TRUE` for a white panel background with a thin
black border.

## Usage

``` r
pmx_house_theme(white_panel = FALSE)
```

## Arguments

- white_panel:

  Logical. When `TRUE`, sets `panel.background` to a white rectangle
  with a thin black border (VPC family). Default `FALSE`.

## Value

A ggplot2 theme object.
