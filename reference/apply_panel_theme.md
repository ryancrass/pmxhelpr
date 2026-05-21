# Internal helper: Apply unified panel theme to a ggplot

All plot families blank `panel.grid.minor` and `panel.grid.major.x`. The
VPC family additionally uses a white panel rather than
[`theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)'s
grey (toggled via `white_panel`). This helper centralizes the panel
theme application so each family picks its variant via parameters rather
than re-stating an inline
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) block.

## Usage

``` r
apply_panel_theme(plot, white_panel = FALSE)
```

## Arguments

- plot:

  A ggplot2 object.

- white_panel:

  Logical. When `TRUE`, sets `panel.background` to a white rectangle
  with a thin black border. Default is `FALSE`. Used by the VPC family.

## Value

A ggplot2 object with the unified panel theme applied.
