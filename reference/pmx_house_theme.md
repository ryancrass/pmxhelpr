# Internal helper: pmxhelpr house ggplot2 theme

The shared base theme applied to every pmxhelpr plot via each preset's
`style_spec(theme = ...)` field:
[`theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
with the minor and vertical major gridlines blanked. The VPC family
passes `white_panel = TRUE` for a white panel background with a thin
black border.

## Usage

``` r
pmx_house_theme(white_panel = FALSE, forest_panel = FALSE)
```

## Arguments

- white_panel:

  Logical. When `TRUE`, sets `panel.background` to a white rectangle
  with a thin black border (VPC family). Default `FALSE`.

- forest_panel:

  Logical. When `TRUE`, applies the forest-family panel variant
  described above; `white_panel` is then ignored. Default `FALSE`.

## Value

A ggplot2 theme object.

## Details

The forest family (`forest_panel = TRUE`) inverts the gridline
orientation: it keeps `panel.grid.major.x` as a faint ruler aligned with
the point estimates and blanks `panel.grid.major.y` (the
[`facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html)
rows group the y-axis by covariate name instead). Panels are transparent
with a thin grey border and no vertical spacing, so the row facets stack
into one frame with their shared borders acting as separator lines. The
covariate row strips are styled for `switch = "y"`: placed outside the
y-axis, no background fill, bold horizontal text aligned to the top-left
so each covariate name heads its group of rows. The y-axis text is
black. The right-hand secondary y-axis carries the estimate column
(left-aligned text at the axis text size, an 8 pt gap from the panel, no
ticks) and the plot subtitle is its bold header, right-aligned across
the full plot width (`plot.title.position = "plot"`) so it sits on the
top edge of the panels above the column.
