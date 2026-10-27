# Internal helper: the rows a forest plot draws, in draw order

Returns `stats$stats` (optionally restricted to one metric) filtered to
the requested reference layout through its `layout` column: rows marked
`"both"` always draw, `"consolidated"` rows (the reference panel) only
in that layout, `"distributed"` rows (the reference copies in each
covariate panel) only in that one.

## Usage

``` r
forest_rows(stats, ref_layout = NULL, metric = NULL)
```

## Arguments

- stats:

  A `forest_stats` object.

- ref_layout:

  `NULL`, `"distributed"`, or `"consolidated"`. `NULL` resolves to
  `"distributed"` when the rows of `metric` (all rows if `metric` is
  `NULL`) include distributed rows, otherwise `"consolidated"`.

- metric:

  Optional metric value to restrict the rows to.

## Value

A data.frame of rows in top-to-bottom draw order.
