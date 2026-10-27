# Internal helper: format numbers to a fixed count of significant digits

Pads trailing zeros so every value in a rendered label carries the same
number of significant digits (`1.00`, `0.800`, `23300`), unlike
[`signif()`](https://rdrr.io/r/base/Round.html) followed by coercion,
which prints `1`, `0.8`, `23300` and falls back to scientific notation
for small magnitudes. Used to build the forest label columns
([`df_forest()`](https://ryancrass.github.io/pmxhelpr/reference/df_forest.md))
and the dose-proportionality `PowerCI` label
([`df_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/df_doseprop.md)).

## Usage

``` r
fmt_sig(x, sigdigits, anchor = x)
```

## Arguments

- x:

  Numeric vector.

- sigdigits:

  Integer scalar; significant digits to keep.

- anchor:

  Numeric vector recycled to the length of `x`; the value(s) whose
  `sigdigits` significant digits set the decimal places. A zero or
  non-finite anchor falls back to `sigdigits - 1` decimal places.
  Default `x`.

## Value

Character vector the length of `x`; `NA` stays `NA`.

## Details

The decimal count of each value is set by `anchor`: `sigdigits`
significant digits of `anchor` fix the decimal places, and `x` is
rounded to them. The default (`anchor = x`) gives every value its own
`sigdigits` significant digits. A shared anchor gives every value the
same decimal places, so a column of labels aligns on the decimal point
(`2.32`, `0.90` rather than `2.32`, `0.903`).
