# Internal helper: Expand a pmx_style into matching point and line sub-keys

Internal helper: Expand a pmx_style into matching point and line
sub-keys

## Usage

``` r
apply_style(style, prefix, defaults)
```

## Arguments

- style:

  A `pmx_style` object.

- prefix:

  Character role prefix (e.g., `"obs"`, `"cent"`).

- defaults:

  Named list of theme defaults.

## Value

The modified defaults list with style fields applied.
