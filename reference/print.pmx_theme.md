# Print method for pmx plot themes

Compact REPL display for any object inheriting `"pmx_theme"` – the
values returned by
[`pmx_theme()`](https://ryancrass.github.io/pmxhelpr/reference/pmx_theme.md)
and the per-plot theme factories. Shows the theme type as a banner and
one line per theme key, listing the inner element type and its set
fields inline.

## Usage

``` r
# S3 method for class 'pmx_theme'
print(x, ...)
```

## Arguments

- x:

  A pmx theme object.

- ...:

  Currently unused.

## Value

`invisible(x)`.
