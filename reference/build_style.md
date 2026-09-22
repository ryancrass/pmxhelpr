# Internal helper: build a style_spec from preset defaults plus user overrides

Each `style_*()` preset supplies a named list of default `style_spec`
fields. Per-series map fields (`colors`, `fill`, `linetypes`, `alphas`,
`shapes`, `sizes`, `linewidths`) are merged entry-wise onto the defaults
so that a partial override (e.g. `colors = c(obs_point = "red")`) keeps
the other roles' default values; all other fields replace their default
wholesale. A per-series map may also be given as a palette `function(n)`
(e.g. `function(n) grDevices::hcl.colors(n, "Viridis")`), which
`ggstylekit` calls with the number of mapped groups; a palette has no
entries to merge, so it replaces the default map wholesale. The merged
fields are passed to
[`ggstylekit::style_spec()`](https://rdrr.io/pkg/ggstylekit/man/style_spec.html).

## Usage

``` r
build_style(defaults, overrides = list())
```

## Arguments

- defaults:

  Named list of default `style_spec` arguments.

- overrides:

  Named list of user overrides (typically `list(...)`).

## Value

A `ggstylekit_style_spec` object.
