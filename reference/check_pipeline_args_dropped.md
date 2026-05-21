# Internal helper: abort if pipeline arguments are passed on the precomputed-stats path.

Plot wrappers like
[`plot_vpc_cont()`](https://ryancrass.github.io/pmxhelpr/reference/plot_vpc_cont.md)
and
[`plot_doseprop()`](https://ryancrass.github.io/pmxhelpr/reference/plot_doseprop.md)
accept either raw data or a precomputed `pmx_stats` container. On the
cached path the pipeline never re-runs, so any pipeline argument the
user supplied would be silently ignored. This helper inspects the
caller's [`match.call()`](https://rdrr.io/r/base/match.call.html),
subtracts the explicitly-allowed plot-only arguments, and aborts when
the remainder is non-empty — making the silent-shadow case audible.

## Usage

``` r
check_pipeline_args_dropped(call, plot_only_args, fn_name)
```

## Arguments

- call:

  A `call` object, typically
  [`match.call()`](https://rdrr.io/r/base/match.call.html) evaluated in
  the wrapper's frame.

- plot_only_args:

  Character vector of argument names that ARE honored on the
  precomputed-stats path (e.g. `"theme"`, `"shown"`, `"pcvpc"`). The
  matched data argument should also be included.

- fn_name:

  Character scalar, the wrapper's name, used in the error message.

## Value

`invisible(NULL)` on success.
