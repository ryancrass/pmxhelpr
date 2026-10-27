# Validate a `pmx_stats` container

Internal helper. Asserts that `x` carries the `pmx_stats` class and
exposes the `stats` / `obs` / `config` slots in the expected shape.
Subclass validators pass their contract (class, required columns,
required config keys) through the optional arguments, so every container
reports missing pieces with the same messages, and then apply any checks
of their own.

## Usage

``` r
validate_pmx_stats(
  x,
  subclass = NULL,
  source = NULL,
  stats_cols = NULL,
  obs_cols = NULL,
  config_keys = NULL,
  require_obs = length(obs_cols) > 0
)
```

## Arguments

- x:

  Object to validate.

- subclass:

  Optional subclass name `x` must carry (e.g. `"forest_stats"`).

- source:

  Name of the function that builds `subclass`, quoted in the class error
  (e.g. `"df_forest"`).

- stats_cols:

  Columns `x$stats` must contain.

- obs_cols:

  Columns `x$obs` must contain.

- config_keys:

  Keys `x$config` must contain.

- require_obs:

  Logical; whether `x$obs` must be a data.frame rather than `NULL`.
  Implied by a non-empty `obs_cols`.

## Value

`invisible(x)` on success.
