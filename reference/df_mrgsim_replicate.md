# Execute a visual predictive check (VPC) simulation using `mrgsolve`

`df_mrgsim_replicate()` is a wrapper function for
[`mrgsolve::mrgsim_df()`](https://mrgsolve.org/docs/reference/mrgsim.html)
that returns a data.frame containing `replicates` iterations of `data`

## Usage

``` r
df_mrgsim_replicate(
  data,
  model,
  replicates,
  dv_var = "DV",
  time_var = "TIME",
  ntime_var = "NTIME",
  pred_var = "PRED",
  ipred_var = "IPRED",
  sim_dv_var = "DV",
  irep_name = "SIM",
  seed = 123456789,
  parallel = FALSE,
  ...
)
```

## Arguments

- data:

  Input dataset. Must contain required variables for
  [`mrgsim_df()`](https://mrgsolve.org/docs/reference/mrgsim.html) other
  than those handled by other arguments.

- model:

  `mrgsolve` model object.

- replicates:

  Number of replicates. Either an integer, or something coercible to an
  integer.

- dv_var:

  Column containing the DV variable in `data`. Accepts bare names or
  strings.

- time_var:

  Column containing the actual time variable. Accepts bare names or
  strings. Default is `TIME`.

- ntime_var:

  Column containing the nominal time variable. Accepts bare names or
  strings. Default is `NTIME`.

- pred_var:

  Name of population prediction output from `model`. Accepts bare names
  or strings. Default is `PRED`.

- ipred_var:

  Name of individual prediction output from `model`. Accepts bare names
  or strings. Default is `IPRED`.

- sim_dv_var:

  Name of simulated DV output from `model`. Accepts bare names or
  strings. Default is `DV`.

- irep_name:

  Name of replicate variable in `data`. Accepts bare names or strings.
  Default is `SIM`.

- seed:

  Random seed. Default is `123456789`.

- parallel:

  Logical. If `TRUE`, replicates run in parallel via
  [`future.apply::future_lapply()`](https://future.apply.futureverse.org/reference/future_lapply.html).
  Requires the `future.apply` package and a parallel plan set by the
  user (e.g., `future::plan(future::multisession, workers = 4)`).
  Default is `FALSE` (sequential).

- ...:

  Additional arguments passed to
  [`mrgsolve::mrgsim_df()`](https://mrgsolve.org/docs/reference/mrgsim.html),
  including `carry_out` and `recover` to control which input columns are
  propagated to the output. The always-carried set (`EVID`, `MDV`,
  `CMT`, `TIME`, `NTIME`, `OBSDV`, and the population prediction column)
  is added to whatever the user passes to `carry_out`.

## Value

A data.frame with `data` x `replicates` rows (unless `obsonly=TRUE` is
passed to
[`mrgsolve::mrgsim_df()`](https://mrgsolve.org/docs/reference/mrgsim.html))
and the output variables `PRED`, `IPRED`, `SIMDV`, `OBSDV`, plus any
input columns listed in `carry_out` / `recover`.

## Details

Under `parallel = TRUE`, per-replicate RNG streams are generated from
`seed` using L'Ecuyer-CMRG (via `future.seed = seed`), so output is
reproducible given the same `seed` and
[`future::plan()`](https://future.futureverse.org/reference/plan.html).
Output under `parallel = TRUE` will differ numerically from
`parallel = FALSE` because the RNG mechanism differs, but is
statistically equivalent.

## See also

Other mrgsolve wrappers:
[`df_mrgsim_addpred()`](https://ryancrass.github.io/pmxhelpr/reference/df_mrgsim_addpred.md),
[`model_mread_load()`](https://ryancrass.github.io/pmxhelpr/reference/model_mread_load.md)

## Examples

``` r
model <- model_mread_load(model = "pkmodel")
#> Loading model from cache.
data_sad_pk <- dplyr::filter(data_sad, CMT %in% c(1,2))
simout <- df_mrgsim_replicate(data = data_sad_pk, model = model, replicates = 100,
dv_var = ODV,
carry_out = c("LLOQ", "WTBL", "FOOD"),
recover = c("USUBJID", "PART"),
irep_name = SIM)

if (FALSE) { # \dontrun{
future::plan(future::multisession, workers = 4)
simout <- df_mrgsim_replicate(data = data_sad_pk, model = model,
                              replicates = 1000, dv_var = ODV,
                              parallel = TRUE)
future::plan(future::sequential)
} # }
```
