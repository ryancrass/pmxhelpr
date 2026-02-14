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
  time_vars = c(TIME = "TIME", NTIME = "NTIME"),
  output_vars = c(PRED = "PRED", IPRED = "IPRED", DV = "DV"),
  num_vars = NULL,
  char_vars = NULL,
  irep_name = "SIM",
  seed = 123456789,
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

  Character name of the DV variable in `data`.

- time_vars:

  Names of actual and nominal time variables. Must be named character
  vector. Defaults is: c(`TIME`=`"TIME"`, `NTIME`=`"NTIME"`).

- output_vars:

  Names of model outputs from `model`. Must be named character vector.
  Defaults is: c(`PRED`= `"PRED"`, `IPRED` = `"IPRED"`, `DV`= `"DV"`).

- num_vars:

  Numeric variables in `data` or simulation output to recover. Must be a
  character vector of variable names from the simulation output to
  `carry_out` and return in output. Defaults are `"CMT"`, `"EVID"`,
  `"MDV"`, `"NTIME"`.

- char_vars:

  Character variables in `data` or simulation output to recover. Must be
  a character vector of variable names from the simulation output to
  `recover` and return in output.

- irep_name:

  Name of replicate variable in `data`. Must be a string. Default is
  `"SIM"`.

- seed:

  Random seed. Default is `123456789`.

- ...:

  Additional arguments passed to
  [`mrgsolve::mrgsim_df()`](https://mrgsolve.org/docs/reference/mrgsim.html).

## Value

A data.frame with `data` x `replicates` rows (unless `obsonly=TRUE` is
passed to
[`mrgsolve::mrgsim_df()`](https://mrgsolve.org/docs/reference/mrgsim.html))
and the output variables in `output_vars`, `num_vars`, and `char_vars`.

## Examples

``` r
model <- model_mread_load(model = "model")
#> Loading model from cache.
simout <- df_mrgsim_replicate(data = data_sad, model = model, replicates = 100,
dv_var = "ODV",
num_vars = c("CMT", "LLOQ", "EVID", "MDV", "WTBL", "FOOD"),
char_vars = c("USUBJID", "PART"),
irep_name = "SIM")
```
