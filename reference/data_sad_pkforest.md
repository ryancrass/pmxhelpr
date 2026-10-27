# Example Replicate-Draws Dataset for Forest Plots of PK Test/Reference Comparisons

Long-format replicate draws of PK exposure metrics for a typical
individual under six covariate scenarios, ready for
[`df_forest()`](https://ryancrass.github.io/pmxhelpr/reference/df_forest.md)
/
[`plot_forest()`](https://ryancrass.github.io/pmxhelpr/reference/plot_forest.md)
with their default column names (`metric`, `cov_name`, `cov_level`,
`value`). Built from
[data_sad_pkforest_sim](https://ryancrass.github.io/pmxhelpr/reference/data_sad_pkforest_sim.md)
with
[`df_forest_preprocess()`](https://ryancrass.github.io/pmxhelpr/reference/df_forest_preprocess.md),
which divides each metric by its value in the reference scenario within
the same replicate to form a test/reference ratio, then stacks the
absolute and ratio metrics to one row per metric per scenario per
replicate.

## Usage

``` r
data_sad_pkforest
```

## Format

### `data.frame` a data frame with 24000 rows and 9 columns:

- ID:

  Scenario identifier (values: 1=reference, 2=fed, 3=50 kg, 4=60 kg,
  5=80 kg, 6=90 kg)

- SIM:

  Replicate identifier (1-500 per scenario)

- WTBL:

  Scenario body weight (units: kg; 70 kg for the reference)

- FOOD:

  Scenario food status (values: 0=fasted, 1=high-fat meal)

- DOSE:

  Dose (units: mg; 200 mg once daily in every scenario)

- metric:

  PK exposure metric name: absolute (`CMAX`, `CMIN`, `AUC`, `CAVG`) or
  test/reference ratio (`CMAXRATIO`, `CMINRATIO`, `AUCRATIO`,
  `CAVGRATIO`)

- value:

  Numeric value of the metric for this `(ID x SIM)` replicate

- cov_name:

  Covariate varied in the scenario (values: `Reference`, `FOOD`, `WTBL`)

- cov_level:

  Covariate level label (values: `Reference`, `Fed`, `50 kg`, `60 kg`,
  `80 kg`, `90 kg`). The reference levels of the varied covariates are
  `Fasted` (`FOOD`) and `70 kg` (`WTBL`); pass them to
  [`df_forest()`](https://ryancrass.github.io/pmxhelpr/reference/df_forest.md)
  as `cov_level_ref = c(FOOD = "Fasted", WTBL = "70 kg")` to draw the
  reference row in each covariate panel.

## Source

simulated

## Details

Each scenario is a typical individual (inter-individual variability
zeroed) receiving 200 mg once daily to steady state, simulated with the
internal `pkmodel`. Each of the 500 replicates runs every scenario on
its own parameter set, drawn by parametric bootstrap from a multivariate
normal on the model's log typical values and covariate coefficients with
an assumed variance-covariance matrix (the structure of sampling from a
model's covariance step). Exposure metrics are derived over the 24-hour
dosing interval at steady state. See `data-raw/data_sad_raw.R`.

## Attributes

Carries a `"cov_level_ref"` attribute,
`c(FOOD = "Fasted", WTBL = "70 kg")`, giving each varied covariate's
reference level label. Pass it to
[`df_forest()`](https://ryancrass.github.io/pmxhelpr/reference/df_forest.md)
as `cov_level_ref` to draw the reference row in every covariate panel.

## See also

[data_sad_pkforest_sim](https://ryancrass.github.io/pmxhelpr/reference/data_sad_pkforest_sim.md)
for the raw replicate output,
[`df_forest_preprocess()`](https://ryancrass.github.io/pmxhelpr/reference/df_forest_preprocess.md)
for the function that builds this frame.
