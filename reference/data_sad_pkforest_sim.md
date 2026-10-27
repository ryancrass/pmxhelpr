# Example Replicate Simulation Output of PK Exposure Metrics Across Covariate Scenarios

Raw replicate simulation output behind
[data_sad_pkforest](https://ryancrass.github.io/pmxhelpr/reference/data_sad_pkforest.md):
PK exposure metrics for a typical individual under six covariate
scenarios, one row per scenario per replicate, with the metrics as
columns. This is the shape a forest plot simulation produces before
test/reference ratios are formed.

## Usage

``` r
data_sad_pkforest_sim
```

## Format

### `data.frame` a data frame with 3000 rows and 9 columns:

- ID:

  Scenario identifier (values: 1=reference, 2=fed, 3=50 kg, 4=60 kg,
  5=80 kg, 6=90 kg)

- SIM:

  Replicate identifier (1-500 per scenario); each replicate uses one
  sampled parameter set across all scenarios

- WTBL:

  Scenario body weight (units: kg; 70 kg for the reference)

- FOOD:

  Scenario food status (values: 0=fasted, 1=high-fat meal)

- DOSE:

  Dose (units: mg; 200 mg once daily in every scenario)

- CMAX:

  Maximum concentration over the steady-state dosing interval (units:
  ng/mL)

- CMIN:

  Concentration at the end of the steady-state dosing interval (units:
  ng/mL)

- AUC:

  Area under the concentration-time curve over the 24-hour dosing
  interval (units: ng\*h/mL)

- CAVG:

  Average concentration over the dosing interval, `AUC / 24` (units:
  ng/mL)

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

## See also

[data_sad_pkforest](https://ryancrass.github.io/pmxhelpr/reference/data_sad_pkforest.md)
for the long-format draws with test/reference ratios.
