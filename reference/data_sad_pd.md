# Example NONMEM Analysis-Ready Dataset for PK/PD Modeling of a Single Ascending Dose Study

Example NONMEM Analysis-Ready Dataset for PK/PD Modeling of a Single
Ascending Dose Study

## Usage

``` r
data_sad_pd
```

## Format

### `data.frame` a data frame with 720 rows and 23 columns:

- LINE:

  Line number

- ID:

  NONMEM subject identifier

- TIME:

  Actual time since first dose (units: hours)

- NTIME:

  Nominal time since first dose (units: hours)

- NDAY:

  Nominal study day (units: days)

- DOSE:

  Nominal dose assignment (units: mg)

- AMT:

  Actual dose amount administrered (units: mg)

- EVID:

  NONMEM-specific event identifier

- ODV:

  Dependent variable in original units (units: ng/mL cmt=2, % cmt=3)

- LDV:

  Log-transformed dependent variable (units: log(ng/mL) cmt=2, % cmt=3)

- CFB:

  Dependent variable change from baseline (units: %)

- CONC:

  Time-matched Observed Drug Concentration with BLQ imputed to zero
  (units: ng/mL)

- CMT:

  NONMEM-specific compartment variable (values: 1=dose, 2=plasma
  concentration, 3=response)

- MDV:

  NONMEM-specific missing dependent variable indicator

- BLQ:

  Concentration below the lower limit of quantification flag (values:
  -1=pre-dose BLQ, 0=not BLQ, 1=post-dose BLQ)

- LLOQ:

  Lower limit of quantification (units: ng/mL)

- FOOD:

  Co-administration with food (values: 0=fasted, 1=high-fat meal)

- SEXF:

  Subject of female sex (values: 0=male, 1=female)

- RACE:

  Subject race (units: hours)

- AGEBL:

  Subject age at baseline (units: years)

- WTBL:

  Subject body weight at baseline (units:kg)

- SCRBL:

  Subject serum creatinine at baseline (units: g/dL)

- CRCLBL:

  Subject creatinine clearance at baseline (units: mL/min)

- USUBJID:

  Study subject identifier

- PART:

  Study part

## Source

simulated
