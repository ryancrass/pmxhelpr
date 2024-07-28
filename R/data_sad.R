#' Example NONMEM Analysis-Ready Dataset for PK Modeling of a Single Ascending Dose Study
#' @format ## `data.frame` a data frame with 720 rows and 23 columns:
#' \describe{
#'   \item{LINE}{Line number }
#'   \item{ID}{NONMEM subject identifier}
#'   \item{TIME}{Actual time since first dose (units: hours)}
#'   \item{NTIME}{Nominal time since first dose (units: hours)}
#'   \item{NDAY}{Nominal study day (units: days)}
#'   \item{DOSE}{Nominal dose assignment (units: mg)}
#'   \item{AMT}{Actual dose amount administrered (units: mg)}
#'   \item{EVID}{NONMEM-specific event identifier}
#'   \item{ODV}{Dependent variable in original units (units: ng/mL)}
#'   \item{LDV}{Log-transformed dependent variable (units: log(ng/mL))}
#'   \item{CMT}{NONMEM-specific compartment variable (values: 1=dose, 2=plasma concentration)}
#'   \item{MDV}{NONMEM-specific missing dependent variable indicator}
#'   \item{BLQ}{Concentration below the lower limit of quantification flag (values: -1=pre-dose BLQ, 0=not BLQ, 1=post-dose BLQ)}
#'   \item{LLOQ}{Lower limit of quantification (units: ng/mL)}
#'   \item{FOOD}{Co-administration with food (values: 0=fasted, 1=high-fat meal)}
#'   \item{SEXF}{Subject of female sex (values: 0=male, 1=female)}
#'   \item{RACE}{Subject race (units: hours)}
#'   \item{AGEBL}{Subject age at baseline (units: years)}
#'   \item{WTBL}{Subject body weight at baseline (units:kg)}
#'   \item{SCRBL}{Subject serum creatinine at baseline (units: g/dL)}
#'   \item{CRCLBL}{Subject creatinine clearance at baseline (units: mL/min)}
#'   \item{USUBJID}{Study subject identifier}
#'   \item{PART}{Study part}
#'   ...
#' }
#' @source simulated
"data_sad"
