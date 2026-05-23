#' Example Replicate-Draws Dataset for Forest Plot of PK Test/Reference Comparisons
#' @format ## `data.frame` a data frame with 16000 rows and 8 columns:
#' \describe{
#'   \item{ID}{Scenario identifier (values: 1=reference, 2=fed, 3=low body weight, 4=high body weight)}
#'   \item{SIM}{Bootstrap replicate identifier (1-500 per scenario)}
#'   \item{WTBL}{Subject body weight at baseline (units: kg)}
#'   \item{FOOD}{Co-administration with food (values: 0=fasted, 1=high-fat meal)}
#'   \item{metric}{PK exposure metric name — absolute (`CMAX`, `CMIN`, `AUC`, `CAVG`) or test/reference ratio (`CMAXRATIO`, `CMINRATIO`, `AUCRATIO`, `CAVGRATIO`)}
#'   \item{value}{Numeric value of the metric for this `(ID x SIM)` replicate}
#'   \item{cov_var}{Covariate column name (values: `REF`, `FOOD`, `WTBL`)}
#'   \item{cov_val}{Covariate level label (values: `REF`, `Fed`, `50 kg`, `90 kg`)}
#' }
#' @source simulated
"data_sad_pkforest"
