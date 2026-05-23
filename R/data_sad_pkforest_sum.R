#' Example Pre-Summarized Dataset for Forest Plot of PK Test/Reference Comparisons
#' @format ## `data.frame` a data frame with 32 rows and 9 columns:
#' \describe{
#'   \item{ID}{Scenario identifier (values: 1=reference, 2=fed, 3=low body weight, 4=high body weight)}
#'   \item{WTBL}{Subject body weight at baseline (units: kg)}
#'   \item{FOOD}{Co-administration with food (values: 0=fasted, 1=high-fat meal)}
#'   \item{metric}{PK exposure metric name — absolute (`CMAX`, `CMIN`, `AUC`, `CAVG`) or test/reference ratio (`CMAXRATIO`, `CMINRATIO`, `AUCRATIO`, `CAVGRATIO`)}
#'   \item{cov_var}{Covariate column name (values: `REF`, `FOOD`, `WTBL`)}
#'   \item{cov_val}{Covariate level label (values: `REF`, `Fed`, `50 kg`, `90 kg`)}
#'   \item{P50}{Median of `value` across the 500 replicates in [data_sad_pkforest]}
#'   \item{P05}{5th percentile (lower bound of 90% CI)}
#'   \item{P95}{95th percentile (upper bound of 90% CI)}
#' }
#' @source simulated
"data_sad_pkforest_sum"
