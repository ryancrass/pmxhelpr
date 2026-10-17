#' Example Replicate-Draws Dataset for Forest Plot of PK Test/Reference Comparisons
#' @format ## `data.frame` a data frame with 16000 rows and 9 columns:
#' \describe{
#'   \item{ID}{Scenario identifier (values: 1=reference, 2=fed, 3=low body weight, 4=high body weight)}
#'   \item{SIM}{Bootstrap replicate identifier (1-500 per scenario)}
#'   \item{WTBL}{Subject body weight at baseline (units: kg)}
#'   \item{FOOD}{Co-administration with food (values: 0=fasted, 1=high-fat meal)}
#'   \item{metric}{PK exposure metric name — absolute (`CMAX`, `CMIN`, `AUC`, `CAVG`) or test/reference ratio (`CMAXRATIO`, `CMINRATIO`, `AUCRATIO`, `CAVGRATIO`)}
#'   \item{value}{Numeric value of the metric for this `(ID x SIM)` replicate}
#'   \item{cov_var}{Covariate column name (values: `Reference`, `FOOD`, `WTBL`)}
#'   \item{cov_var_desc}{Covariate description corresponding to `cov_var` (values: `Reference`, `Food`, `Weight`)}
#'   \item{cov_val}{Covariate level label (values: `Reference`, `Fed`, `50 kg`, `90 kg`)}
#'   \item{cov_ref}{Reference level of the covariate varied in this row (values: `NA` for `Reference` rows, `"Fasted"` for `FOOD` rows, `"70 kg"` for `WTBL` rows). Informational; pass the same mapping to [df_forest()] as `cov_level_ref = c(FOOD = "Fasted", WTBL = "70 kg")` to disperse the reference row into each covariate panel.}
#' }
#' @source simulated
"data_sad_pkforest"
