#' Example Replicate-Draws Dataset for Forest Plots of PK Test/Reference Comparisons
#'
#' @description
#' Long-format replicate draws of PK exposure metrics for a typical individual
#' under six covariate scenarios, ready for [df_forest()] / [plot_forest()]
#' with their default column names (`metric`, `cov_var`, `cov_val`, `value`).
#' Built from [data_sad_pkforest_sim] by dividing each metric by its value in
#' the reference scenario (`ID == 1`) within the same replicate to form a
#' test/reference ratio, then stacking the absolute and ratio metrics to one
#' row per metric per scenario per replicate.
#'
#' @details
#' Each scenario is a typical individual (inter-individual variability zeroed)
#' receiving 200 mg once daily to steady state, simulated with the internal
#' `pkmodel`. Each of the 500 replicates runs every scenario on its own
#' parameter set, drawn by parametric bootstrap from a multivariate normal on
#' the model's log typical values and covariate coefficients with an assumed
#' variance-covariance matrix (the structure of sampling from a model's
#' covariance step). Exposure metrics are derived over the 24-hour dosing
#' interval at steady state. See `data-raw/data_sad_raw.R`.
#'
#' @format ## `data.frame` a data frame with 24000 rows and 8 columns:
#' \describe{
#'   \item{ID}{Scenario identifier (values: 1=reference, 2=fed, 3=50 kg, 4=60 kg, 5=80 kg, 6=90 kg)}
#'   \item{SIM}{Replicate identifier (1-500 per scenario)}
#'   \item{WTBL}{Scenario body weight (units: kg; 70 kg for the reference)}
#'   \item{FOOD}{Scenario food status (values: 0=fasted, 1=high-fat meal)}
#'   \item{metric}{PK exposure metric name: absolute (`CMAX`, `CMIN`, `AUC`, `CAVG`) or test/reference ratio (`CMAXRATIO`, `CMINRATIO`, `AUCRATIO`, `CAVGRATIO`)}
#'   \item{value}{Numeric value of the metric for this `(ID x SIM)` replicate}
#'   \item{cov_var}{Covariate varied in the scenario (values: `Reference`, `FOOD`, `WTBL`)}
#'   \item{cov_val}{Covariate level label (values: `Reference`, `Fed`, `50 kg`, `60 kg`, `80 kg`, `90 kg`). The reference levels of the varied covariates are `Fasted` (`FOOD`) and `70 kg` (`WTBL`); pass them to [df_forest()] as `cov_level_ref = c(FOOD = "Fasted", WTBL = "70 kg")` to disperse the reference row into each covariate panel.}
#' }
#' @seealso [data_sad_pkforest_sim] for the raw replicate output.
#' @source simulated
"data_sad_pkforest"
