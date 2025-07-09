#' Example NCA Parameter Dataset Output from PKNCA
#' @format ## `data.frame` a data frame with 720 rows and 23 columns:
#' \describe{
#'   \item{ID}{NONMEM subject identifier}
#'   \item{DOSE}{Nominal dose assignment (units: mg)}
#'   \item{PART}{Study part}
#'   \item{start}{Start of the interval over which parameters are derived (units: hours)}
#'   \item{end}{End of the interval over which parameters are derived (units: hours)}
#'   \item{PPTESTCD}{NCA parameter names}
#'   \item{PPORRES}{NCA parameter values}
#'   \item{exclude}{Flag for exclusion}
#'   \item{units_dose}{Dose units}
#'   \item{units_conc}{Concentration units}
#'   \item{units_time}{Time units}
#'   ...
#' }
#' @source simulated
"data_sad_nca"
