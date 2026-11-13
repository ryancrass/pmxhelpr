#' Load an mrgsolve model file from the internal model library
#'
#' @description  `model_mread_load()` returns an mrgsolve model object from the internal
#' model library in `pmxhelpr`
#'
#' @details
#' The library holds the models used to simulate the example datasets
#' ([data_sad] and the datasets derived from it. Print one with
#' [mrgsolve::see()] for the full code.
#'
#' * `"pkmodel"`: two-compartment PK model with transit (n=1) absorption and
#'    dose-dependent bioavailabilty. Dose in mg; `IPRED` and `DV` in ng/mL.
#'   * Covariate effects, all read from same-named columns of the simulation
#'     input:
#'     * `WT` (kg) scales `CL` and `Q` with exponent 0.75, and `VC` and `VP`
#'       with exponent 1 (allometric scaling). Centered on 70 kg.
#'     * `FOOD = 1` reduces `KA` and increases bioavailability.
#'     * `DOSE` (mg) scales bioavailability by `(DOSE / 100)^0.33`
#'     * When a column is absent, its parameter keeps its default (`WT = 70`,
#'       `FOOD = 0`, `DOSE = 100`). `data_sad` has `DOSE` and `FOOD`, but
#'       weight as `WTBL`, so its simulations use `WT = 70`.
#'   * Between-subject variability: exponential on `CL`, `VC`, and `KA`
#'     (variances 0.075, 0.1, 0.2).
#'   * Residual error: proportional (variance 0.09).
#' * `"pdmodel"`: the same PK driving an indirect-response model in which
#'   concentration inhibits the response input (sigmoid Imax, `IMAX = -0.95`,
#'   `IC50 = 15` ng/mL, Hill 1.25) from a baseline of 100%. `IPRED` and `DV`
#'   are the response; `IPREDPK` is the concentration.
#'
#' @param model Model file name. Must be a string.
#' @param ... Additional arguments passed to [mrgsolve::mread_cache()].
#'
#' @family mrgsolve wrappers
#' @return An `mrgsolve` model object.
#' @export model_mread_load
#'
#' @examples
#' model <- model_mread_load("pkmodel")

model_mread_load <- function(model, ...){
  model_path <- system.file("models", paste0(model, ".cpp") , package = "pmxhelpr")
  check_modlib(model, model_path)
  model_object <- mrgsolve::mread_cache(model_path, ...)
  return(model_object)
}
