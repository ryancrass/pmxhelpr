#' Execute a visual predictive check (VPC) simulation using `mrgsolve`
#'
#' @description  `mrgsim_vpc()` is a wrapper function for [mrgsolve::mrgsim_df()]
#' that returns a data.frame containing `replicates` iterations of `data`
#'
#' @param data Input dataset.
#' @param model `mrgsolve` model object.
#' @param replicates Number of replicates. Either an integer, or something coercible to an integer.
#' @param time_vars Names of actual and nominal time variables. Must be named character vector.
#'    Defaults are `"TIME"` and `"NTIME"`.
#' @param output_vars Names of model outputs from `model`. Must be named character vector.
#'    Defaults are `"PRED"`, `"IPRED"`, and `"DV"`.
#' @param num_vars Numeric variables in `data` or simulation output to recover.
#'    Must be a character vector of variable names from the simulation output to `carry_out`
#'    and return in output. Defaults are `"CMT"`, `"EVID"`, `"MDV"`, `"NTIME"`.
#' @param char_vars Character variables in `data` or simulation output to recover.
#'    Must be a character vector of variable names from the simulation output to `recover`
#'    and return in output.
#' @param irep_name Name of replicate variable in `data`. Must be a string. Default is `"SIM"`.
#' @param seed Random seed. Default is `123456789`.
#' @param ... Additional arguments passed to [mrgsolve::mrgsim_df()].
#'
#' @return A data.frame with `data` x `replicates` rows (unless `obsonly=TRUE`)
#'    and the output variables in `output_vars`, `num_vars`, and `char_vars`.
#'
#' @importFrom rlang :=
#' @export mrgsim_vpc
#'
#' @examples
#' model <- model_load(model = "model")
#' simout <- mrgsim_vpc(data = data_sad, model = model, replicates = 100,
#' output_vars = c(DV = "ODV"),
#' num_vars = c("CMT", "LLOQ", "EVID", "MDV", "WTBL", "FOOD"),
#' char_vars = c("USUBJID", "PART"),
#' irep_name = "SIM")

mrgsim_vpc <- function(data,
                    model,
                    replicates,
                    time_vars = c(TIME = "TIME",
                                  NTIME = "NTIME"),
                    output_vars = c(PRED = "PRED",
                                    IPRED = "IPRED",
                                    DV = "DV"),
                    num_vars = NULL,
                    char_vars = NULL,
                    irep_name = "SIM",
                    seed = 123456789,
                    ...) {
  ##Data Rename
  data <- data |>
    dplyr::rename(dplyr::any_of(c(output_vars, time_vars))) |>
    dplyr::rename("OBSDV" = DV)

  data <- df_add_pred(data, model)

  ##Run Simulation
  withr::with_seed(seed = seed,

                   simout <- lapply(
                     seq(replicates),
                     function(rep, data, model) {
                       mrgsolve::mrgsim_df(x = model, data = data,
                                           carry_out = paste(c("PRED", "IPRED", "DV", "OBSDV",
                                                               time_vars,
                                                               num_vars),
                                                             collapse = ","),
                                           recover = paste(char_vars,collapse = ","),
                                           ...) |>
                         dplyr::mutate(!!irep_name := rep) |>
                         dplyr::select(ID, TIME, NTIME, PRED, IPRED, SIMDV=DV,OBSDV, dplyr::everything())} ,
                     data = data,
                     model = model) |>
                     dplyr::bind_rows()

  )

  return(simout)
}
