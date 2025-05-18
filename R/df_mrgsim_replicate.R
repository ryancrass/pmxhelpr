#' Execute a visual predictive check (VPC) simulation using `mrgsolve`
#'
#' @description  `df_mrgsim_replicate()` is a wrapper function for [mrgsolve::mrgsim_df()]
#' that returns a data.frame containing `replicates` iterations of `data`
#'
#' @param data Input dataset. Must contain required variables for `mrgsim_df()` other than those handled by
#'    other arguments.
#' @param model `mrgsolve` model object.
#' @param replicates Number of replicates. Either an integer, or something coercible to an integer.
#' @param dv_var Character name of the DV variable in `data`.
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
#' @return A data.frame with `data` x `replicates` rows (unless `obsonly=TRUE` is passed to [mrgsolve::mrgsim_df()])
#'    and the output variables in `output_vars`, `num_vars`, and `char_vars`.
#'
#' @importFrom rlang :=
#' @export df_mrgsim_replicate
#'
#' @examples
#' model <- model_mread_load(model = "model")
#' simout <- df_mrgsim_replicate(data = data_sad, model = model, replicates = 100,
#' dv_var = "ODV",
#' num_vars = c("CMT", "LLOQ", "EVID", "MDV", "WTBL", "FOOD"),
#' char_vars = c("USUBJID", "PART"),
#' irep_name = "SIM")

df_mrgsim_replicate <- function(data,
                    model,
                    replicates,
                    dv_var = "DV",
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

  ##Update Defaults to time_vars and output_vars
  time_vars <- list_update(time_vars, c(TIME = "TIME",
                                        NTIME = "NTIME"))
  output_vars <- list_update(output_vars, c(PRED = "PRED",
                                            IPRED = "IPRED",
                                            DV = "DV"))

  #Checks
  check_df(data)
  check_mrgmod(model)
  check_mrgmod_outputvars(model, output_vars)
  check_integer(replicates)
  check_varsindf(data, dv_var)
  check_varsindf(data, time_vars)
  check_varsindf(data, num_vars)
  check_varsindf(data, char_vars)
  check_integer(seed)

  ##Data Rename
  data <- data |>
    dplyr::rename(dplyr::any_of(time_vars)) |>
    dplyr::rename("OBSDV" = !!as.symbol(dv_var))

  data <- df_addpred(data, model, output_var = output_vars[["IPRED"]])

  ##Run Simulation
  withr::with_seed(seed = seed,

                   simout <- lapply(
                     seq(as.integer(replicates)),
                     function(rep, data, model) {
                       mrgsolve::mrgsim_df(x = model, data = data,
                                           carry_out = paste(c(output_vars[["PRED"]], output_vars[["IPRED"]],
                                                               output_vars[["DV"]], "OBSDV","EVID", "MDV", "CMT",
                                                               time_vars,
                                                               num_vars),
                                                             collapse = ","),
                                           recover = paste(char_vars,collapse = ","),
                                           ...) |>
                         dplyr::mutate(!!irep_name := rep) |>
                         dplyr::select(ID,
                                       TIME,
                                       NTIME,
                                       PRED = output_vars[["PRED"]],
                                       IPRED = output_vars[["IPRED"]],
                                       SIMDV= output_vars[["DV"]],
                                       OBSDV,
                                       dplyr::everything())} ,
                     data = data,
                     model = model) |>
                     dplyr::bind_rows()

  )

  return(simout)
}
