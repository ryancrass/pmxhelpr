#' Execute a visual predictive check (VPC) simulation using `mrgsolve`
#'
#' @description  `df_mrgsim_replicate()` is a wrapper function for [mrgsolve::mrgsim_df()]
#' that returns a data.frame containing `replicates` iterations of `data`
#'
#' @param data Input dataset. Must contain required variables for `mrgsim_df()` other than those handled by
#'    other arguments.
#' @param model `mrgsolve` model object.
#' @param replicates Number of replicates. Either an integer, or something coercible to an integer.
#' @param dv_var Column containing the DV variable in `data`. Accepts bare names or strings.
#' @param time_vars Names of actual and nominal time variables. Must be named character vector.
#'    Defaults is: c(`TIME`=`"TIME"`, `NTIME`=`"NTIME"`).
#' @param output_vars Names of model outputs from `model`. Must be named character vector.
#'    Defaults is: c(`PRED`= `"PRED"`, `IPRED` = `"IPRED"`, `DV`= `"DV"`).
#' @param num_vars Numeric variables in `data` or simulation output to recover.
#'    Must be a character vector of variable names from the simulation output to `carry_out`
#'    and return in output. Default is `NULL`. Note that `"CMT"`, `"EVID"`, `"MDV"`,
#'    `"TIME"`, and `"NTIME"` are always carried automatically.
#' @param char_vars Character variables in `data` or simulation output to recover.
#'    Must be a character vector of variable names from the simulation output to `recover`
#'    and return in output.
#' @param irep_name Name of replicate variable in `data`. Accepts bare names or strings. Default is `SIM`.
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
#' model <- model_mread_load(model = "pkmodel")
#' data_sad_pk <- dplyr::filter(data_sad, CMT %in% c(1,2))
#' simout <- df_mrgsim_replicate(data = data_sad_pk, model = model, replicates = 100,
#' dv_var = ODV,
#' num_vars = c("CMT", "LLOQ", "EVID", "MDV", "WTBL", "FOOD"),
#' char_vars = c("USUBJID", "PART"),
#' irep_name = SIM)

df_mrgsim_replicate <- function(data,
                    model,
                    replicates,
                    dv_var = DV,
                    time_vars = c(TIME = "TIME",
                                  NTIME = "NTIME"),
                    output_vars = c(PRED = "PRED",
                                    IPRED = "IPRED",
                                    DV = "DV"),
                    num_vars = NULL,
                    char_vars = NULL,
                    irep_name = SIM,
                    seed = 123456789,
                    ...) {

  dv_var_str    <- rlang::as_name(rlang::ensym(dv_var))
  irep_name_str <- rlang::as_name(rlang::ensym(irep_name))

  ##Update Defaults to time_vars and output_vars
  time_vars <- init_time_vars(time_vars)
  output_vars <- list_update(output_vars, c(PRED = "PRED",
                                            IPRED = "IPRED",
                                            DV = "DV"))

  #Checks
  check_df(data)
  check_mrgmod(model)
  check_mrgmod_outputvars(model, output_vars)
  check_integer(replicates)
  if(replicates < 1) {rlang::abort(message = "argument `replicates` must be >= 1")}
  check_varsindf(data, dv_var_str)
  check_varsindf(data, time_vars)
  check_varsindf(data, num_vars)
  check_varsindf(data, char_vars)
  check_integer(seed)

  ##Handle DV Variable
  data <- dplyr::rename(data, dplyr::any_of(c(OBSDV = dv_var_str)))

  #Handle Time Variables
  data <- rename_time_vars(data, time_vars)

  data <- rlang::inject(df_addpred(data, model, output_var = !!output_vars[["IPRED"]]))

  ##Run Simulation
  withr::with_seed(seed = seed,

                   simout <- lapply(
                     seq(as.integer(replicates)),
                     function(rep, data, model) {
                       mrgsolve::mrgsim_df(x = model, data = data,
                                           carry_out = paste(unique(c(output_vars[["PRED"]], output_vars[["IPRED"]],
                                                               output_vars[["DV"]], "OBSDV", "EVID", "MDV", "CMT",
                                                               "TIME", "NTIME",
                                                               num_vars)),
                                                             collapse = ","),
                                           recover = paste(char_vars,collapse = ","),
                                           ...) |>
                         dplyr::mutate(!!irep_name_str := rep) |>
                         dplyr::rename(dplyr::any_of(c(PRED = output_vars[["PRED"]],
                                                       IPRED = output_vars[["IPRED"]],
                                                       SIMDV = output_vars[["DV"]]))) |>
                         dplyr::select(ID, TIME, NTIME,
                                       PRED, IPRED, SIMDV, OBSDV,
                                       dplyr::everything())} ,
                     data = data,
                     model = model) |>
                     dplyr::bind_rows()

  )

  return(simout)
}
