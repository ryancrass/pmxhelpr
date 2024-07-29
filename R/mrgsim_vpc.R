#' mrgsim_vpc
#'
#' @param data simulation input dataset
#' @param model mrgsolve model object
#' @param replicates number of iterations in the simulation
#' @param time_vars named character vector of time variables (default: c(TIME = "TIME", NTIME = "NTIME"))
#' @param output_vars named character vector of numeric output variables to return (default: c(PRED = "PRED", IPRED = "IPRED", DV = "DV"))
#' @param num_vars character vector of numeric variable names from the simulation output to return
#' @param char_vars character vector of variable names to return
#' @param irep_name character string name of the iteration variable (default: "SIM")
#' @param seed random seed
#' @param ... additional arguments passed to mrgsolve::mrgsim_df
#'
#' @return data.frame of simulated output
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
                    num_vars,
                    char_vars,
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
                                                               num_vars),
                                                             collapse = ","),
                                           recover = paste(char_vars,collapse = ","),
                                           ...) |>
                         dplyr::mutate(!!irep_name := rep) |>
                         dplyr::select(ID, TIME, PRED, IPRED, SIMDV=DV,OBSDV, dplyr::everything())} ,
                     data = data,
                     model = model) |>
                     dplyr::bind_rows()

  )

  return(simout)
}
