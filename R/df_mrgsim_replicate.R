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
#' @param time_var Column containing the actual time variable.
#'    Accepts bare names or strings. Default is `TIME`.
#' @param ntime_var Column containing the nominal time variable.
#'    Accepts bare names or strings. Default is `NTIME`.
#' @param pred_var Name of population prediction output from `model`.
#'    Accepts bare names or strings. Default is `PRED`.
#' @param ipred_var Name of individual prediction output from `model`.
#'    Accepts bare names or strings. Default is `IPRED`.
#' @param sim_dv_var Name of simulated DV output from `model`.
#'    Accepts bare names or strings. Default is `DV`.
#' @param num_vars Numeric variables in `data` to carry into output.
#'    Default is `NULL`, which auto-carries all numeric columns of `data`
#'    not already in the always-carried set (`EVID`, `MDV`, `CMT`,
#'    `TIME`, `NTIME`, `ID`, `OBSDV`, `PRED`, `IPRED`, `SIMDV`).
#'    Pass an explicit character vector to carry exactly that list.
#' @param char_vars Character variables in `data` to recover into output.
#'    Default is `NULL`, which auto-recovers all character columns of `data`.
#'    Pass an explicit character vector to recover exactly that list.
#' @param irep_name Name of replicate variable in `data`. Accepts bare names or strings. Default is `SIM`.
#' @param seed Random seed. Default is `123456789`.
#' @param ... Additional arguments passed to [mrgsolve::mrgsim_df()].
#'
#' @return A data.frame with `data` x `replicates` rows (unless `obsonly=TRUE` is passed to [mrgsolve::mrgsim_df()])
#'    and the output variables `PRED`, `IPRED`, `SIMDV`, `OBSDV`, plus the columns selected by
#'    `num_vars` / `char_vars` (auto-carried by default; see those parameters).
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
                    time_var = TIME,
                    ntime_var = NTIME,
                    pred_var = PRED,
                    ipred_var = IPRED,
                    sim_dv_var = DV,
                    num_vars = NULL,
                    char_vars = NULL,
                    irep_name = SIM,
                    seed = 123456789,
                    ...) {

  dv_var_str     <- resolve_var(rlang::enquo(dv_var))
  time_var_str   <- resolve_var(rlang::enquo(time_var))
  ntime_var_str  <- resolve_var(rlang::enquo(ntime_var))
  pred_var_str   <- resolve_var(rlang::enquo(pred_var))
  ipred_var_str  <- resolve_var(rlang::enquo(ipred_var))
  sim_dv_var_str <- resolve_var(rlang::enquo(sim_dv_var))
  irep_name_str  <- resolve_var(rlang::enquo(irep_name))

  #Checks
  check_df(data, "data")
  if (nrow(data) == 0L) rlang::abort("argument `data` must have at least one row")
  check_mrgmod(model, "model")
  check_mrgmod_outputvars(model, sim_dv_var_str, ipred_var_str)
  check_integer(replicates, "replicates")
  if(replicates < 1) {rlang::abort(message = "argument `replicates` must be >= 1")}
  check_varsindf(data, dv_var_str, "data", "dv_var")
  check_varsindf(data, time_var_str, "data", "time_var")
  check_varsindf(data, ntime_var_str, "data", "ntime_var")
  check_varsindf(data, num_vars, "data", "num_vars")
  check_varsindf(data, char_vars, "data", "char_vars")
  check_integer(seed, "seed")

  ##Handle DV Variable
  data <- dplyr::rename(data, dplyr::any_of(c(OBSDV = dv_var_str)))

  #Handle Time Variables
  data <- df_prep_timevars(data, time_var_str, ntime_var_str)

  data <- df_mrgsim_addpred(data, model, output_var = ipred_var_str)

  #Variables to Return
  default_vars <- c("EVID", "MDV", "CMT")
  out_vars <- c(pred_var_str, ipred_var_str,sim_dv_var_str, "OBSDV", "TIME", "NTIME")

  #Auto-carry input columns when the user did not enumerate them explicitly.
  #`data` here has been renamed (dv_var -> OBSDV) and time-prepped, so its
  #column names align with what mrgsolve will emit.
  auto_pool <- setdiff(colnames(data), c(default_vars, out_vars, "ID"))
  if (is.null(num_vars)) {
    num_vars <- auto_pool[vapply(data[auto_pool], is.numeric, logical(1))]
  }
  if (is.null(char_vars)) {
    char_vars <- auto_pool[vapply(data[auto_pool], is.character, logical(1))]
  }

  ##Run Simulation
  withr::with_seed(seed = seed,

                   simout <- lapply(
                     seq(as.integer(replicates)),
                     function(rep, data, model) {
                       mrgsolve::mrgsim_df(x = model, data = data,
                                           carry_out = paste(unique(c(default_vars,
                                                                      out_vars,
                                                                      num_vars)),
                                                             collapse = ","),
                                           recover = paste(char_vars,collapse = ","),
                                           ...) |>
                         dplyr::mutate("{irep_name_str}" := rep) |>
                         dplyr::rename(dplyr::any_of(c(PRED = pred_var_str,
                                                       IPRED = ipred_var_str,
                                                       SIMDV = sim_dv_var_str))) |>
                         dplyr::select(ID, TIME, NTIME,
                                       PRED, IPRED, SIMDV, OBSDV,
                                       dplyr::everything())} ,
                     data = data,
                     model = model) |>
                     dplyr::bind_rows()

  )

  return(simout)
}
