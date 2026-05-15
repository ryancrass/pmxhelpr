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
#' @param irep_name Name of replicate variable in `data`. Accepts bare names or strings. Default is `SIM`.
#' @param seed Random seed. Default is `123456789`.
#' @param ... Additional arguments passed to [mrgsolve::mrgsim_df()], including
#'   `carry_out` and `recover` to control which input columns are propagated to
#'   the output. The always-carried set (`EVID`, `MDV`, `CMT`, `TIME`, `NTIME`,
#'   `OBSDV`, and the population prediction column) is added to whatever the
#'   user passes to `carry_out`.
#'
#' @family mrgsolve wrappers
#' @return A data.frame with `data` x `replicates` rows (unless `obsonly=TRUE` is passed to [mrgsolve::mrgsim_df()])
#'    and the output variables `PRED`, `IPRED`, `SIMDV`, `OBSDV`, plus any input
#'    columns listed in `carry_out` / `recover`.
#'
#' @importFrom rlang := %||%
#' @export df_mrgsim_replicate
#'
#' @examples
#' model <- model_mread_load(model = "pkmodel")
#' data_sad_pk <- dplyr::filter(data_sad, CMT %in% c(1,2))
#' simout <- df_mrgsim_replicate(data = data_sad_pk, model = model, replicates = 100,
#' dv_var = ODV,
#' carry_out = c("LLOQ", "WTBL", "FOOD"),
#' recover = c("USUBJID", "PART"),
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
  check_integer(seed, "seed")

  ##Handle DV Variable
  data <- dplyr::rename(data, dplyr::any_of(c(OBSDV = dv_var_str)))

  #Handle Time Variables
  data <- df_prep_timevars(data, time_var_str, ntime_var_str)

  data <- df_mrgsim_addpred(data, model, output_var = ipred_var_str)

  #Always-carried set required by the downstream dplyr::select(); union with
  #user-supplied carry_out so the function's output structure is preserved.
  internal_carry <- c("EVID", "MDV", "CMT", "TIME", "NTIME", "OBSDV", pred_var_str)
  dots <- list(...)
  user_carry   <- dots$carry_out %||% character(0)
  user_recover <- dots$recover   %||% character(0)
  dots$carry_out <- NULL
  dots$recover   <- NULL
  carry_out_final <- union(internal_carry, user_carry)

  ##Run Simulation
  withr::with_seed(
    seed = seed,
    simout <- lapply(
      seq(as.integer(replicates)),
      function(rep, data, model) {
        do.call(mrgsolve::mrgsim_df,
                c(list(x = model, data = data,
                       carry_out = carry_out_final,
                       recover   = user_recover),
                  dots)) |>
          dplyr::mutate("{irep_name_str}" := rep) |>
          dplyr::rename(dplyr::any_of(c(PRED = pred_var_str,
                                        IPRED = ipred_var_str,
                                        SIMDV = sim_dv_var_str))) |>
          dplyr::select(ID, TIME, NTIME,
                        PRED, IPRED, SIMDV, OBSDV,
                        dplyr::everything())
      },
      data = data,
      model = model
    ) |> dplyr::bind_rows()
  )

  return(simout)
}
