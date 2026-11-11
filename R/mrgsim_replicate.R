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
#' @param pred_var Must be `"PRED"` (the default). The population prediction
#'    is computed by [df_mrgsim_addpred()] from `ipred_var` with random
#'    effects zeroed, and is always returned as `PRED`.
#' @param ipred_var Name of the individual prediction output captured by
#'    `model` (a model output, not a column of `data`). Returned as `IPRED`.
#'    Accepts bare names or strings. Default is `IPRED`.
#' @param sim_dv_var Name of the simulated DV output captured by `model` (a
#'    model output, not a column of `data`). Returned as `SIMDV`.
#'    Accepts bare names or strings. Default is `DV`.
#' @param rep_var Name of replicate variable in `data`. Accepts bare names or strings. Default is `SIM`.
#' @param irep_name `r lifecycle::badge("deprecated")` Use `rep_var`.
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
#' @importFrom rlang := %||% .data
#' @export df_mrgsim_replicate
#'
#' @examples
#' model <- model_mread_load(model = "pkmodel")
#' data_sad_pk <- dplyr::filter(data_sad, CMT %in% c(1,2))
#' # 20 replicates keeps the example fast; use >= 100 for a real VPC
#' simout <- df_mrgsim_replicate(data = data_sad_pk, model = model, replicates = 20,
#' dv_var = ODV,
#' carry_out = c("LLOQ", "WTBL", "FOOD"),
#' recover = c("USUBJID", "PART"),
#' rep_var = SIM)

df_mrgsim_replicate <- function(data,
                    model,
                    replicates,
                    dv_var = "DV",
                    time_var = "TIME",
                    ntime_var = "NTIME",
                    pred_var = "PRED",
                    ipred_var = "IPRED",
                    sim_dv_var = "DV",
                    rep_var = "SIM",
                    seed = 123456789,
                    ...,
                    irep_name) {

  dv_var_str     <- resolve_var(rlang::enquo(dv_var))
  time_var_str   <- resolve_var(rlang::enquo(time_var))
  ntime_var_str  <- resolve_var(rlang::enquo(ntime_var))
  pred_var_str   <- resolve_var(rlang::enquo(pred_var))
  ipred_var_str  <- resolve_var(rlang::enquo(ipred_var))
  sim_dv_var_str <- resolve_var(rlang::enquo(sim_dv_var))
  rep_var_q <- rlang::enquo(rep_var)
  if (use_deprecated_arg(!missing(irep_name), !missing(rep_var), "df_mrgsim_replicate",
                         "irep_name", "rep_var")) {
    rep_var_q <- rlang::enquo(irep_name)
  }
  rep_var_str    <- resolve_var(rep_var_q)

  #Checks
  check_df(data, "data")
  check_mrgmod(model, "model")
  check_mrgmod_outputvars(model, sim_dv_var_str, ipred_var_str)
  if (pred_var_str != "PRED") {
    pmx_abort(c(
      "{.arg pred_var} must be {.val PRED}.",
      "i" = "The population prediction is computed from {.arg ipred_var} with random effects zeroed and is always returned as {.var PRED}."
    ), "bad_arg")
  }
  check_integer(replicates, "replicates")
  if(replicates < 1) {pmx_abort("{.arg replicates} must be >= 1.", "bad_arg")}
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
  dots <- rlang::list2(...)
  ## `parallel` was a formal argument until 0.7.0; without this check it would
  ## fall into `...` and mrgsim_df() would warn once per replicate.
  if ("parallel" %in% names(dots)) {
    pmx_abort("{.arg parallel} was removed in pmxhelpr 0.7.0; replicates always run sequentially.",
              "bad_arg")
  }
  dots$carry_out <- union(internal_carry, dots$carry_out %||% character(0))

  ##Run Simulation
  rep_fn <- function(rep, data, model) {
    out <- do.call(mrgsolve::mrgsim_df,
                   c(list(x = model, data = data), dots))
    out[[rep_var_str]] <- rep
    out
  }

  reps <- seq_len(replicates)

  withr::with_seed(
    seed = seed,
    simout <- lapply(reps, rep_fn, data = data, model = model) |>
      dplyr::bind_rows()
  )

  simout <- simout |>
    dplyr::rename(dplyr::any_of(c(PRED  = pred_var_str,
                                  IPRED = ipred_var_str,
                                  SIMDV = sim_dv_var_str))) |>
    dplyr::select("ID", "TIME", "NTIME",
                  "PRED", "IPRED", "SIMDV", "OBSDV",
                  dplyr::everything())

  return(simout)
}
