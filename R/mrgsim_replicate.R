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
#' @param ipred_output Name of the individual prediction output captured by
#'    `model` (a model output, not a column of `data`). Returned as `IPRED`,
#'    and, simulated with random effects zeroed ([mrgsolve::zero_re()]), as
#'    the population prediction `PRED`. Accepts bare names or strings.
#'    Default is `IPRED`.
#' @param dv_output Name of the simulated DV output captured by `model` (a
#'    model output, not a column of `data`). Returned as `SIMDV`.
#'    Accepts bare names or strings. Default is `DV`.
#' @param rep_var Name of replicate variable in `data`. Accepts bare names or strings. Default is `SIM`.
#' @param irep_name `r lifecycle::badge("deprecated")` Use `rep_var`.
#' @param ipred_var `r lifecycle::badge("deprecated")` Use `ipred_output`.
#' @param sim_dv_var `r lifecycle::badge("deprecated")` Use `dv_output`.
#' @param pred_var `r lifecycle::badge("deprecated")` Has no effect: `PRED`
#'    is always the `ipred_output` model output simulated with random effects
#'    zeroed.
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
                    ipred_output = "IPRED",
                    dv_output = "DV",
                    rep_var = "SIM",
                    seed = 123456789,
                    ...,
                    irep_name,
                    pred_var,
                    ipred_var,
                    sim_dv_var) {

  ## Bare names are looked up in data columns and in the model's outputs
  ## first, so the model must be checked before its outputs are read.
  check_mrgmod(model, "model")
  data_cols <- names(data)
  model_outputs <- mrgsolve::outvars(model)$capture
  dv_var_str     <- resolve_var(rlang::enquo(dv_var), cols = data_cols)
  time_var_str   <- resolve_var(rlang::enquo(time_var), cols = data_cols)
  ntime_var_str  <- resolve_var(rlang::enquo(ntime_var), cols = data_cols)
  ipred_output_q <- rlang::enquo(ipred_output)
  if (use_deprecated_arg(!missing(ipred_var), !missing(ipred_output), "df_mrgsim_replicate",
                         "ipred_var", "ipred_output")) {
    ipred_output_q <- rlang::enquo(ipred_var)
  }
  ipred_output_str <- resolve_var(ipred_output_q, cols = model_outputs)
  dv_output_q <- rlang::enquo(dv_output)
  if (use_deprecated_arg(!missing(sim_dv_var), !missing(dv_output), "df_mrgsim_replicate",
                         "sim_dv_var", "dv_output")) {
    dv_output_q <- rlang::enquo(sim_dv_var)
  }
  dv_output_str <- resolve_var(dv_output_q, cols = model_outputs)
  if (!missing(pred_var)) {
    lifecycle::deprecate_warn(
      "0.8.0", "df_mrgsim_replicate(pred_var)",
      details = "`PRED` is always the `ipred_output` model output simulated with random effects zeroed."
    )
  }
  rep_var_q <- rlang::enquo(rep_var)
  if (use_deprecated_arg(!missing(irep_name), !missing(rep_var), "df_mrgsim_replicate",
                         "irep_name", "rep_var")) {
    rep_var_q <- rlang::enquo(irep_name)
  }
  rep_var_str    <- resolve_var(rep_var_q)

  #Checks
  check_df(data, "data")
  check_mrgmod_outputvars(model, dv_output_str, ipred_output_str)
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

  data <- df_mrgsim_addpred(data, model, ipred_output = ipred_output_str)

  #Always-carried set required by the downstream dplyr::select(); union with
  #user-supplied carry_out so the function's output structure is preserved.
  internal_carry <- c("EVID", "MDV", "CMT", "TIME", "NTIME", "OBSDV", "PRED")
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
    dplyr::rename(dplyr::any_of(c(IPRED = ipred_output_str,
                                  SIMDV = dv_output_str))) |>
    dplyr::select("ID", "TIME", "NTIME",
                  "PRED", "IPRED", "SIMDV", "OBSDV",
                  dplyr::everything())

  return(simout)
}
