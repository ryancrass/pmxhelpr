#' Execute a visual predictive check (VPC) simulation using `mrgsolve`
#'
#' @description  `df_mrgsim_replicate()` is a wrapper function for [mrgsolve::mrgsim_df()]
#' that returns a data.frame containing `replicates` iterations of `data`
#'
#' @param data Input dataset. Must contain required variables for `mrgsim_df()` other than those handled by
#'    other arguments.
#' @param model `mrgsolve` model object.
#' @param replicates Number of replicates. Either an integer, or something coercible to an integer.
#'    May be omitted when `params` is given; it is then `nrow(params)`.
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
#' @param params A data.frame of parameter values, one row per replicate, or
#'    `NULL` (default) to run every replicate on `model`'s parameters. Row
#'    `i` sets the parameters of replicate `i` (e.g. one row per bootstrap
#'    run or posterior draw, for a VPC that includes parameter uncertainty),
#'    so `params` needs exactly `replicates` rows. Columns must be numeric,
#'    named for parameters of `model` (see [mrgsolve::param()]), and not also
#'    columns of `data`, which mrgsolve would use instead. Each replicate's
#'    `PRED` is simulated with that replicate's parameters.
#' @param ... Additional arguments passed to [mrgsolve::mrgsim_df()], including
#'   `carry_out` and `recover` to control which input columns are propagated to
#'   the output. The always-carried set (`EVID`, `MDV`, `CMT`, `TIME`, `NTIME`,
#'   `OBSDV`, and the population prediction column) is added to whatever the
#'   user passes to `carry_out`.
#'
#' @family mrgsolve wrappers
#' @return A data.frame with `data` x `replicates` rows (unless `obsonly=TRUE` is passed to [mrgsolve::mrgsim_df()])
#'    and the output variables `PRED`, `IPRED`, `SIMDV`, `OBSDV`, plus any input
#'    columns listed in `carry_out` / `recover`. Existing columns with these
#'    names are replaced (with a warning for data columns). With `params`,
#'    `PRED` differs by replicate.
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
#'
#' # Parameter uncertainty: one parameter set per replicate (here 20 draws
#' # around the model's typical values; in practice, bootstrap or posterior
#' # estimates)
#' draws <- withr::with_seed(1, data.frame(
#'   TVCL = rlnorm(20, log(20), 0.1),
#'   TVVC = rlnorm(20, log(35.7), 0.1)
#' ))
#' simout_unc <- df_mrgsim_replicate(data = data_sad_pk, model = model,
#'                                   params = draws, dv_var = ODV)

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
                    params = NULL,
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
  if (!is.null(params)) check_df(params, "params")
  if (missing(replicates)) {
    if (is.null(params)) {
      pmx_abort("{.arg replicates} is required unless {.arg params} is given.", "bad_arg")
    }
    replicates <- nrow(params)
  }
  check_integer(replicates, "replicates")
  if(replicates < 1) {pmx_abort("{.arg replicates} must be >= 1.", "bad_arg")}
  check_varsindf(data, dv_var_str, "data", "dv_var")
  check_varsindf(data, time_var_str, "data", "time_var")
  check_varsindf(data, ntime_var_str, "data", "ntime_var")
  check_integer(seed, "seed")

  ##Handle DV Variable
  data <- set_output_col(data, "OBSDV", dv_var_str, "`dv_var` column", character(0))
  if (dv_var_str != "OBSDV") data[[dv_var_str]] <- NULL

  #Handle Time Variables
  data <- df_prep_timevars(data, time_var_str, ntime_var_str)

  if (!is.null(params)) check_params(params, model, data, replicates)

  ## PRED from model's parameters; with `params`, each replicate replaces it
  ## with its own (see rep_fn).
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
    if (!is.null(params)) {
      model <- mrgsolve::param(model, as.list(params[rep, , drop = FALSE]))
      ## Replacing the base PRED is expected here, so its overwrite warning
      ## is muffled.
      data <- withCallingHandlers(
        df_mrgsim_addpred(data, model, ipred_output = ipred_output_str),
        pmxhelpr_warning_overwrite = function(w) invokeRestart("muffleWarning")
      )
    }
    out <- do.call(mrgsolve::mrgsim_df,
                   c(list(x = model, data = data), dots))
    out[[rep_var_str]] <- rep
    out
  }

  reps <- seq_len(replicates)

  ## Validate the input data once rather than in every replicate's mrgsim
  ## call. `recover` needs the raw data frame (its output differs on a
  ## validated data set), and with `params` each replicate's data carries its
  ## own PRED, so those cases keep per-replicate validation.
  if (is.null(dots$recover) && is.null(params)) data <- mrgsolve::valid_data_set(data, model)

  withr::with_seed(
    seed = seed,
    simout <- lapply(reps, rep_fn, data = data, model = model) |>
      dplyr::bind_rows()
  )

  simout <- set_output_col(simout, "IPRED", ipred_output_str, "`ipred_output` output",
                           model_outputs)
  simout <- set_output_col(simout, "SIMDV", dv_output_str, "`dv_output` output",
                           model_outputs)
  sources <- setdiff(c(ipred_output_str, dv_output_str), c("PRED", "IPRED", "SIMDV", "OBSDV"))
  simout <- simout[setdiff(names(simout), sources)]
  simout <- simout |>
    dplyr::select("ID", "TIME", "NTIME",
                  "PRED", "IPRED", "SIMDV", "OBSDV",
                  dplyr::everything())

  return(simout)
}


## `params` for df_mrgsim_replicate(): numeric, NA-free columns named for
## model parameters, none of them also a column of `data` (mrgsolve would
## use the data column and silently ignore the parameter), one row per
## replicate.
check_params <- function(params, model, data, replicates, call = rlang::caller_env()) {
  if (ncol(params) == 0) {
    pmx_abort("{.arg params} must have at least one parameter column.", "bad_arg", call = call)
  }
  bad_type <- names(params)[!vapply(params, function(x) is.numeric(x) && !anyNA(x), logical(1))]
  if (length(bad_type) > 0) {
    pmx_abort("{.arg params} column{?s} {.var {bad_type}} must be numeric with no missing values.",
              "bad_arg", call = call)
  }
  unknown <- setdiff(names(params), names(mrgsolve::param(model)))
  if (length(unknown) > 0) {
    pmx_abort(c("{.arg params} column{?s} {.var {unknown}} {?is/are} not {?a parameter/parameters} of {.arg model}.",
                "i" = "See {.code mrgsolve::param(model)} for its parameters."),
              "bad_arg", call = call)
  }
  overlap <- intersect(names(params), names(data))
  if (length(overlap) > 0) {
    pmx_abort(c("{.arg params} column{?s} {.var {overlap}} {?is/are} also {?a column/columns} of {.arg data}, which mrgsolve uses instead.",
                "i" = "Drop {cli::qty(overlap)}{?it/them} from {.arg params} or from {.arg data}."),
              "bad_arg", call = call)
  }
  if (nrow(params) != replicates) {
    n_params <- nrow(params)
    pmx_abort("{.arg params} has {n_params} row{?s}; it needs one per replicate ({replicates}).",
              "bad_arg", call = call)
  }
}


## Set the fixed output column `target` from column `source`. A different
## column already named `target` is replaced; that warns unless it is a model
## output (the caller chose another source), mirroring df_mrgsim_addpred()'s
## PRED warning.
set_output_col <- function(df, target, source, what, model_outputs,
                           call = rlang::caller_env()) {
  if (identical(target, source)) return(df)
  if (target %in% names(df) && !target %in% model_outputs) {
    pmx_warn("{.arg data} already contains a {.var {target}} column; it is replaced by the {what} {.var {source}}.",
             "overwrite", call = call)
  }
  df[[target]] <- df[[source]]
  df
}
