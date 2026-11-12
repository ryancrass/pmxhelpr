# error messages name the exported function and the user's argument

    Code
      plot_dvtime(d1, dv_var = "ODV", dose_var = "nope", dosenorm = TRUE)
    Condition
      Error in `plot_dvtime()`:
      ! `dose_var` must name variable in `data`.
      x Not found: "nope".
      i Available columns: ID, TIME, NTIME, NDAY, DOSE, AMT, EVID, ODV, LDV, CFB, CONC, LINE, CMT, MDV, BLQ, LLOQ, FOOD, SEXF, ..., USUBJID, and PART.
    Code
      plot_vpc_cont(sim_pk(), pi = c(0.9, 0.1))
    Condition
      Error in `df_vpcstats()`:
      ! `pi` must be ordered: pi[1] < pi[2].
    Code
      plot_vpc_cont(vs, pi = c(0.1, 0.9))
    Condition
      Error in `plot_vpc_cont()`:
      ! `plot_vpc_cont()` cannot accept pipeline arguments when `data` is a precomputed <pmx_stats> object.
      x Pipeline argument passed: `pi`.
      i Either call `df_vpcstats()` with this argument and pass its result to `plot_vpc_cont()`, or pass raw data and let `plot_vpc_cont()` build the stats.
    Code
      plot_build_vpc(list())
    Condition
      Error in `plot_build_vpc()`:
      ! `stats` must be a <vpc_stats> object (output of `df_vpcstats()`).
    Code
      df_forest(dplyr::rename(data_sad_pkforest, m = metric))
    Condition
      Error in `df_forest()`:
      ! `metric_name_var/metric_value_var/cov_name_var/cov_level_var` must name variable in `data`.
      x Not found: "metric".
      i Available columns: ID, SIM, WTBL, FOOD, DOSE, m, value, cov_name, and cov_level.
    Code
      df_mrgsim_replicate(d1, model = list(), replicates = 2)
    Condition
      Error in `df_mrgsim_replicate()`:
      ! `model` must be class <mrgmod>.

