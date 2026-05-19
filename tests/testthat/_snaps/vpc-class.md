# summary.vpc_stats output is stable (snapshot)

    Code
      summary(out)
    Output
      <vpc_stats>
        stats: 38 rows x 35 columns
        obs:   515 rows
        config: n_replicates = 5, loq = 1, strat_var = FOOD
        column groups (stats):
          identifiers  : BIN_MID, FOOD
          counts       : obs_n, obs_n_blq, obs_prop_blq
          sim BLQ      : sim_prop_blq_low, sim_prop_blq_med, sim_prop_blq_hi  [std-only]
          std observed : obs_low, obs_med, obs_hi
          std simulated: sim_low_low, sim_low_med, sim_low_hi, sim_med_low, sim_med_med, sim_med_hi, sim_hi_low, sim_hi_med, sim_hi_hi
          pc observed  : pc_obs_low, pc_obs_med, pc_obs_hi
          pc simulated : pc_sim_low_low, pc_sim_low_med, pc_sim_low_hi, pc_sim_med_low, pc_sim_med_med, pc_sim_med_hi, pc_sim_hi_low, pc_sim_hi_med, pc_sim_hi_hi
          metadata     : ci, pi_low, pi_hi
      
        Use `x$stats` and `x$obs` for the underlying data.frames.

