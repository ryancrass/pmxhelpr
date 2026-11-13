# summary.vpc_stats output is stable (snapshot)

    Code
      summary(out)
    Output
      <vpc_stats>
        stats: 38 rows x 35 columns
        obs:   501 rows
        config: n_replicates = 5, ci = 0.9, pi = c(0.05, 0.95), mode = auto, loq = 1, strat_var = FOOD
        column groups (stats):
          identifiers  : BIN_MID, FOOD
          counts       : obs_n, obs_n_blq, obs_prop_blq
          sim BLQ      : sim_prop_blq_low, sim_prop_blq_med, sim_prop_blq_hi  [std-only]
          std observed : obs_low, obs_med, obs_hi
          std simulated: sim_low_low, sim_low_med, sim_low_hi, sim_med_low, sim_med_med, sim_med_hi, sim_hi_low, sim_hi_med, sim_hi_hi
          pc observed  : pc_obs_low, pc_obs_med, pc_obs_hi
          pc simulated : pc_sim_low_low, pc_sim_low_med, pc_sim_low_hi, pc_sim_med_low, pc_sim_med_med, pc_sim_med_hi, pc_sim_hi_low, pc_sim_hi_med, pc_sim_hi_hi
          metadata     : ci, pi_low, pi_hi
        CI coverage (observed quantile inside its 90% simulated CI):
          FOOD     quantile  std        pc         cens
          0        5%         6/11 55%   8/16 50%
          0        50%        6/14 43%   8/16 50%  15/19 79%
          0        95%       12/16 75%  12/16 75%
          1        5%        11/14 79%  14/16 88%
          1        50%       12/14 86%  15/16 94%  17/19 89%
          1        95%        9/16 56%   9/16 56%
          Overall  5%        17/25 68%  22/32 69%
          Overall  50%       18/28 64%  23/32 72%  32/38 84%
          Overall  95%       21/32 66%  21/32 66%
        PI coverage (observations outside the 5%-95% simulated PI; expected 5% below, 5% above):
          FOOD     std below  std above   pc below   pc above
          0        25/570 4%  30/570  5%  24/412 6%  31/412  8%
          1         6/114 5%   8/114  7%    6/89 7%   10/89 11%
          Overall  31/684 5%  38/684  6%  30/501 6%  41/501  8%
      
        Use `x$stats` and `x$obs` for the underlying data.frames.

