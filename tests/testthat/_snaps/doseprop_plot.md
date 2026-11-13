# doseprop_stats print/summary output is stable (snapshot)

    Code
      print(stats)
    Output
      <doseprop_stats>
        stats: 2 rows x 10 columns
        obs:   72 rows
        config: metric_name_var = PPTESTCD, metric_value_var = PPORRES, dose_var = DOSE, ci = 0.9, method = normal
      
        stats body:
        Intercept StandardError  CI Power  LCL  UCL Proportional
      1     2.530        0.0663 90%  1.32 1.22 1.43        FALSE
      2    -0.465        0.0603 90%  1.40 1.30 1.49        FALSE
                               PowerCI                 Interpretation   PPTESTCD
      1 Power: 1.32 (90% CI 1.22-1.43) Greater than dose-proportional aucinf.obs
      2 Power: 1.40 (90% CI 1.30-1.49) Greater than dose-proportional       cmax
      
        Use `x$obs` for the observation overlay.

---

    Code
      summary(stats)
    Output
      <doseprop_stats>
        stats: 2 rows x 10 columns
        obs:   72 rows
        config: metric_name_var = PPTESTCD, metric_value_var = PPORRES, dose_var = DOSE, ci = 0.9, method = normal
      
        per-metric:
          aucinf.obs: Power: 1.32 (90% CI 1.22-1.43) -- Greater than dose-proportional
          cmax: Power: 1.40 (90% CI 1.30-1.49) -- Greater than dose-proportional

