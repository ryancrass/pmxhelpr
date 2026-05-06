# doseprop_stats print/summary output is stable (snapshot)

    Code
      print(stats)
    Output
      <doseprop_stats>
        stats: 2 rows x 10 columns
        obs:   72 rows
        config: metric_var = PPTESTCD, exp_var = PPORRES, dose_var = DOSE, ci = 0.9, method = normal
      
        stats body:
        Intercept StandardError  CI Power   LCL  UCL Proportional
      1      4.04        0.0663 90% 0.997 0.888 1.11         TRUE
      2      1.09        0.0616 90% 1.070 0.967 1.17         TRUE
                                 PowerCI    Interpretation   PPTESTCD
      1 Power: 0.997 (90% CI 0.888-1.11) Dose-proportional aucinf.obs
      2  Power: 1.07 (90% CI 0.967-1.17) Dose-proportional       cmax
      
        Use `x$obs` for the observation overlay.

---

    Code
      summary(stats)
    Output
      <doseprop_stats>
        stats: 2 rows x 10 columns
        obs:   72 rows
        config: metric_var = PPTESTCD, exp_var = PPORRES, dose_var = DOSE, ci = 0.9, method = normal
      
        per-metric:
          aucinf.obs: Power: 0.997 (90% CI 0.888-1.11) -- Dose-proportional
          cmax: Power: 1.07 (90% CI 0.967-1.17) -- Dose-proportional

