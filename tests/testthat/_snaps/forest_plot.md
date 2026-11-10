# summary.forest_stats output is stable (snapshot)

    Code
      summary(out)
    Output
      <forest_stats>
        stats: 8 rows x 13 columns
        config: metric_name_var = metric, metric_value_var = value, cov_name_var = cov_name, cov_level_var = cov_level, ci = 0.9, sigdigits = 3, cov_name_ref = Reference, cov_level_ref = c(FOOD = "Fasted", WTBL = "70 kg")
      
        per metric:
          [AUCRATIO]  n = 500  Reference | FOOD: Fasted*, Fed | WTBL: 50 kg, 60 kg, 70 kg*, 80 kg, 90 kg
        * reference copy in a covariate panel (ref_layout = "distributed")

