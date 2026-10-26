# summary.forest_stats output is stable (snapshot)

    Code
      summary(out)
    Output
      <forest_stats>
        stats: 8 rows x 12 columns
        config: metric_name_var = metric, cov_name_var = cov_name, cov_level_var = cov_level, ci = 0.9, sigdigits = 3, cov_name_ref = Reference, cov_level_ref = c(FOOD = "Fasted", WTBL = "70 kg")
      
        per-row:
          [AUCRATIO] Reference  (consolidated)
          [AUCRATIO] FOOD: Fasted  (distributed)
          [AUCRATIO] FOOD: Fed
          [AUCRATIO] WTBL: 50 kg
          [AUCRATIO] WTBL: 60 kg
          [AUCRATIO] WTBL: 70 kg  (distributed)
          [AUCRATIO] WTBL: 80 kg
          [AUCRATIO] WTBL: 90 kg

