# print.pmx_theme output is stable (snapshot)

    Code
      print(plot_dvtime_theme())
    Output
      <plot_dvtime_theme>
        obs_point     <pmx_point>: shape = 1, size = 0.75, alpha = 0.5
        obs_line      <pmx_line>: linewidth = 0.5, linetype = 1, alpha = 0.5
        cent_point    <pmx_point>: shape = 16, size = 1.25, alpha = 0
        cent_line     <pmx_line>: linewidth = 0.75, linetype = 1, alpha = 1
        cent_errorbar <pmx_errorbar>: linewidth = 0.75, linetype = 1, alpha = 1, width = NULL
        ref_line      <pmx_line>: linewidth = 0.5, linetype = 2, alpha = 1
        loq_line      <pmx_line>: linewidth = 0.5, linetype = 2, alpha = 1

---

    Code
      print(plot_vpc_theme())
    Output
      <plot_vpc_theme>
        obs_point       <pmx_point>: shape = 1, size = 1, alpha = 0.7, color = #0000FF
        obs_median_line <pmx_line>: linewidth = 1, linetype = solid, color = #FF0000
        obs_pi_line     <pmx_line>: linewidth = 0.5, linetype = dashed, color = #0000FF
        sim_pi_line     <pmx_line>: linewidth = 1, linetype = dotted, color = #000000
        sim_pi_ci       <pmx_ribbon>: fill = #0000FF, alpha = 0.15
        sim_pi_area     <pmx_ribbon>: fill = #0000FF, alpha = 0.15
        sim_median_line <pmx_line>: linewidth = 1, linetype = dashed, color = #000000
        sim_median_ci   <pmx_ribbon>: fill = #FF0000, alpha = 0.3
        loq_line        <pmx_line>: linewidth = 0.5, linetype = dashed, color = #990000

