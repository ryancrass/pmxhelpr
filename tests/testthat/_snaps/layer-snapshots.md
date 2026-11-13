# plot_dvtime: colored mean +/- SD, per-row LLOQ imputation, log y

    Code
      layer_summary(p)
    Output
      layer 1: GeomHline / StatIdentity  rows=1
        colour=black alpha=1 linetype=dashed linewidth=0.5
        yintercept: [0, 0] mean=0
      layer 2: GeomPoint / StatIdentity  rows=684
        colour=#F8766D,#A3A500,#00BF7D,#00B0F6,#E76BF3 fill=NA alpha=0.5 shape=1 size=0.75
        x: [0, 168.2] mean=40.05
        y: [-0.301, 3.531] mean=1.291
      layer 3: GeomPoint / StatSummary  rows=95
        colour=#F8766D,#A3A500,#00BF7D,#00B0F6,#E76BF3 fill=NA alpha=0 shape=16 size=1.25
        x: [0, 168] mean=40.05
        y: [-0.301, 3.266] mean=1.258
      layer 4: GeomLine / StatSummary  rows=95
        colour=#F8766D,#A3A500,#00BF7D,#00B0F6,#E76BF3 alpha=1 linetype=solid linewidth=0.75
        x: [0, 168] mean=40.05
        y: [-0.301, 3.266] mean=1.258
      layer 5: GeomErrorbar / StatSummary  rows=95
        colour=#F8766D,#A3A500,#00BF7D,#00B0F6,#E76BF3 alpha=1 linetype=1 linewidth=0.75
        x: [0, 168] mean=40.05
        ymin: [-0.4936, 3.122] mean=1.024
        ymax: [-0.301, 3.438] mean=1.493
        xmin: [-2.1, 165.9] mean=37.95
        xmax: [2.1, 170.1] mean=42.15
      scales: x=identity y=log-10
      facet: FacetNull panels=1
      label x: TIME
      label y: DV
      label caption: Thick lines are the geo. mean + geo. SD error bars
      All BLQ observations are imputed to 1/2 LLOQ

# plot_dvtime: median + IQR, dose-normalized, reference line

    Code
      layer_summary(p)
    Output
      layer 1: GeomHline / StatIdentity  rows=1
        colour=black alpha=1 linetype=dashed linewidth=0.5
        yintercept: [1, 1] mean=1
      layer 2: GeomPoint / StatIdentity  rows=684
        colour=black fill=NA alpha=0.5 shape=1 size=0.75
        x: [0, 168.2] mean=40.05
        y: [0.003425, 8.493] mean=1.686
      layer 3: GeomPoint / StatSummary  rows=16
        colour=black fill=NA alpha=0 shape=16 size=1.25
        x: [0.5, 120] mean=28.06
        y: [0.008162, 3.276] mean=1.255
      layer 4: GeomLine / StatSummary  rows=16
        colour=black alpha=1 linetype=solid linewidth=0.75
        x: [0.5, 120] mean=28.06
        y: [0.008162, 3.276] mean=1.255
      layer 5: GeomErrorbar / StatSummary  rows=16
        colour=black alpha=1 linetype=1 linewidth=0.75
        x: [0.5, 120] mean=28.06
        ymin: [0.005131, 2.086] mean=0.7626
        ymax: [0.02558, 4.513] mean=1.98
        xmin: [-1.6, 117.9] mean=25.96
        xmax: [2.6, 122.1] mean=30.16
      scales: x=identity y=identity
      facet: FacetNull panels=1
      label x: TIME
      label y: DV
      label caption: Thick lines are the median + IQR error bars

# plot_dvconc: colored loess and linear trends with loess SE

    Code
      layer_summary(p)
    Output
      layer 1: GeomSmooth / StatSmooth  rows=400
        colour=#F8766D,#A3A500,#00BF7D,#00B0F6,#E76BF3 fill=#F8766D,#A3A500,#00BF7D,#00B0F6,#E76BF3 alpha=0.4 linetype=solid linewidth=1
        x: [0, 3397] mean=578.1
        y: [41.2, 98.06] mean=72.92
        ymin: [27.61, 95.64] mean=60.88
        ymax: [54.79, 133.7] mean=84.96
      layer 2: GeomSmooth / StatSmooth  rows=400
        colour=#F8766D,#A3A500,#00BF7D,#00B0F6,#E76BF3 fill=#F8766D,#A3A500,#00BF7D,#00B0F6,#E76BF3 alpha=0.4 linetype=dashed linewidth=1
        x: [0, 3397] mean=578.1
        y: [46.86, 94.44] mean=71.15
      layer 3: GeomPoint / StatIdentity  rows=684
        colour=#F8766D,#A3A500,#00BF7D,#00B0F6,#E76BF3 fill=NA alpha=0.5 shape=1 size=1.25
        x: [0, 3397] mean=210.4
        y: [7.839, 100] mean=73.88
      scales: x=identity y=identity
      facet: FacetNull panels=1
      label x: IDV
      label y: DV
      label caption: Points are observations
      LOESS fit with 95% CI and linear fit overlaid

# plot_gof: DV/IPRED/PRED overlays, median + IQR, LLOQ method 1

    Code
      layer_summary(p)
    Output
      layer 1: GeomHline / StatIdentity  rows=1
        colour=black alpha=1 linetype=dashed linewidth=0.5
        yintercept: [1, 1] mean=1
      layer 2: GeomPoint / StatIdentity  rows=684
        colour=darkgrey fill=NA alpha=0.5 shape=1 size=0.75
        x: [0, 168.2] mean=40.05
        y: [0, 3397] mean=210.5
      layer 3: GeomPoint / StatSummary  rows=19
        colour=blue fill=NA alpha=0 shape=16 size=1.25
        x: [0, 168] mean=40.05
        y: [0, 387.9] mean=131
      layer 4: GeomLine / StatSummary  rows=19
        colour=blue alpha=1 linetype=solid linewidth=0.75
        x: [0, 168] mean=40.05
        y: [0, 387.9] mean=131
      layer 5: GeomErrorbar / StatSummary  rows=19
        colour=blue alpha=1 linetype=1 linewidth=0.75
        x: [0, 168] mean=40.05
        ymin: [0, 141.8] mean=38.64
        ymax: [0, 694.4] mean=249.5
        xmin: [-2.1, 165.9] mean=37.95
        xmax: [2.1, 170.1] mean=42.15
      layer 6: GeomPoint / StatSummary  rows=19
        colour=green fill=NA alpha=0 shape=16 size=1.25
        x: [0, 168] mean=40.05
        y: [0, 379.2] mean=134.4
      layer 7: GeomLine / StatSummary  rows=19
        colour=green alpha=1 linetype=solid linewidth=0.75
        x: [0, 168] mean=40.05
        y: [0, 379.2] mean=134.4
      layer 8: GeomPoint / StatSummary  rows=19
        colour=red fill=NA alpha=0 shape=16 size=1.25
        x: [0, 168] mean=40.05
        y: [0, 316.8] mean=111.6
      layer 9: GeomLine / StatSummary  rows=19
        colour=red alpha=1 linetype=solid linewidth=0.75
        x: [0, 168] mean=40.05
        y: [0, 316.8] mean=111.6
      scales: x=identity y=identity
      facet: FacetNull panels=1
      label x: TIME
      label y: DV
      label caption: Thick lines are the median + IQR error bars
      Post-dose BLQ observations are imputed to 1/2 LLOQ

# plot_doseprop: power-model fits for AUC and Cmax

    Code
      layer_summary(p)
    Output
      layer 1: GeomPoint / StatIdentity  rows=60
        colour=black fill=NA alpha=0.7 shape=1 size=2
        x: [1, 2.602] mean=1.92
        y: [0.7868, 4.551] mean=3.012
      layer 2: GeomSmooth / StatSmooth  rows=160
        colour=black fill=lightgrey alpha=0.4 linetype=solid linewidth=1
        x: [1, 2.602] mean=1.801
        y: [1.171, 4.468] mean=2.851
        ymin: [1.062, 4.403] mean=2.788
        ymax: [1.28, 4.533] mean=2.915
      scales: x=log-10 y=log-10
      facet: FacetWrap(label) panels=2
      label x: Dose
      label y: Exposure

# plot_forest: reference row distributed into each covariate panel

    Code
      layer_summary(p)
    Output
      layer 1: GeomBlank / StatIdentity  rows=7
        x: [0.826, 2.316] mean=1.209
        y: [1, 5] mean=2.571
      layer 2: GeomVline / StatIdentity  rows=2
        colour=grey30 alpha=1 linetype=dashed linewidth=0.6
        xintercept: [1, 1] mean=1
      layer 3: GeomErrorbar / StatIdentity  rows=5
        colour=black alpha=1 linetype=solid linewidth=0.6
        x: [0.826, 2.316] mean=1.292
        y: [1, 5] mean=2.6
        ymin: [1, 5] mean=2.6
        ymax: [1, 5] mean=2.6
        xmin: [0.7749, 2.004] mean=1.184
        xmax: [0.8798, 2.649] mean=1.408
      layer 4: GeomPoint / StatIdentity  rows=5
        colour=black fill=NA alpha=1 shape=16 size=2.2
        x: [0.826, 2.316] mean=1.292
        y: [1, 5] mean=2.6
      layer 5: GeomPoint / StatIdentity  rows=2
        colour=black fill=white alpha=1 shape=21 size=2.2
        x: [1, 1] mean=1
        y: [2, 3] mean=2.5
      scales: x=identity y=discrete
      facet: FacetGrid(cov_name) panels=2
      label x: AUCRATIO

# plot_forest: consolidated reference panel with a reference band

    Code
      layer_summary(p)
    Output
      layer 1: GeomBlank / StatIdentity  rows=6
        x: [0.826, 2.316] mean=1.244
        y: [1, 4] mean=2
      layer 2: GeomRect / StatIdentity  rows=3
        colour=NA fill=grey80 alpha=0.3 linetype=1 linewidth=0.5
        xmin: [0.8, 0.8] mean=0.8
        xmax: [1.25, 1.25] mean=1.25
      layer 3: GeomVline / StatIdentity  rows=3
        colour=grey30 alpha=1 linetype=dashed linewidth=0.6
        xintercept: [1, 1] mean=1
      layer 4: GeomErrorbar / StatIdentity  rows=5
        colour=black alpha=1 linetype=solid linewidth=0.6
        x: [0.826, 2.316] mean=1.292
        y: [1, 4] mean=2.2
        ymin: [1, 4] mean=2.2
        ymax: [1, 4] mean=2.2
        xmin: [0.7749, 2.004] mean=1.184
        xmax: [0.8798, 2.649] mean=1.408
      layer 5: GeomPoint / StatIdentity  rows=5
        colour=black fill=NA alpha=1 shape=16 size=2.2
        x: [0.826, 2.316] mean=1.292
        y: [1, 4] mean=2.2
      layer 6: GeomPoint / StatIdentity  rows=1
        colour=black fill=white alpha=1 shape=21 size=2.2
        x: [1, 1] mean=1
        y: [1, 1] mean=1
      scales: x=identity y=discrete
      facet: FacetGrid(cov_name) panels=3
      label x: AUCRATIO

# plot_vpc_cont: standard VPC stratified by FOOD with per-row LLOQ

    Code
      layer_summary(p)
    Output
      layer 1: GeomRibbon / StatIdentity  rows=38
        colour=NA fill=#0000FF alpha=0.15 linetype=1 linewidth=0.5
        x: [0, 168] mean=40.05
        y: [0, 120.1] mean=17.4
        ymin: [0, 120.1] mean=17.4
        ymax: [0, 321.3] mean=56.1
      layer 2: GeomRibbon / StatIdentity  rows=38
        colour=NA fill=#0000FF alpha=0.15 linetype=1 linewidth=0.5
        x: [0, 168] mean=40.05
        y: [0, 1359] mean=353.5
        ymin: [0, 1359] mean=353.5
        ymax: [0, 2755] mean=680.5
      layer 3: GeomRibbon / StatIdentity  rows=38
        colour=NA fill=#FF0000 alpha=0.3 linetype=1 linewidth=0.5
        x: [0, 168] mean=40.05
        y: [0, 236.6] mean=75.94
        ymin: [0, 236.6] mean=75.94
        ymax: [0, 518] mean=155.2
      layer 4: GeomLine / StatIdentity  rows=38
        colour=#FF0000 alpha=NA linetype=solid linewidth=1
        x: [0, 168] mean=40.05
        y: [1.97, 420.9] mean=185.5
      layer 5: GeomLine / StatIdentity  rows=38
        colour=#0000FF alpha=NA linetype=dashed linewidth=0.5
        x: [0, 168] mean=40.05
        y: [1.601, 329.8] mean=64.65
      layer 6: GeomLine / StatIdentity  rows=38
        colour=#0000FF alpha=NA linetype=dashed linewidth=0.5
        x: [0, 168] mean=40.05
        y: [1.28, 2288] mean=601.7
      layer 7: GeomHline / StatIdentity  rows=2
        colour=#990000 alpha=NA linetype=dashed linewidth=0.5
        yintercept: [1, 1] mean=1
      layer 8: GeomPoint / StatIdentity  rows=501
        colour=#0000FF fill=NA alpha=0.7 shape=1 size=1
        x: [0.31, 120.2] mean=18.51
        y: [1.02, 3397] mean=287.3
      scales: x=identity y=identity
      facet: FacetWrap(FOOD) panels=2
      label x: TIME
      label y: DV
      label caption: Replicates = 10

# plot_vpc_cont: prediction-corrected VPC stratified by FOOD

    Code
      layer_summary(p)
    Output
      layer 1: GeomRibbon / StatIdentity  rows=38
        colour=NA fill=#0000FF alpha=0.15 linetype=1 linewidth=0.5
        x: [0, 168] mean=40.05
        y: [0.1911, 120] mean=33.51
        ymin: [0.1911, 120] mean=33.51
        ymax: [0.5458, 324.5] mean=89.94
      layer 2: GeomRibbon / StatIdentity  rows=38
        colour=NA fill=#0000FF alpha=0.15 linetype=1 linewidth=0.5
        x: [0, 168] mean=40.05
        y: [0.8574, 498.4] mean=209.6
        ymin: [0.8574, 498.4] mean=209.6
        ymax: [2.919, 995.4] mean=382.3
      layer 3: GeomRibbon / StatIdentity  rows=38
        colour=NA fill=#FF0000 alpha=0.3 linetype=1 linewidth=0.5
        x: [0, 168] mean=40.05
        y: [0.293, 236.7] mean=95.03
        ymin: [0.293, 236.7] mean=95.03
        ymax: [1.535, 520] mean=176.5
      layer 4: GeomLine / StatIdentity  rows=38
        colour=#FF0000 alpha=NA linetype=solid linewidth=1
        x: [0, 168] mean=40.05
        y: [0.3722, 415.6] mean=152.5
      layer 5: GeomLine / StatIdentity  rows=38
        colour=#0000FF alpha=NA linetype=dashed linewidth=0.5
        x: [0, 168] mean=40.05
        y: [0.2433, 329.7] mean=74.6
      layer 6: GeomLine / StatIdentity  rows=38
        colour=#0000FF alpha=NA linetype=dashed linewidth=0.5
        x: [0, 168] mean=40.05
        y: [1.015, 762.1] mean=292.5
      layer 7: GeomPoint / StatIdentity  rows=501
        colour=#0000FF fill=NA alpha=0.7 shape=1 size=1
        x: [0.31, 120.2] mean=18.51
        y: [0.2171, 942.6] mean=168.3
      scales: x=identity y=identity
      facet: FacetWrap(FOOD) panels=2
      label x: TIME
      label y: Prediction-corrected DV
      label caption: Replicates = 10

# plot_vpc_cens: BLQ-proportion VPC

    Code
      layer_summary(p)
    Output
      layer 1: GeomRibbon / StatIdentity  rows=19
        colour=NA fill=#FF0000 alpha=0.3 linetype=1 linewidth=0.5
        x: [0, 168] mean=40.05
        y: [0, 1] mean=0.2349
        ymin: [0, 1] mean=0.2349
        ymax: [0, 1] mean=0.2857
      layer 2: GeomLine / StatIdentity  rows=19
        colour=#FF0000 alpha=NA linetype=solid linewidth=1
        x: [0, 168] mean=40.05
        y: [0, 1] mean=0.2675
      layer 3: GeomPoint / StatIdentity  rows=19
        colour=#FF0000 fill=NA alpha=0.7 shape=1 size=1
        x: [0, 168] mean=40.05
        y: [0, 1] mean=0.2675
      scales: x=identity y=identity
      facet: FacetNull panels=1
      label x: TIME
      label y: Proportion BLQ
      label caption: Replicates = 10

# plot_vpc_legend: default legend panel

    Code
      layer_summary(plot_vpc_legend())
    Output
      layer 1: GeomPoint / StatIdentity  rows=1
        colour=#0000FF fill=NA alpha=NA shape=1 size=1
      layer 2: GeomLine / StatIdentity  rows=1
        colour=#FF0000 alpha=NA linetype=solid linewidth=1
      layer 3: GeomLine / StatIdentity  rows=1
        colour=#0000FF alpha=NA linetype=dashed linewidth=0.5
      layer 4: GeomRect / StatIdentity  rows=1
        colour=NA fill=#FF0000 alpha=0.3 linetype=1 linewidth=0.5
      layer 5: GeomRect / StatIdentity  rows=1
        colour=NA fill=#0000FF alpha=0.15 linetype=1 linewidth=0.5
      scales: x=identity y=identity
      facet: FacetNull panels=1
      label x: x
      label y: y

