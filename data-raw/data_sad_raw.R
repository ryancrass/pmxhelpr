#####Load the development package (functions + inst/models via pkgload)#####
devtools::load_all()

#####Read model files#####

##Load Model Files
pkmod <- mrgsolve::mread(system.file(
  "models",
  "pkmodel.cpp",
  package = "pmxhelpr"
))
pkmod_th <- mrgsolve::param(pkmod)@data |> as.data.frame()
pdmod <- mrgsolve::mread(system.file(
  "models",
  "pdmodel.cpp",
  package = "pmxhelpr"
))
pdmod_th <- mrgsolve::param(pdmod)


#####Create `data_sad` internal NONMEM analysis-ready dataset for a SAD study#####

#Define Study Conditions: Doses, Sample Size, LLOQ
doses <- c(10, 50, 100, 100, 200, 400) #Dose levels in mg
n_doses <- length(doses)
n_subj_per_dose <- 6 #Standard SAD Cohort: 6 active, 2 placebo
n_subj_total <- n_doses * n_subj_per_dose
lloq <- 1 #ng/mL
times <- c(
  mrgsolve::tgrid(0, 2, 0.5),
  mrgsolve::tgrid(3, 5, 1),
  mrgsolve::tgrid(8, 16, 4),
  mrgsolve::tgrid(24, 36, 12),
  mrgsolve::tgrid(48, 168, 24)
)


##Create Dosing Data
dose_data <- data.frame(id = 1:n_subj_total, time = 0, amt = doses) |>
  dplyr::arrange(amt) |>
  dplyr::mutate(id = 1, evid = 1, cmt = 1, id = cumsum(id)) |>
  dplyr::group_by(amt) |>
  dplyr::mutate(
    tmp = 1,
    tmp = cumsum(tmp),
    food = ifelse(tmp > n_subj_per_dose, 1, 0)
  ) |>
  dplyr::select(-tmp) |>
  dplyr::ungroup()


##Create Covariate Data
withr::with_seed(
  seed = 123456789,

  cov_data <- data.frame(
    id = 1:n_subj_total,
    sexf = sample(c(0, 1), n_subj_total, replace = T),
    race = sample(
      c(1, 2, 3),
      n_subj_total,
      replace = T,
      prob = c(0.6, 0.2, 0.2)
    ),
    agebl = sample(c(18:35), n_subj_total, replace = T),
    wtbl = round(
      sample(rnorm(1000, 75, 15), n_subj_total, replace = F),
      digits = 1
    ),
    scrbl = round(
      sample(rnorm(1000, 0.9, 0.2), n_subj_total, replace = F),
      digits = 2
    )
  ) |>
    dplyr::mutate(
      crclbl = signif(
        ((140 - agebl) / scrbl) * (wtbl / 72) * ifelse(sexf == 1, 0.85, 1),
        digits = 3
      )
    )
)


##Join Dosing and Covariate Data
sim_input_data <- dplyr::left_join(dose_data, cov_data)
colnames(sim_input_data) <- toupper(colnames(sim_input_data))


##Run PK Simulation
withr::with_seed(
  seed = 123456789,
  simout <- mrgsolve::mrgsim_df(
    x = pkmod,
    data = sim_input_data,
    tgrid = times,
    obsonly = TRUE
  )
)


##Create Concentration Data
conc_data <- simout |>
  dplyr::select(ID, NTIME = TIME, DV) |>
  dplyr::mutate(
    DV = round(DV, digits = 2),
    DV = ifelse(DV < lloq, NA_real_, DV),
    LDV = round(log(DV), digits = 4),
    MDV = as.numeric(is.na(DV)),
    CMT = 2,
    EVID = 0,
    BLQ = ifelse(NTIME == 0, -1, MDV),
    LLOQ = lloq
  ) |>
  dplyr::rename(ODV = DV)


##Assemble Complete PK Dataset
withr::with_seed(
  seed = 123456789,

  data_sad_pk <-
    dplyr::bind_rows(
      dplyr::rename(sim_input_data, NTIME = TIME),
      conc_data
    ) |>
    dplyr::mutate(
      DOSE = AMT,
      TIME = dplyr::case_when(
        NTIME == 0 ~ 0,
        .default = round(jitter(NTIME, factor = 2), digits = 2)
      ),
      NDAY = floor(NTIME / 24) + 1
    ) |>
    dplyr::group_by(ID) |>
    tidyr::fill(DOSE, FOOD, SEXF:CRCLBL) |>
    dplyr::ungroup() |>
    dplyr::arrange(ID, TIME, NTIME, EVID) |>
    dplyr::mutate(
      LINE = 1,
      LINE = cumsum(LINE),
      USUBJID = paste0("STUDYNUM-SITENUM-", ID),
      PART = ifelse(FOOD == 0, "Part 1-SAD", "Part 2-FE")
    ) |>
    dplyr::select(
      LINE,
      ID,
      TIME,
      NTIME,
      NDAY,
      DOSE,
      AMT,
      EVID,
      ODV,
      LDV,
      CMT,
      MDV,
      BLQ,
      LLOQ,
      FOOD,
      SEXF,
      RACE,
      AGEBL,
      WTBL,
      SCRBL,
      CRCLBL,
      USUBJID,
      PART
    )
)


##Run PD Simulation
withr::with_seed(
  seed = 123456789,
  pdsimout <- df_mrgsim_replicate(
    data_sad_pk,
    pdmod,
    replicates = 1,
    dv_var = "ODV",
    carry_out = "LINE"
  )
)

pd_data <- data_sad_pk |>
  dplyr::filter(CMT == 2) |>
  dplyr::left_join(dplyr::select(pdsimout, LINE, IPRED, IPREDPK)) |>
  dplyr::mutate(
    CMT = 3,
    CONC = ifelse(is.na(ODV), 0, ODV),
    ODV = IPRED,
    LDV = IPRED,
    CFB = ODV - 100
  ) |>
  dplyr::select(-IPRED, -IPREDPK)

data_sad <- dplyr::bind_rows(data_sad_pk, pd_data) |>
  dplyr::arrange(ID, TIME, CMT) |>
  dplyr::select(ID:LDV, CFB, CONC, everything())


usethis::use_data(data_sad, overwrite = TRUE)


#####Create `data_sad_nca` internal dataset of NCA parameters for `data_sad`#####

##Set NCA options
PKNCA::PKNCA.options(
  conc.blq = list(
    "first" = "keep",
    "middle" = unique(data_sad$LLOQ[!is.na(data_sad$LLOQ)]),
    "last" = "drop"
  ),
  allow.tmax.in.half.life = FALSE,
  min.hl.r.squared = 0.9
)

##Calculation Intervals and Requested Parameters
intervals <-
  data.frame(
    start = 0,
    end = Inf,
    auclast = TRUE,
    aucinf.obs = TRUE,
    aucpext.obs = TRUE,
    half.life = TRUE,
    cmax = TRUE,
    vz.obs = TRUE,
    cl.obs = TRUE
  )

#Impute BLQ concentrations to 0 (PKNCA formatting)
data_sad_nca_input <- data_sad |>
  dplyr::filter(CMT %in% c(1, 2)) |>
  dplyr::mutate(CONC = ifelse(is.na(ODV), 0, ODV), AMT = AMT / 1000) #convert from mg to ug. Concentration in ng/mL = ug/L


#Build PKNCA objects for concentration and dose including relevant strata
conc_obj <- PKNCA::PKNCAconc(
  dplyr::filter(data_sad_nca_input, EVID == 0),
  CONC ~ TIME | ID + DOSE + PART
)
dose_obj <- PKNCA::PKNCAdose(
  dplyr::filter(data_sad_nca_input, EVID == 1),
  AMT ~ TIME | ID + PART
)
nca_data_obj <- PKNCA::PKNCAdata(conc_obj, dose_obj, intervals = intervals)
nca_results_obj <- as.data.frame(PKNCA::pk.nca(nca_data_obj))

#Add Units
data_sad_nca <- nca_results_obj |>
  dplyr::mutate(units_dose = "mg", units_conc = "ng/mL", units_time = "hours")

usethis::use_data(data_sad_nca, overwrite = TRUE)


#####Create `data_sad_pkfit` internal dataset of PK model outputs for `data_sad` using `model`

withr::with_seed(
  seed = 987654321,
  pkfit <- df_mrgsim_replicate(
    data_sad_pk,
    pkmod,
    replicates = 1,
    dv_var = "ODV",
    carry_out = "LINE"
  )
)

data_sad_pkfit <- data_sad_pk |>
  dplyr::left_join(dplyr::select(pkfit, LINE, IPRED, PRED))

usethis::use_data(data_sad_pkfit, overwrite = TRUE)


#####Create `data_sad_pkforest_sim` and `data_sad_pkforest` internal datasets#####
## Forest plot replicate simulation: a typical individual (IIV zeroed) under
## each covariate scenario receives 200 mg QD to steady state. Each replicate
## runs all scenarios on its own parameter set, drawn by parametric bootstrap
## from a multivariate normal defined by an assumed variance-covariance
## matrix on the model's log typical values and covariate coefficients
## (the structure of sampling from a model's covariance step).

n_rep <- 500

## Scenario design table: one row per covariate condition, with the labels
## `df_forest()` reads from `cov_var` / `cov_val`.
forest_scn <- data.frame(
  ID = 1:6,
  WTBL = c(70, 70, 50, 60, 80, 90),
  FOOD = c(0, 1, 0, 0, 0, 0),
  cov_var = c("Reference", "FOOD", rep("WTBL", 4)),
  cov_val = c("Reference", "Fed", "50 kg", "60 kg", "80 kg", "90 kg")
)

## Parameter uncertainty: log typical values + natural-scale covariate
## coefficients, assumed SEs and correlations -> variance-covariance matrix.
tv_names <- c("TVCL", "TVVC", "TVKA", "TVQ", "TVVP")
cov_names <- c("WT_CL", "WT_VC", "WT_Q", "WT_VP", "FOOD_KA", "FOOD_F1")
theta <- c(log(unlist(pkmod_th[tv_names])), unlist(pkmod_th[cov_names]))
theta_se <- c(
  rep(0.10, length(tv_names)),
  WT_CL = 0.15, WT_VC = 0.15, WT_Q = 0.15, WT_VP = 0.15,
  FOOD_KA = 0.10, FOOD_F1 = 0.20
)
theta_corr <- diag(length(theta))
dimnames(theta_corr) <- list(names(theta), names(theta))
theta_corr["TVCL", "TVVC"] <- theta_corr["TVVC", "TVCL"] <- 0.3
theta_corr["TVQ", "TVVP"] <- theta_corr["TVVP", "TVQ"] <- 0.3
theta_corr["FOOD_KA", "FOOD_F1"] <- theta_corr["FOOD_F1", "FOOD_KA"] <- -0.2
theta_vcov <- (theta_se %o% theta_se) * theta_corr

withr::with_seed(
  seed = 987654321,
  forest_idata <- MASS::mvrnorm(n_rep, mu = theta, Sigma = theta_vcov) |>
    as.data.frame()
)
forest_idata[tv_names] <- exp(forest_idata[tv_names])
forest_idata <- dplyr::mutate(forest_idata, SIM = dplyr::row_number(), .before = 1)

## Simulation input: every scenario x replicate is its own mrgsolve `ID`
## (so the dose records never collide); the replicate `SIM` and scenario
## `OID` ride along, and `idata` repeats each replicate's parameter set
## under its scenario IDs.
forest_sim <- tidyr::expand_grid(OID = forest_scn$ID, SIM = 1:n_rep) |>
  dplyr::mutate(ID = dplyr::row_number(), .before = 1) |>
  dplyr::left_join(dplyr::select(forest_scn, OID = ID, WT = WTBL, FOOD), by = "OID") |>
  dplyr::mutate(
    TIME = 0, AMT = 200, DOSE = 200, CMT = 1, EVID = 1, II = 24, SS = 1
  )

forest_idata_sim <- forest_sim |>
  dplyr::select(ID, SIM) |>
  dplyr::left_join(forest_idata, by = "SIM") |>
  dplyr::select(-SIM)

forest_grid <- mrgsolve::tgrid(start = 0, end = 24, delta = 0.1)

withr::with_seed(
  seed = 987654321,
  pkforest_fit <- mrgsolve::mrgsim_df(
    mrgsolve::zero_re(pkmod),
    data = forest_sim,
    idata = forest_idata_sim,
    tgrid = forest_grid,
    carry_out = c("SIM", "OID", "WT", "FOOD", "DOSE"),
    obsonly = TRUE
  )
)

## Raw replicate output: exposure metrics over the steady-state dosing
## interval, one row per scenario x replicate.
data_sad_pkforest_sim <- pkforest_fit |>
  dplyr::select(-ID) |>
  dplyr::rename(ID = OID, WTBL = WT) |>
  dplyr::group_by(ID, SIM, WTBL, FOOD, DOSE) |>
  dplyr::summarise(
    CMAX = max(IPRED),
    CMIN = dplyr::last(IPRED),
    AUC = PKNCA::pk.calc.auc.last(IPRED, TIME),
    CAVG = AUC / 24,
    .groups = "drop"
  ) |>
  dplyr::arrange(ID, SIM) |>
  as.data.frame()

usethis::use_data(data_sad_pkforest_sim, overwrite = TRUE)

## Long replicate draws: within-replicate test/reference ratios against the
## reference scenario (ID 1), pivoted to one row per metric with the
## scenario labels joined.
data_sad_pkforest <- data_sad_pkforest_sim |>
  dplyr::group_by(SIM) |>
  dplyr::mutate(
    CMAXRATIO = CMAX / CMAX[ID == 1],
    CMINRATIO = CMIN / CMIN[ID == 1],
    AUCRATIO = AUC / AUC[ID == 1],
    CAVGRATIO = CAVG / CAVG[ID == 1]
  ) |>
  dplyr::ungroup() |>
  tidyr::pivot_longer(CMAX:CAVGRATIO, names_to = "metric", values_to = "value") |>
  dplyr::left_join(dplyr::select(forest_scn, ID, cov_var, cov_val), by = "ID") |>
  dplyr::select(ID, SIM, WTBL, FOOD, metric, value, cov_var, cov_val) |>
  as.data.frame()

usethis::use_data(data_sad_pkforest, overwrite = TRUE)
