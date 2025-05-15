#####Create data_sad internal analysis-ready dataset for a SAD study#####

#Define Study Conditions: Doses, Sample Size, LLOQ
doses <- c(10, 50, 100, 100, 200, 400) #Dose levels in mg
n_doses <- length(doses)
n_subj_per_dose <- 6 #Standard SAD Cohort: 6 active, 2 placebo
n_subj_total <- n_doses*n_subj_per_dose
lloq <- 1 #ng/mL
times <- c(mrgsolve::tgrid(0,2,0.5),
           mrgsolve::tgrid(3,5,1),
           mrgsolve::tgrid(8,16,4),
           mrgsolve::tgrid(24,36,12),
           mrgsolve::tgrid(48,168,24))


##Create Dosing Data
dose_data <- data.frame(id = 1:n_subj_total, time=0, amt = doses) |>
  dplyr::arrange(amt) |>
  dplyr::mutate(id = 1,
         evid = 1,
         cmt = 1,
         id = cumsum(id)) |>
  dplyr::group_by(amt) |>
  dplyr::mutate(tmp = 1,
         tmp = cumsum(tmp),
         food = ifelse(tmp > n_subj_per_dose, 1, 0)) |>
  dplyr::select(-tmp) |>
  dplyr::ungroup()


##Create Covariate Data
withr::with_seed(
  seed = 123456789,

  cov_data <- data.frame(id = 1:n_subj_total,
                         sexf = sample(c(0,1), n_subj_total, replace=T),
                         race = sample(c(1,2,3), n_subj_total, replace=T,
                                       prob=c(0.6,0.2, 0.2)),
                         agebl = sample(c(18:35), n_subj_total,replace=T),
                         wtbl = round(
                           sample(rnorm(1000, 75, 15), n_subj_total, replace=F),
                           digits = 1),
                         scrbl = round(
                           sample(rnorm(1000, 0.9, 0.2), n_subj_total, replace=F),
                           digits=2)
  ) |>
    dplyr::mutate(
      crclbl = signif(((140-agebl)/scrbl)*(wtbl/72)*ifelse(sexf==1, 0.85,1),
                      digits = 3)
    )

)


##Join Dosing and Covariate Data
sim_input_data <- dplyr::left_join(dose_data, cov_data)
colnames(sim_input_data) <- toupper(colnames(sim_input_data))


##Load Model File
mod <- mrgsolve::mread(system.file("models", "model.cpp", package="pmxhelpr"))


##Run Simulation
withr::with_seed(
  seed = 123456789,
  simout <- mrgsolve::mrgsim_df(x = mod, data = sim_input_data, tgrid = times, obsonly=TRUE)
)


##Create Concentration Data
conc_data <- simout |>
  dplyr::select(ID, NTIME = TIME, DV) |>
  dplyr::mutate(DV = round(DV, digits = 2),
         DV = ifelse(DV < lloq, NA_real_, DV),
         LDV = round(log(DV), digits = 4),
         MDV = as.numeric(is.na(DV)),
         CMT = 2,
         EVID = 0,
         BLQ = ifelse(NTIME == 0, -1, MDV),
         LLOQ = lloq) |>
  dplyr::rename(ODV = DV)


##Assemble Complete Dataset
withr::with_seed(
  seed = 123456789,

  data_sad <-
    dplyr::bind_rows(
      dplyr::rename(sim_input_data, NTIME = TIME),
      conc_data) |>
    dplyr::mutate(DOSE = AMT,
           TIME = dplyr::case_when(NTIME == 0 ~ 0,
                            .default = round(jitter(NTIME, factor = 2), digits=2)),
           NDAY = floor(NTIME/24) + 1) |>
    dplyr::group_by(ID) |>
    tidyr::fill(DOSE, FOOD, SEXF:CRCLBL) |>
    dplyr::ungroup() |>
    dplyr::arrange(ID, TIME, NTIME, EVID) |>
    dplyr::mutate(LINE = 1,
           LINE = cumsum(LINE),
           USUBJID = paste0("STUDYNUM-SITENUM-", ID),
           PART = ifelse(FOOD == 0, "Part 1-SAD", "Part 2-FE")) |>
    dplyr::select(LINE, ID, TIME, NTIME, NDAY, DOSE, AMT, EVID, ODV, LDV, CMT, MDV, BLQ, LLOQ,
           FOOD, SEXF, RACE, AGEBL, WTBL, SCRBL, CRCLBL,
           USUBJID, PART)
)

usethis::use_data(data_sad, overwrite = TRUE)

#####Create data_nca internal dataset of NCA parameters for data_sad#####

##Set NCA options
PKNCA::PKNCA.options(conc.blq = list("first" = "keep",
                              "middle" = unique(data_sad$LLOQ[!is.na(data_sad$LLOQ)]),
                              "last" = "drop"),
              allow.tmax.in.half.life = FALSE,
              min.hl.r.squared = 0.9)

##Calculation Intervals and Requested Parameters
intervals <-
  data.frame(start = 0,
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
  dplyr::mutate(CONC = ifelse(is.na(ODV), 0, ODV),
                AMT = AMT/1000) #convert from mg to ug. Concentration in ng/mL = ug/L


#Build PKNCA objects for concentration and dose including relevant strata
conc_obj <- PKNCA::PKNCAconc(dplyr::filter(data_sad_nca_input, EVID==0), CONC~TIME|ID+DOSE+PART)
dose_obj <- PKNCA::PKNCAdose(dplyr::filter(data_sad_nca_input, EVID==1), AMT~TIME|ID+PART)
nca_data_obj <- PKNCA::PKNCAdata(conc_obj, dose_obj, intervals = intervals)
nca_results_obj <- as.data.frame(pk.nca(nca_data_obj))

#Add Units
data_sad_nca <- nca_results_obj |>
  dplyr::mutate(units_dose = "mg",
                units_conc = "ng/mL",
                units_time = "hours")

usethis::use_data(data_sad_nca)
