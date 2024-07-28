#####Study Design - Single Oral Dose#####

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
dose_data <- data.frame(id = 1:n_subj_total, time=0, amt = doses) %>%
  dplyr::arrange(amt) %>%
  dplyr::mutate(id = 1,
         evid = 1,
         cmt = 1,
         id = cumsum(id)) %>%
  dplyr::group_by(amt) %>%
  dplyr::mutate(tmp = 1,
         tmp = cumsum(tmp),
         food = ifelse(tmp > n_subj_per_dose, 1, 0)) %>%
  dplyr::select(-tmp) %>%
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
  ) %>%
    dplyr::mutate(
      crclbl = signif(((140-agebl)/scrbl)*(wtbl/72)*ifelse(sexf==1, 0.85,1),
                      digits = 3)
    )

)


##Join Dosing and Covariate Data
sim_input_data <- dplyr::left_join(dose_data, cov_data)
colnames(sim_input_data) <- toupper(colnames(sim_input_data))


##Load Model File
mod <- mrgsolve::mread(here::here("data-raw/model.cpp"))


##Run Simulation
withr::with_seed(
  seed = 123456789,
  simout <- mrgsolve::mrgsim_df(x = mod, data = sim_input_data, tgrid = times, obsonly=TRUE)
)


##Create Concentration Data
conc_data <- simout %>%
  dplyr::select(ID, NTIME = TIME, DV) %>%
  dplyr::mutate(DV = round(DV, digits = 2),
         DV = ifelse(DV < lloq, NA_real_, DV),
         LDV = round(log(DV), digits = 4),
         MDV = as.numeric(is.na(DV)),
         CMT = 2,
         EVID = 0,
         BLQ = ifelse(NTIME == 0, -1, MDV),
         LLOQ = lloq) %>%
  dplyr::rename(ODV = DV)


##Assemble Complete Dataset
withr::with_seed(
  seed = 123456789,

  data_sad <-
    dplyr::bind_rows(
      dplyr::rename(sim_input_data, NTIME = TIME),
      conc_data) %>%
    dplyr::mutate(DOSE = AMT,
           TIME = dplyr::case_when(NTIME == 0 ~ 0,
                            .default = round(jitter(NTIME), digits=2)),
           NDAY = floor(NTIME/24) + 1) %>%
    dplyr::group_by(ID) %>%
    tidyr::fill(DOSE, FOOD, SEXF:CRCLBL) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(ID, TIME, NTIME, EVID) %>%
    dplyr::mutate(LINE = 1,
           LINE = cumsum(LINE),
           USUBJID = paste0("STUDYNUM-SITENUM-", ID),
           PART = ifelse(FOOD == 0, "Part 1-SAD", "Part 2-FE")) %>%
    dplyr::select(LINE, ID, TIME, NTIME, NDAY, DOSE, AMT, EVID, ODV, LDV, CMT, MDV, BLQ, LLOQ,
           FOOD, SEXF, RACE, AGEBL, WTBL, SCRBL, CRCLBL,
           USUBJID, PART)
)

usethis::use_dat(data_sad)
