##### NSE Bare Name Tests #####
## Tests that bare column names produce identical results to string column names

####### df_addn #######

test_that("df_addn accepts bare names and matches string output", {
  d1 <- df_addn(data_sad, grp_var = DOSE, id_var = ID)
  d2 <- df_addn(data_sad, grp_var = "DOSE", id_var = "ID")
  expect_identical(d1, d2)
})

test_that("df_addn accepts bare names with sep argument", {
  d1 <- df_addn(data_sad, grp_var = DOSE, id_var = ID, sep = "mg")
  d2 <- df_addn(data_sad, grp_var = "DOSE", id_var = "ID", sep = "mg")
  expect_identical(d1, d2)
})

####### df_addpred #######

test_that("df_addpred accepts bare names and matches string output", {
  model <- model_mread_load(model = "model")
  d1 <- df_addpred(data_sad, model, output_var = IPRED)
  d2 <- df_addpred(data_sad, model, output_var = "IPRED")
  expect_identical(d1, d2)
})

####### mod_loglog #######

test_that("mod_loglog accepts bare names and matches string output", {
  dat <- dplyr::filter(data_sad_nca, PPTESTCD == "aucinf.obs")
  m1 <- mod_loglog(dat, exp_var = PPORRES, dose_var = DOSE)
  m2 <- mod_loglog(dat, exp_var = "PPORRES", dose_var = "DOSE")
  expect_identical(coef(m1), coef(m2))
})

####### df_doseprop #######

test_that("df_doseprop accepts bare names and matches string output", {
  t1 <- df_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"),
                     metric_var = PPTESTCD, exp_var = PPORRES, dose_var = DOSE)
  t2 <- df_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"),
                     metric_var = "PPTESTCD", exp_var = "PPORRES", dose_var = "DOSE")
  expect_identical(t1, t2)
})

####### df_nobsbin #######

test_that("df_nobsbin accepts bare names and matches string output", {
  n1 <- df_nobsbin(data_sad, bin_var = NTIME)
  n2 <- df_nobsbin(data_sad, bin_var = "NTIME")
  expect_identical(n1, n2)
})

####### df_pcdv #######

test_that("df_pcdv accepts bare names and matches string output", {
  model <- model_mread_load(model = "model")
  data_pred <- df_addpred(data_sad, model)
  p1 <- df_pcdv(data_pred, bin_var = NTIME, dvpred_vars = c(DV = "ODV", PRED = "PRED"))
  p2 <- df_pcdv(data_pred, bin_var = "NTIME", dvpred_vars = c(DV = "ODV", PRED = "PRED"))
  expect_identical(p1, p2)
})

####### df_mrgsim_replicate #######

test_that("df_mrgsim_replicate accepts bare names", {
  model <- model_mread_load(model = "model")
  s1 <- df_mrgsim_replicate(data_sad, model, replicates = 2,
                              dv_var = ODV, irep_name = SIM,
                              num_vars = c("CMT", "EVID", "MDV"),
                              char_vars = c("USUBJID"))
  expect_true(nrow(s1) > 0)
  expect_true("SIM" %in% colnames(s1))
})

####### plot_dvtime #######

test_that("plot_dvtime accepts bare names and produces ggplot", {
  expect_s3_class(plot_dvtime(data_sad, dv_var = ODV), "ggplot")
})

test_that("plot_dvtime bare names match string output aesthetics", {
  p1 <- plot_dvtime(data_sad, dv_var = ODV)
  p2 <- plot_dvtime(data_sad, dv_var = "ODV")
  expect_equal(p1$labels, p2$labels)
})

test_that("plot_dvtime accepts bare col_var", {
  data <- df_addn(dplyr::mutate(data_sad, Dose = DOSE), grp_var = Dose, sep = "mg")
  expect_s3_class(plot_dvtime(data, dv_var = ODV, col_var = Dose), "ggplot")
})

####### plot_dvtime_dual #######

test_that("plot_dvtime_dual accepts bare names", {
  data <- df_addn(dplyr::mutate(data_sad_pd, Dose = DOSE), grp_var = Dose, sep = "mg")
  expect_s3_class(plot_dvtime_dual(data, dv_var1 = ODV, dv_var2 = ODV, col_var = Dose),
                  "ggplot")
})

####### plot_dvconc #######

test_that("plot_dvconc accepts bare names and produces ggplot", {
  data <- df_addn(dplyr::mutate(data_sad_pd, Dose = DOSE), grp_var = Dose, sep = "mg")
  expect_s3_class(plot_dvconc(data, dv_var = ODV, idv_var = CONC, col_var = Dose), "ggplot")
})

test_that("plot_dvconc bare names match string output", {
  data <- df_addn(dplyr::mutate(data_sad_pd, Dose = DOSE), grp_var = Dose, sep = "mg")
  p1 <- plot_dvconc(data, dv_var = ODV, idv_var = CONC)
  p2 <- plot_dvconc(data, dv_var = "ODV", idv_var = "CONC")
  expect_equal(p1$labels, p2$labels)
})

####### plot_popgof #######

test_that("plot_popgof accepts bare names", {
  expect_s3_class(plot_popgof(data_sad_pkfit, output_vars = c(DV = "ODV"),
                               grp_var = ID, dose_var = DOSE, dosenorm = TRUE),
                  "ggplot")
})

####### plot_doseprop #######

test_that("plot_doseprop accepts bare names", {
  expect_s3_class(
    plot_doseprop(dplyr::filter(data_sad_nca, PART == "Part 1-SAD"),
                  metrics = c("aucinf.obs", "cmax"),
                  metric_var = PPTESTCD, exp_var = PPORRES, dose_var = DOSE),
    "ggplot")
})
