#####Test df_vpcstats#####

## Setup: shared simulation output
testsim <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3),
                               model = model_mread_load("pkmodel"),
                               replicates = 10,
                               dv_var = "ODV")

## Test Output
test_that("df_vpcstats returns a data.frame", {
  result <- df_vpcstats(sim = testsim)
  expect_s3_class(result, "data.frame")
})

test_that("df_vpcstats returns expected columns", {
  result <- df_vpcstats(sim = testsim)
  expected_cols <- c("NTIME", "nbin",
                     "q5_low", "q5_med", "q5_hi",
                     "q50_low", "q50_med", "q50_hi",
                     "q95_low", "q95_med", "q95_hi",
                     "obs5", "obs50", "obs95")
  expect_true(all(expected_cols %in% colnames(result)))
})

test_that("df_vpcstats returns one row per unique bin value", {
  result <- df_vpcstats(sim = testsim)
  expect_equal(nrow(result), length(unique(testsim$NTIME)))
})

## Test Stratification
test_that("df_vpcstats with strat_var returns rows for each bin x stratum", {
  testsim_strat <- df_mrgsim_replicate(
    data = dplyr::filter(data_sad, CMT != 3),
    model = model_mread_load("pkmodel"),
    replicates = 10,
    dv_var = "ODV",
    char_vars = "FOOD")
  testsim_strat <- dplyr::mutate(testsim_strat, FOOD_f = factor(FOOD))

  result <- df_vpcstats(sim = testsim_strat, strat_var = FOOD_f)
  expect_true("FOOD_f" %in% colnames(result))
  expect_equal(nrow(result),
               nrow(unique(testsim_strat[, c("NTIME", "FOOD_f")])))
})

## Test LLOQ handling
test_that("df_vpcstats with loq returns NA for observed quantiles below threshold", {
  result <- df_vpcstats(sim = testsim, loq = 1e6)
  expect_true(all(is.na(result$obs5)))
  expect_true(all(is.na(result$obs50)))
  expect_true(all(is.na(result$obs95)))
})

## Test quantile ordering
test_that("simulated quantile medians are ordered q5_med <= q50_med <= q95_med", {
  result <- df_vpcstats(sim = testsim)
  expect_true(all(result$q5_med <= result$q50_med, na.rm = TRUE))
  expect_true(all(result$q50_med <= result$q95_med, na.rm = TRUE))
})
