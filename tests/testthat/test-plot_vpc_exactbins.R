test_that("Error if incorrect class for arugmument `sim`", {
  expect_error(plot_vpc_exactbins(
    sim = "df",
    pcvpc = TRUE
  ))
})

test_that("Error if TIME variable specified in time_vars does not exist in `sim`", {
  expect_error(plot_vpc_exactbins(
    sim = df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 10,
                              output_vars = c(DV = "ODV")),
    pcvpc = TRUE,
    strat_vars = NULL,
    time_vars = c(TIME = "ATFD", NTIME = "NTIME")
  ))
})

test_that("Error if NTIME variable specified in time_vars does not exist in `sim`", {
  expect_error(plot_vpc_exactbins(
    sim = df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 10,
                              output_vars = c(DV = "ODV")),
    pcvpc = TRUE,
    strat_vars = NULL,
    time_vars = c(TIME = "TIME", NTIME = "NTFD")
  ))
})

test_that("Error if OBSDV variable specified in output_vars does not exist in `sim`", {
  expect_error(plot_vpc_exactbins(
    sim = df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 10,
                              output_vars = c(OBSDV = "DV")),
    pcvpc = TRUE,
    strat_vars = NULL
  ))
})

test_that("Error if SIMDV variable specified in output_vars does not exist in `sim`", {
  expect_error(plot_vpc_exactbins(
    sim = df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 10,
                              output_vars = c(OBSDV = "SIMDV")),
    pcvpc = TRUE,
    strat_vars = NULL
  ))
})

test_that("Error if variables specified by strat_vars do not exist in `sim`", {
  expect_error(plot_vpc_exactbins(
    sim = df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 10,
                              output_vars = c(OBSDV = "SIMDV")),
    pcvpc = TRUE,
    strat_vars = c("test")
  ))
})

test_that("Error if variable specified by irep_name does not exist in `sim`", {
  expect_error(plot_vpc_exactbins(
    sim = df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 10,
                              output_vars = c(OBSDV = "SIMDV")),
    pcvpc = TRUE,
    irep_name = "test"
  ))
})
