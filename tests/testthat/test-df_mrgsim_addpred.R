##Test Output
test_that("output is a data.frame", {
  expect_s3_class(df_mrgsim_addpred(data=dplyr::filter(data_sad, CMT != 3),model=model_mread_load("pkmodel")),
               "data.frame")
})

test_that("output data.frame contains same number of rows as input data.frame", {
  expect_equal(nrow(df_mrgsim_addpred(data=data_sad,model=model_mread_load("pkmodel"))),
               nrow(data_sad))
})

test_that("output data.frame contains variable PRED", {
  expect_named(dplyr::select(df_mrgsim_addpred(data=dplyr::filter(data_sad, CMT != 3),model=model_mread_load("pkmodel")), PRED),
               "PRED")
})

test_that("output is returned if non-default option specified for argument `output_var`", {
  expect_s3_class(df_mrgsim_addpred(data=dplyr::filter(data_sad, CMT != 3),model=model_mread_load("pkmodel"), output_var = "Y"),
                  "data.frame")
})


##Test Argument Handling
test_that("Error if incorrect class for arugmument `data`", {
  expect_error(df_mrgsim_addpred(data="",model=model_mread_load("pkmodel")),
               regexp = "argument `data` must be a `data.frame`")
})

test_that("Error if incorrect class for arugmument `model`", {
  expect_error(df_mrgsim_addpred(data=dplyr::filter(data_sad, CMT != 3),model="pkmodel"),
               regexp = "argument `model` must be class `mrgmod`")
})

test_that("Error if incorrect class for arugmument `output_var`", {
  expect_error(df_mrgsim_addpred(data=dplyr::filter(data_sad, CMT != 3),model=model_mread_load("pkmodel"), output_var = "PRED"),
  regexp = "argument `output_var` must be captured as output in `model`")
})

##Test NSE Bare Names
test_that("df_mrgsim_addpred accepts bare names and matches string output", {
  model <- model_mread_load(model = "pkmodel")
  d1 <- df_mrgsim_addpred(dplyr::filter(data_sad, CMT != 3), model, output_var = IPRED)
  d2 <- df_mrgsim_addpred(dplyr::filter(data_sad, CMT != 3), model, output_var = "IPRED")
  expect_identical(d1, d2)
})

test_that("df_mrgsim_addpred warns when input data already contains a PRED column", {
  data <- dplyr::filter(data_sad, CMT != 3)
  data$PRED <- 0
  expect_warning(
    df_mrgsim_addpred(data = data, model = model_mread_load("pkmodel")),
    regexp = "already contains a `PRED` column"
  )
})

test_that("df_mrgsim_addpred errors on zero-row data", {
  data <- dplyr::filter(data_sad, CMT != 3)[0, ]
  expect_error(
    df_mrgsim_addpred(data = data, model = model_mread_load("pkmodel")),
    regexp = "must have at least one row"
  )
})

##Test `...` forwarding
test_that("arguments in `...` reach mrgsim_df() and leave PRED deterministic", {
  model <- model_mread_load(model = "pkmodel")
  data <- dplyr::filter(data_sad, CMT != 3)
  base <- df_mrgsim_addpred(data, model)
  set.seed(1)
  d1 <- df_mrgsim_addpred(data, model, recover = "USUBJID")
  set.seed(2)
  d2 <- df_mrgsim_addpred(data, model, recover = "USUBJID")
  expect_equal(d1$PRED, base$PRED)
  expect_equal(d2$PRED, base$PRED)
  # PRED is the simulation with every random effect zeroed
  ref <- mrgsolve::mrgsim_df(mrgsolve::zero_re(model), data = data, carry_out = "IPRED")
  expect_equal(base$PRED, ref$IPRED)
})

test_that("a `carry_out` in `...` is merged with `output_var`", {
  model <- model_mread_load(model = "pkmodel")
  data <- dplyr::filter(data_sad, CMT != 3)
  expect_equal(df_mrgsim_addpred(data, model, carry_out = "DOSE")$PRED,
               df_mrgsim_addpred(data, model)$PRED)
})

test_that("arguments in `...` that change the row count abort", {
  expect_error(
    df_mrgsim_addpred(dplyr::filter(data_sad, CMT != 3), model_mread_load("pkmodel"),
                      obsonly = TRUE),
    regexp = "needs one output row per input row"
  )
})
