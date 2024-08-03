test_that("output data.frame contains variable PRED", {
  expect_equal(length(df_addpred(data=data_sad,model=model_mread_load("model"))$PRED),
               nrow(data_sad))
})
