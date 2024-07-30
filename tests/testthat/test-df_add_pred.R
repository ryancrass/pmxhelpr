test_that("output data.frame contains variable PRED", {
  expect_equal(length(df_add_pred(data=data_sad,model=model_load("model"))$PRED),
               nrow(data_sad))
})
