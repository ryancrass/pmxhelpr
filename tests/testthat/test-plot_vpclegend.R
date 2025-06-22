##Test Output
test_that("Output is a `ggplot` object", {
  expect_s3_class(plot_vpclegend(),
                  class = "ggplot")
})


##Test Arguments
