#####plot_dvtime####

##Test Output
test_that("Output is a `ggplot` plot object", {
  expect_s3_class(plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = "ODV"),
               class = "ggplot")
})

test_that("Output plot maps variable TIME to the x aesthetic", {
  expect_equal(
    rlang::quo_name(plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = "ODV")$mapping$x),
    "TIME"
  )
})

test_that("Output plot maps variable DV to the y aesthetic", {
  expect_equal(
    rlang::quo_name(plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = "ODV")$mapping$y),
    "DV"
  )
})

test_that("Output plot contains a caption when argument `show_caption` = TRUE", {
  expect_equal(
    plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = "ODV")$labels$caption,
    "Solid circles and thick lines are the mean\nOpen circles are observations"
  )
})

test_that("Output plot does not contain a caption when `show_caption = FALSE`", {
  expect_no_match(
    names(plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = "ODV", show_caption = FALSE)$labels),
    "caption"
  )
})


##Test NSE Bare Names
test_that("plot_dvtime accepts bare names and produces ggplot", {
  expect_s3_class(plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = ODV), "ggplot")
})

test_that("plot_dvtime bare names match string output aesthetics", {
  p1 <- plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = ODV)
  p2 <- plot_dvtime(dplyr::filter(data_sad, CMT != 3), dv_var = "ODV")
  expect_equal(p1$labels, p2$labels)
})

test_that("plot_dvtime accepts bare col_var", {
  data <- dplyr::mutate(dplyr::filter(data_sad, CMT != 3), Dose = var_addn(DOSE, ID, sep = "mg"))
  expect_s3_class(plot_dvtime(data, dv_var = ODV, col_var = Dose), "ggplot")
})
