#####var_addn####

##Test Output
test_that("Output is a factor with same length as input", {
  out <- var_addn(data_sad$DOSE, data_sad$ID)
  expect_true(is.factor(out))
  expect_equal(length(out), nrow(data_sad))
})

test_that("Factor labels contain (n=) with correct counts", {
  out <- var_addn(data_sad$DOSE, data_sad$ID)
  labels <- levels(out)
  expect_true(all(grepl("\\(n=\\d+\\)", labels)))
})

test_that("Correct count of unique IDs per group", {
  expected_n <- dplyr::n_distinct(data_sad$ID[data_sad$DOSE == unique(data_sad$DOSE)[1]])
  out <- var_addn(data_sad$DOSE, data_sad$ID)
  first_label <- levels(out)[1]
  expect_match(first_label, paste0("n=", expected_n))
})

test_that("sep argument is included in factor labels", {
  out <- var_addn(data_sad$DOSE, data_sad$ID, sep = "mg")
  labels <- levels(out)
  expect_true(all(grepl("mg", labels)))
})

test_that("Factor labels without sep do not contain extra separator", {
  out <- var_addn(data_sad$DOSE, data_sad$ID)
  labels <- levels(out)
  expect_false(any(grepl("mg", labels)))
})
