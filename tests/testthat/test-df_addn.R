#####df_addn####

##Test Output
test_that("Output is a data.frame with same number of rows", {
  out <- df_addn(data_sad, grp_var = "DOSE", id_var = "ID")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), nrow(data_sad))
})

test_that("grp_var column is converted to a factor", {
  out <- df_addn(data_sad, grp_var = "DOSE", id_var = "ID")
  expect_true(is.factor(out$DOSE))
})

test_that("Factor labels contain (n=) with correct counts", {
  out <- df_addn(data_sad, grp_var = "DOSE", id_var = "ID")
  labels <- levels(out$DOSE)
  expect_true(all(grepl("\\(n=\\d+\\)", labels)))
})

test_that("Correct count of unique IDs per group", {
  expected_n <- dplyr::n_distinct(data_sad$ID[data_sad$DOSE == unique(data_sad$DOSE)[1]])
  out <- df_addn(data_sad, grp_var = "DOSE", id_var = "ID")
  first_label <- levels(out$DOSE)[1]
  expect_match(first_label, paste0("n=", expected_n))
})

test_that("sep argument is included in factor labels", {
  out <- df_addn(data_sad, grp_var = "DOSE", id_var = "ID", sep = "mg")
  labels <- levels(out$DOSE)
  expect_true(all(grepl("mg", labels)))
})

test_that("Factor labels without sep do not contain extra separator", {
  out <- df_addn(data_sad, grp_var = "DOSE", id_var = "ID")
  labels <- levels(out$DOSE)
  expect_false(any(grepl("mg", labels)))
})

##Test Argument Handling
test_that("Error if incorrect class for argument `data`", {
  expect_error(df_addn(data = "not_a_df", grp_var = "DOSE"),
               regexp = "must be a `data.frame`")
})

test_that("Error if grp_var does not exist in data", {
  expect_error(df_addn(data = data_sad, grp_var = "NONEXIST"),
               regexp = "must be variables in `data`")
})

test_that("Error if id_var does not exist in data", {
  expect_error(df_addn(data = data_sad, grp_var = "DOSE", id_var = "NONEXIST"),
               regexp = "must be variables in `data`")
})

##Test NSE Bare Names
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
