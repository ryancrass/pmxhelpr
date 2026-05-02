##### var_addn ####

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

test_that("var_addn errors on mismatched lengths", {
  expect_error(var_addn(c(1, 2), c(1, 2, 3)),
               regexp = "must have the same length")
})


##### var_dosenorm ####

test_that("Output is numeric with same length as input", {
  dv <- c(10, 20, 30)
  dose <- c(100, 200, 300)
  out <- var_dosenorm(dv, dose)
  expect_true(is.numeric(out))
  expect_equal(length(out), length(dv))
})

test_that("Dose normalization returns dv / dose", {
  dv <- c(10, 20, 30)
  dose <- c(100, 200, 300)
  expect_equal(var_dosenorm(dv, dose), dv / dose)
})

test_that("Dose normalization works with dataset columns", {
  data <- dplyr::filter(data_sad, CMT == 2, EVID == 0)
  out <- var_dosenorm(data$ODV, data$DOSE)
  expect_equal(out, data$ODV / data$DOSE)
})

test_that("NA in dv propagates to output", {
  expect_equal(var_dosenorm(c(10, NA, 30), c(100, 200, 300)), c(0.1, NA, 0.1))
})

test_that("var_dosenorm errors on non-numeric dv_var", {
  expect_error(var_dosenorm(c("a", "b"), c(1, 2)),
               regexp = "`dv_var` must be numeric")
})

test_that("var_dosenorm errors on mismatched lengths", {
  expect_error(var_dosenorm(c(1, 2), c(1, 2, 3)),
               regexp = "must have the same length")
})

test_that("var_dosenorm warns and returns NA when dose_var contains zeros", {
  expect_warning(out <- var_dosenorm(c(10, 20, 30), c(100, 0, 300)),
                 regexp = "zero value")
  expect_true(is.na(out[2]))
  expect_equal(out[c(1, 3)], c(0.1, 0.1))
})

test_that("var_dosenorm preserves NA in dose_var without warning", {
  expect_no_warning(out <- var_dosenorm(c(10, 20, 30), c(100, NA, 300)))
  expect_true(is.na(out[2]))
})


##### var_predcorr ####

test_that("Output is numeric with same length as input", {
  dv <- c(5, 10, 15, 20)
  pred <- c(8, 12, 10, 14)
  out <- var_predcorr(dv, pred)
  expect_true(is.numeric(out))
  expect_equal(length(out), length(dv))
})

test_that("Prediction correction uses median of pred as reference", {
  dv <- c(5, 10, 15, 20)
  pred <- c(8, 12, 10, 14)
  predbin <- stats::median(pred)
  expected <- 0 + (dv - 0) * ((predbin - 0) / (pred - 0))
  expect_equal(var_predcorr(dv, pred), expected)
})

test_that("lower_bound shifts the correction formula", {
  dv <- c(5, 10, 15, 20)
  pred <- c(8, 12, 10, 14)
  lb <- 2
  predbin <- stats::median(pred)
  expected <- lb + (dv - lb) * ((predbin - lb) / (pred - lb))
  expect_equal(var_predcorr(dv, pred, lower_bound = lb), expected)
})

test_that("Constant pred returns original dv values", {
  dv <- c(5, 10, 15)
  pred <- c(10, 10, 10)
  expect_equal(var_predcorr(dv, pred), dv)
})

test_that("NA in dv propagates to output", {
  dv <- c(5, NA, 15)
  pred <- c(8, 12, 10)
  out <- var_predcorr(dv, pred)
  expect_true(is.na(out[2]))
  expect_false(is.na(out[1]))
})

test_that("pred equal to lower_bound returns NA instead of Inf", {
  dv <- c(5, 10, 15)
  pred <- c(8, 0, 10)
  out <- var_predcorr(dv, pred)
  expect_true(is.na(out[2]))
  expect_false(is.na(out[1]))
  expect_false(is.na(out[3]))
})

test_that("var_predcorr errors on non-numeric dv_var", {
  expect_error(var_predcorr(c("a", "b"), c(1, 2)),
               regexp = "`dv_var` must be numeric")
})

test_that("var_predcorr errors on mismatched lengths", {
  expect_error(var_predcorr(c(1, 2), c(1, 2, 3)),
               regexp = "must have the same length")
})

test_that("var_predcorr warns and returns all-NA when pred_var is all NA", {
  expect_warning(out <- var_predcorr(c(5, 10, 15), c(NA_real_, NA_real_, NA_real_)),
                 regexp = "all NA")
  expect_true(all(is.na(out)))
  expect_length(out, 3)
})

test_that("var_predcorr handles partial NA in pred_var without warning", {
  expect_no_warning(out <- var_predcorr(c(5, 10, 15), c(8, NA, 12)))
  expect_true(is.na(out[2]))
})


##### var_loqcens ####

test_that("var_loqcens encodes values below loq as -Inf", {
  out <- pmxhelpr:::var_loqcens(c(1, 2, 5, 10), loq = 3)
  expect_equal(out, c(-Inf, -Inf, 5, 10))
})

test_that("var_loqcens encodes is.na positions as -Inf with no loq or mdv", {
  out <- pmxhelpr:::var_loqcens(c(1, NA, 5))
  expect_equal(out, c(1, -Inf, 5))
})

test_that("var_loqcens encodes mdv == 1 positions as -Inf", {
  out <- pmxhelpr:::var_loqcens(c(1, 2, 5), mdv = c(0, 1, 0))
  expect_equal(out, c(1, -Inf, 5))
})

test_that("var_loqcens combines loq, mdv, and is.na triggers", {
  out <- pmxhelpr:::var_loqcens(c(1, 2, NA, 10), loq = 3, mdv = c(0, 0, 0, 1))
  expect_equal(out, c(-Inf, -Inf, -Inf, -Inf))
})

test_that("var_loqcens errors on non-numeric x", {
  expect_error(pmxhelpr:::var_loqcens(c("a", "b"), loq = 1),
               regexp = "`x` must be numeric")
})

test_that("var_loqcens errors on non-numeric loq", {
  expect_error(pmxhelpr:::var_loqcens(c(1, 2), loq = "a"),
               regexp = "`loq` must be numeric")
})

test_that("var_loqcens errors on loq with bad length", {
  expect_error(pmxhelpr:::var_loqcens(c(1, 2, 3), loq = c(1, 2)),
               regexp = "length 1 or the same length")
})

test_that("var_loqcens errors on mdv with mismatched length", {
  expect_error(pmxhelpr:::var_loqcens(c(1, 2, 3), mdv = c(0, 1)),
               regexp = "must be the same length")
})

test_that("var_loqcens accepts vector loq matching length(x)", {
  out <- pmxhelpr:::var_loqcens(c(1, 2, 5), loq = c(0.5, 3, 0.5))
  expect_equal(out, c(1, -Inf, 5))
})

test_that("var_loqcens with all-NA loq is a no-op for the loq trigger", {
  expect_equal(pmxhelpr:::var_loqcens(c(1, 2, 5), loq = NA_real_),
               c(1, 2, 5))
})

##### var_infna ####

test_that("var_infna replaces -Inf with NA_real_ and leaves +Inf alone", {
  out <- pmxhelpr:::var_infna(c(1, -Inf, 3, Inf, NA))
  expect_equal(out, c(1, NA_real_, 3, Inf, NA_real_))
})

test_that("var_infna errors on non-numeric x", {
  expect_error(pmxhelpr:::var_infna(c("a", "b")),
               regexp = "`x` must be numeric")
})
