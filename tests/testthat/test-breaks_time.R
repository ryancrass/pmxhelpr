#####var_timebreaks#####

##Test Output
test_that("Output is a `numeric` vector ", {
  expect_vector(pmxhelpr:::var_timebreaks(data_sad$NTIME), ptype = numeric())
})

test_that("var_timebreaks honors n upper bound and returns monotonic increasing breaks", {
  breaks <- pmxhelpr:::var_timebreaks(data_sad$NTIME, n = 10)
  expect_lte(length(breaks), 10)
  expect_gte(length(breaks), 2)
  expect_true(all(diff(breaks) > 0))
})

test_that("Maximum time break is <= maximum value of the time binning variable ", {
  expect_lte(max(pmxhelpr:::var_timebreaks(data_sad$NTIME)),
             max(data_sad$NTIME, na.rm=T))
})

##Test Argument Handling

test_that("Error if incorrect class for arugmument `x`", {
  expect_error(pmxhelpr:::var_timebreaks(x = c("1", "$", "3")),
               regexp = "argument `x` must be coercible to class `numeric`")
})

test_that("Error if `unit` is not within expected values", {
  expect_error(pmxhelpr:::var_timebreaks(unique(data_sad$NTIME), unit = "years"),
               regexp = "argument `timeu` must be one of")
})

test_that("Error if arugmument `n` is not coercible to an integer", {
  expect_error(pmxhelpr:::var_timebreaks(unique(data_sad$NTIME), n = "$"),
               regexp = "argument `n` must be coercible to class `integer`")
})

test_that("Error on empty x vector", {
  expect_error(pmxhelpr:::var_timebreaks(numeric(0)),
               regexp = "must contain at least one non-NA numeric value")
})

test_that("Error on all-NA x vector", {
  # check_numeric rejects all-NA before var_timebreaks's own guard
  expect_error(pmxhelpr:::var_timebreaks(c(NA_real_, NA_real_)),
               regexp = "must be coercible to class `numeric`")
})
