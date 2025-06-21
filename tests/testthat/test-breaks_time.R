#####breaks_time#####

##Test Output
test_that("Output is a `numeric` vector ", {
  expect_vector(breaks_time(data_sad$NTIME), ptype = numeric())
})

test_that("Length of time bins returned matches `n` argument", {
  expect_lte(length(breaks_time(data_sad$NTIME, n = 10)),
             10)
})

test_that("Maximum time break is <= maximum value of the time binning variable ", {
  expect_lte(max(breaks_time(data_sad$NTIME)),
             max(data_sad$NTIME, na.rm=T))
})

##Test Argument Handling

test_that("Error if incorrect class for arugmument `x`", {
  expect_error(breaks_time(x = c("1", "$", "3")),
               regexp = "argument `x` must be coercible to class `numeric`")
})

test_that("Error if `unit` is not within expected values", {
  expect_error(breaks_time(unique(data_sad$NTIME), unit = "years"),
               regexp = "argument timeu must be one of: hours, days, weeks, month")
})

test_that("Error if arugmument `n` is not coercible to an integer", {
  expect_error(breaks_time(unique(data_sad$NTIME), n = "$"),
               regexp = "argument `n` must be coercible to class `integer`")
})
