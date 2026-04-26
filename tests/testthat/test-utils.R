#####utils - internal helper functions####

#####list_update####

test_that("list_update returns source unchanged when update = NULL", {
  src <- list(a = 1, b = 2)
  expect_equal(pmxhelpr:::list_update(NULL, src), src)
})

test_that("list_update overrides matching keys from update", {
  src <- list(a = 1, b = 2)
  result <- pmxhelpr:::list_update(list(a = 99), src)
  expect_equal(result$a, 99)
  expect_equal(result$b, 2)
})

test_that("list_update warns on non-matching keys in update", {
  src <- list(a = 1, b = 2)
  expect_warning(pmxhelpr:::list_update(list(fake = 1), src),
                 regexp = "not a valid element")
})

test_that("list_update preserves keys not mentioned in update", {
  src <- list(a = 1, b = 2, c = 3)
  result <- pmxhelpr:::list_update(list(a = 10), src)
  expect_equal(result$b, 2)
  expect_equal(result$c, 3)
})

#####check_df####

test_that("check_df does not error on valid data.frame", {
  expect_no_error(pmxhelpr:::check_df(data.frame(x = 1), "data"))
})

test_that("check_df errors on non-data.frame", {
  expect_error(pmxhelpr:::check_df("not_a_df", "data"),
               regexp = "must be a `data.frame`")
})

#####check_numeric####

test_that("check_numeric does not error on numeric input", {
  expect_no_error(pmxhelpr:::check_numeric(42, "x"))
})

test_that("check_numeric errors on non-numeric string", {
  expect_error(pmxhelpr:::check_numeric("$", "x"),
               regexp = "must be coercible to class `numeric`")
})

#####check_numeric_strict####

test_that("check_numeric_strict does not error on numeric input", {
  expect_no_error(pmxhelpr:::check_numeric_strict(42, "x"))
})

test_that("check_numeric_strict errors on character input", {
  expect_error(pmxhelpr:::check_numeric_strict("1", "x"),
               regexp = "must be class `numeric`")
})

#####check_integer####

test_that("check_integer does not error on integer-coercible input", {
  expect_no_error(pmxhelpr:::check_integer(5, "n"))
})

test_that("check_integer errors on non-integer string", {
  expect_error(pmxhelpr:::check_integer("$", "n"),
               regexp = "must be coercible to class `integer`")
})

#####check_varsindf####

test_that("check_varsindf does not error when variable exists in data", {
  expect_no_error(pmxhelpr:::check_varsindf(data.frame(x = 1), "x", "data", "var"))
})

test_that("check_varsindf errors when variable does not exist", {
  expect_error(pmxhelpr:::check_varsindf(data.frame(x = 1), "y", "data", "var"),
               regexp = "must be variables in")
})

#####check_factor####

test_that("check_factor does not error on factor-coercible column", {
  df <- data.frame(x = c("a", "b", "c"))
  expect_no_error(pmxhelpr:::check_factor(df, "x", "x"))
})

#####check_timeu####

test_that("check_timeu does not error on valid time units", {
  expect_no_error(pmxhelpr:::check_timeu("hours"))
  expect_no_error(pmxhelpr:::check_timeu("days"))
  expect_no_error(pmxhelpr:::check_timeu("weeks"))
  expect_no_error(pmxhelpr:::check_timeu("months"))
})

test_that("check_timeu errors on invalid time unit", {
  expect_error(pmxhelpr:::check_timeu("years"),
               regexp = "argument timeu must be one of")
})

#####check_loq_method####

test_that("check_loq_method does not error when loq_method = 0", {
  expect_no_error(pmxhelpr:::check_loq_method(NULL, 0, data.frame()))
})

test_that("check_loq_method errors when loq_method = NULL", {
  expect_error(pmxhelpr:::check_loq_method(NULL, NULL, data.frame()),
               regexp = "argument `loq_method` must be 0, 1, or 2")
})

test_that("check_loq_method errors when loq_method = 1, loq = NULL, and no LLOQ column", {
  expect_error(pmxhelpr:::check_loq_method(NULL, 1, data.frame(x = 1)),
               regexp = "numeric variable `LLOQ` must be present")
})

#####check_levelsinvar####

test_that("check_levelsinvar does not error when levels exist", {
  df <- data.frame(x = c("a", "b", "c"))
  expect_no_error(pmxhelpr:::check_levelsinvar(df, "x", c("a", "b"), "x", "levels"))
})

test_that("check_levelsinvar errors when levels don't exist", {
  df <- data.frame(x = c("a", "b", "c"))
  expect_error(pmxhelpr:::check_levelsinvar(df, "x", c("z"), "x", "levels"),
               regexp = "must be levels in variable")
})

#####check_lm####

test_that("check_lm does not error on lm object", {
  fit <- lm(mpg ~ wt, data = mtcars)
  expect_no_error(pmxhelpr:::check_lm(fit, "fit"))
})

test_that("check_lm errors on non-lm object", {
  expect_error(pmxhelpr:::check_lm("not_lm", "fit"),
               regexp = "must be class `lm`")
})

#####capture_col####

test_that("capture_col returns NULL for null quo", {
  expect_null(pmxhelpr:::capture_col(rlang::quo(NULL)))
})

test_that("capture_col returns column name string for non-null quo", {
  expect_equal(pmxhelpr:::capture_col(rlang::quo(DOSE)), "DOSE")
})
