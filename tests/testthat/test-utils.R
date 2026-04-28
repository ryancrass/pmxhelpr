#####utils - internal helper functions####

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
               regexp = "must be variable")
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

#####resolve_var####

test_that("resolve_var returns NULL for null quo", {
  f <- function(x = NULL) pmxhelpr:::resolve_var(rlang::enquo(x), nullable = TRUE)
  expect_null(f())
})

test_that("resolve_var returns column name string for bare name quo", {
  expect_equal(pmxhelpr:::resolve_var(rlang::quo(DOSE)), "DOSE")
})

test_that("resolve_var returns string for string literal quo", {
  expect_equal(pmxhelpr:::resolve_var(rlang::quo("DOSE")), "DOSE")
})

test_that("resolve_var with nullable returns NULL when value is NULL", {
  val <- NULL
  expect_null(pmxhelpr:::resolve_var(rlang::quo(val), nullable = TRUE))
})

test_that("resolve_var resolves variable containing string", {
  val <- "DOSE"
  expect_equal(pmxhelpr:::resolve_var(rlang::quo(val)), "DOSE")
})

#####df_prep_dvtime####

test_that("df_prep_dvtime renames DV and returns list with data and lloq", {
  df <- data.frame(TIME = 1:3, NTIME = 1:3, ODV = c(10, 20, 30),
                   MDV = c(0, 0, 0), EVID = c(0, 0, 0))
  result <- pmxhelpr:::df_prep_dvtime(df, "TIME", "NTIME", dv_var_str = "ODV")
  expect_true(is.list(result))
  expect_true("DV" %in% colnames(result$data))
  expect_true(is.na(result$lloq))
})

test_that("df_prep_dvtime applies BLQ imputation", {
  df <- data.frame(TIME = c(0, 1, 2), NTIME = c(0, 1, 2),
                   DV = c(NA, NA, 5), MDV = c(1, 1, 0), EVID = c(0, 0, 0),
                   LLOQ = c(1, 1, 1))
  result <- pmxhelpr:::df_prep_dvtime(df, "TIME", "NTIME", loq_method = 1)
  expect_equal(result$data$DV[1], 0)
  expect_equal(result$data$DV[2], 0.5)
  expect_equal(result$data$DV[3], 5)
  expect_equal(result$lloq, 1)
})

test_that("df_prep_dvtime applies dose normalization to all output vars", {
  df <- data.frame(TIME = 1:2, NTIME = 1:2, DV = c(10, 20), IPRED = c(12, 22),
                   MDV = c(0, 0), EVID = c(0, 0), DOSE = c(100, 100))
  result <- pmxhelpr:::df_prep_dvtime(df, "TIME", "NTIME",
                                        ipred_var_str = "IPRED",
                                        dosenorm = TRUE)
  expect_equal(result$data$DV, c(0.1, 0.2))
  expect_equal(result$data$IPRED, c(0.12, 0.22))
})

test_that("df_prep_dvtime coerces col_var to factor", {
  df <- data.frame(TIME = 1:2, NTIME = 1:2, DV = c(1, 2),
                   MDV = c(0, 0), EVID = c(0, 0), GRP = c("a", "b"))
  result <- pmxhelpr:::df_prep_dvtime(df, "TIME", "NTIME", col_var_str = "GRP")
  expect_true(is.factor(result$data$GRP))
})

test_that("df_prep_dvtime errors on missing dv_var", {
  df <- data.frame(TIME = 1, NTIME = 1, MDV = 0)
  expect_error(
    pmxhelpr:::df_prep_dvtime(df, "TIME", "NTIME"),
    regexp = "must be variable"
  )
})

test_that("df_prep_dvtime errors on invalid timeu", {
  df <- data.frame(TIME = 1, NTIME = 1, DV = 1, MDV = 0)
  expect_error(
    pmxhelpr:::df_prep_dvtime(df, "TIME", "NTIME", timeu = "years"),
    regexp = "argument timeu must be one of"
  )
})

test_that("df_prep_dvtime errors on missing dose_var when dosenorm = TRUE", {
  df <- data.frame(TIME = 1, NTIME = 1, DV = 1, MDV = 0)
  expect_error(
    pmxhelpr:::df_prep_dvtime(df, "TIME", "NTIME", dosenorm = TRUE, dose_var_str = "DOSE"),
    regexp = "must be variable"
  )
})

#####prep_plot_env####

test_that("prep_plot_env returns list with expected elements", {
  df <- data.frame(NTIME = c(0, 1, 2, 4, 8))
  result <- pmxhelpr:::prep_plot_env(df, cent = "mean", log_y = FALSE,
                                     obs_dv = TRUE, grp_dv = FALSE,
                                     timeu = "hours", n_breaks = 8,
                                     theme = NULL,
                                     theme_fn = pmxhelpr::plot_dvtime_theme)
  expect_true(is.list(result))
  expect_named(result, c("caption", "xbreaks", "plottheme", "width"))
  expect_true(is.character(result$caption))
  expect_true(is.numeric(result$xbreaks))
  expect_true(is.list(result$plottheme))
  expect_true(is.numeric(result$width))
})
