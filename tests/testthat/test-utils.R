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

test_that("check_varsindf names the missing column(s) in the error", {
  expect_error(pmxhelpr:::check_varsindf(data.frame(x = 1), "TFD", "data", "time_var"),
               regexp = "not found: 'TFD'")
})

test_that("check_varsindf lists all missing columns when multiple are absent", {
  expect_error(
    pmxhelpr:::check_varsindf(data.frame(x = 1), c("WT", "AGE", "SEX"), "data", "num_vars"),
    regexp = "'WT'.*'AGE'.*'SEX'"
  )
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
               regexp = "argument `timeu` must be one of")
})

#####check_loq_method####

test_that("check_loq_method does not error when loq_method = 0", {
  expect_no_error(pmxhelpr:::check_loq_method(NULL, 0, data.frame()))
})

test_that("check_loq_method errors when loq_method = NULL", {
  expect_error(pmxhelpr:::check_loq_method(NULL, NULL, data.frame()),
               regexp = "argument `loq_method` must be")
})

test_that("check_loq_method errors when loq_method = 1, loq = NULL, and no LLOQ column", {
  expect_error(pmxhelpr:::check_loq_method(NULL, 1, data.frame(x = 1)),
               regexp = "argument `loq` must be numeric or variable `LLOQ` must be present")
})

test_that("check_loq_method resolves string aliases to numeric", {
  expect_equal(pmxhelpr:::check_loq_method(NULL, "none", data.frame()), 0)
  expect_equal(pmxhelpr:::check_loq_method(1, "postdose", data.frame(LLOQ = 1)), 1)
  expect_equal(pmxhelpr:::check_loq_method(1, "all", data.frame(LLOQ = 1)), 2)
})

test_that("check_loq_method errors on invalid string", {
  expect_error(pmxhelpr:::check_loq_method(NULL, "invalid", data.frame()),
               regexp = "argument `loq_method` must be")
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

test_that("df_prep_dvtime errors on missing dose_var when dosenorm = TRUE", {
  df <- data.frame(TIME = 1, NTIME = 1, DV = 1, MDV = 0, EVID = 0)
  expect_error(
    pmxhelpr:::df_prep_dvtime(df, "TIME", "NTIME", dosenorm = TRUE, dose_var_str = "DOSE"),
    regexp = "argument `dose_var` must be variable"
  )
})

test_that("df_prep_dvtime filters EVID != 0 rows", {
  df <- data.frame(TIME = c(0, 1, 2), NTIME = c(0, 1, 2),
                   DV = c(NA, 5, 10), MDV = c(1, 0, 0),
                   EVID = c(1, 0, 0))
  result <- pmxhelpr:::df_prep_dvtime(df, "TIME", "NTIME")
  expect_equal(nrow(result$data), 2L)
  expect_true(all(result$data$EVID == 0))
})

test_that("df_prep_dvtime errors on missing EVID column", {
  df <- data.frame(TIME = 1, NTIME = 1, DV = 1, MDV = 0)
  expect_error(
    pmxhelpr:::df_prep_dvtime(df, "TIME", "NTIME"),
    regexp = "argument `EVID` must be variable"
  )
})

#####prep_plot_env####

test_that("prep_plot_env returns list with expected elements", {
  df <- data.frame(NTIME = c(0, 1, 2, 4, 8))
  result <- pmxhelpr:::prep_plot_env(df, cent = "mean", log_y = FALSE,
                                     theme = NULL,
                                     theme_fn = pmxhelpr::plot_dvtime_theme)
  expect_true(is.list(result))
  expect_named(result, c("caption", "plottheme", "width"))
  expect_true(is.character(result$caption))
  expect_true(is.list(result$plottheme))
  expect_true(is.numeric(result$width))
})

#####check_color####

test_that("check_color accepts valid color names and hex strings", {
  expect_no_error(pmxhelpr:::check_color("red", "color"))
  expect_no_error(pmxhelpr:::check_color("#FF0000", "color"))
})

test_that("check_color accepts NULL", {
  expect_no_error(pmxhelpr:::check_color(NULL, "color"))
})

test_that("check_color errors on invalid color name", {
  expect_error(pmxhelpr:::check_color("saalmon", "color"),
               regexp = "must be a valid color name or hex string")
})

#####check_size####

test_that("check_size accepts non-negative numeric and NULL", {
  expect_no_error(pmxhelpr:::check_size(1.5, "size"))
  expect_no_error(pmxhelpr:::check_size(0, "size"))   # zero is the "hide layer" idiom
  expect_no_error(pmxhelpr:::check_size(NULL, "size"))
})

test_that("check_size errors on negative or NA", {
  expect_error(pmxhelpr:::check_size(-1, "size"), regexp = "non-negative numeric")
  expect_error(pmxhelpr:::check_size(NA_real_, "size"), regexp = "non-negative numeric")
})

#####check_shape####

test_that("check_shape accepts integer in 0:25, character, and NULL", {
  expect_no_error(pmxhelpr:::check_shape(16, "shape"))
  expect_no_error(pmxhelpr:::check_shape("circle", "shape"))
  expect_no_error(pmxhelpr:::check_shape(NULL, "shape"))
})

test_that("check_shape errors on out-of-range integer", {
  expect_error(pmxhelpr:::check_shape(99, "shape"), regexp = "integer in 0:25")
  expect_error(pmxhelpr:::check_shape(-1, "shape"), regexp = "integer in 0:25")
})

test_that("check_shape errors on logical input", {
  expect_error(pmxhelpr:::check_shape(TRUE, "shape"), regexp = "integer in 0:25 or a character")
})

#####check_quantile_pair####

test_that("check_quantile_pair accepts ordered length-2 numeric in [0,1]", {
  expect_no_error(pmxhelpr:::check_quantile_pair(c(0.05, 0.95), "pi"))
})

test_that("check_quantile_pair errors on length != 2", {
  expect_error(pmxhelpr:::check_quantile_pair(0.5, "pi"), regexp = "length-2 numeric")
  expect_error(pmxhelpr:::check_quantile_pair(c(0.1, 0.5, 0.9), "pi"), regexp = "length-2 numeric")
})

test_that("check_quantile_pair errors on values outside [0,1]", {
  expect_error(pmxhelpr:::check_quantile_pair(c(-0.1, 0.5), "pi"), regexp = "in \\[0, 1\\]")
  expect_error(pmxhelpr:::check_quantile_pair(c(0.1, 1.5), "pi"), regexp = "in \\[0, 1\\]")
})

test_that("check_quantile_pair errors on reversed pair", {
  expect_error(pmxhelpr:::check_quantile_pair(c(0.95, 0.05), "pi"),
               regexp = "must be ordered")
})

test_that("check_quantile_pair errors on NA", {
  expect_error(pmxhelpr:::check_quantile_pair(c(0.05, NA), "pi"),
               regexp = "no NAs")
})

#####check_quantile_scalar####

test_that("check_quantile_scalar accepts scalar in (0,1)", {
  expect_no_error(pmxhelpr:::check_quantile_scalar(0.9, "ci"))
})

test_that("check_quantile_scalar rejects boundary values", {
  expect_error(pmxhelpr:::check_quantile_scalar(0, "ci"),
               regexp = "in \\(0, 1\\)")
  expect_error(pmxhelpr:::check_quantile_scalar(1, "ci"),
               regexp = "in \\(0, 1\\)")
})

test_that("check_quantile_scalar rejects vector input", {
  expect_error(pmxhelpr:::check_quantile_scalar(c(0.5, 0.95), "ci"),
               regexp = "single numeric")
})

