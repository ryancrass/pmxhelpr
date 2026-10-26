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

#####normalize_time_unit####

test_that("normalize_time_unit returns canonical value for valid aliases", {
  expect_equal(pmxhelpr:::normalize_time_unit("hours"),  "hours")
  expect_equal(pmxhelpr:::normalize_time_unit("hrs"),    "hours")
  expect_equal(pmxhelpr:::normalize_time_unit("h"),      "hours")
  expect_equal(pmxhelpr:::normalize_time_unit("days"),   "days")
  expect_equal(pmxhelpr:::normalize_time_unit("wks"),    "weeks")
  expect_equal(pmxhelpr:::normalize_time_unit("mo"),     "months")
  expect_equal(pmxhelpr:::normalize_time_unit("months"), "months")
})

test_that("normalize_time_unit errors on invalid time unit", {
  expect_error(pmxhelpr:::normalize_time_unit("years"),
               regexp = "argument `unit` must be one of")
  expect_error(pmxhelpr:::normalize_time_unit("years", name = "timeu"),
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

test_that("df_prep_dvtime messages when inheriting loq from LLOQ column", {
  df <- data.frame(TIME = c(0, 1, 2), NTIME = c(0, 1, 2),
                   DV = c(NA, NA, 5), MDV = c(1, 1, 0), EVID = c(0, 0, 0),
                   LLOQ = c(1, 1, 1))
  expect_message(
    pmxhelpr:::df_prep_dvtime(df, "TIME", "NTIME", loq_method = 1),
    regexp = "Inheriting per-row `loq` from `LLOQ` column in `data`"
  )
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

#####df_prep_blq#####

test_that("df_prep_blq with loq_method = 0 is a no-op", {
  d <- dplyr::rename(dplyr::filter(data_sad, CMT %in% c(1, 2)), DV = ODV)
  out <- pmxhelpr:::df_prep_blq(d, loq = 10, loq_method = 0)
  expect_identical(out, d)
})

test_that("df_prep_blq loq_method = 1 imputes 0 pre-dose and 0.5*LOQ post-dose for MDV==1", {
  d <- dplyr::rename(dplyr::filter(data_sad, CMT %in% c(1, 2)), DV = ODV)
  out <- pmxhelpr:::df_prep_blq(d, loq = 10, loq_method = 1)
  pre  <- out$DV[out$MDV == 1 & out$EVID == 0 & out$TIME <= 0]
  post <- out$DV[out$MDV == 1 & out$EVID == 0 & out$TIME >  0]
  expect_true(length(pre)  > 0 && all(pre  == 0))
  expect_true(length(post) > 0 && all(post == 5))
})

test_that("df_prep_blq loq_method = 2 imputes 0.5*LOQ for all MDV==1 rows", {
  d <- dplyr::rename(dplyr::filter(data_sad, CMT %in% c(1, 2)), DV = ODV)
  out <- pmxhelpr:::df_prep_blq(d, loq = 10, loq_method = 2)
  blq <- out$DV[out$MDV == 1 & out$EVID == 0]
  expect_true(length(blq) > 0 && all(blq == 5))
})

test_that("df_prep_blq loq_method = 2 lifts pred_vars below LOQ to 0.5*LOQ", {
  d <- dplyr::rename(dplyr::filter(data_sad, CMT %in% c(1, 2)), DV = ODV)
  d$PRED <- d$DV
  out <- pmxhelpr:::df_prep_blq(d, loq = 10, loq_method = 2,
                                pred_vars = "PRED")
  obs <- out[out$EVID == 0, ]
  below <- obs$PRED[!is.na(obs$PRED) & obs$PRED < 10]
  expect_true(length(below) > 0 && all(below == 5))
})

test_that("df_prep_blq messages when inheriting loq from LLOQ column", {
  d <- dplyr::rename(dplyr::filter(data_sad, CMT %in% c(1, 2)), DV = ODV)
  expect_message(
    pmxhelpr:::df_prep_blq(d, loq = NULL, loq_method = 1),
    regexp = "Inheriting per-row `loq` from `LLOQ` column in `data`"
  )
})

#####internal check_* helpers#####

test_that("check_modlib errors with the model name when the path is missing", {
  expect_error(pmxhelpr:::check_modlib("nope", tempfile()),
               regexp = "`nope`.*does not exist")
})

test_that("check_mrgmod accepts an mrgmod object", {
  m <- model_mread_load("pkmodel")
  expect_silent(pmxhelpr:::check_mrgmod(m, "model"))
})

test_that("check_mrgmod errors on non-mrgmod input", {
  expect_error(pmxhelpr:::check_mrgmod(list(), "model"),
               regexp = "must be class `mrgmod`")
})

test_that("check_mrgmod_outputvars errors when neither sim_dv_var nor ipred_var is captured", {
  m <- model_mread_load("pkmodel")
  expect_error(
    pmxhelpr:::check_mrgmod_outputvars(m, "DOES_NOT_EXIST", "ALSO_NOT"),
    regexp = "must contain output variables"
  )
})

test_that("check_capture errors with the variable name when missing from capture", {
  m <- model_mread_load("pkmodel")
  expect_error(
    pmxhelpr:::check_capture(m, "NOPE", "pkmodel", "sim_dv_var"),
    regexp = "must be captured as output"
  )
})

test_that("check_loglog_args errors on bad method", {
  expect_error(pmxhelpr:::check_loglog_args("other", 0.95, 3),
               regexp = "must be `normal` or `tdist`")
})

test_that("check_loglog_args errors on ci outside (0,1)", {
  expect_error(pmxhelpr:::check_loglog_args("normal", 1.5, 3),
               regexp = "argument `ci` must be a single numeric value in \\(0, 1\\)")
})

test_that("check_loglog_args accepts valid arguments silently", {
  expect_silent(pmxhelpr:::check_loglog_args("normal", 0.95, 3))
  expect_silent(pmxhelpr:::check_loglog_args("tdist",  0.90, 4))
})

test_that("check_pipeline_args_dropped accepts plot-only args silently", {
  fake_call <- quote(plot_vpc_cont(data = stats_obj, style = my_style))
  expect_silent(pmxhelpr:::check_pipeline_args_dropped(
    fake_call, plot_only_args = c("data", "style"), fn_name = "plot_vpc_cont"))
})

test_that("check_pipeline_args_dropped aborts with the disallowed arg names", {
  fake_call <- quote(plot_vpc_cont(data = stats_obj, loq = 1, mode = "drop"))
  expect_error(
    pmxhelpr:::check_pipeline_args_dropped(
      fake_call, plot_only_args = c("data", "style"), fn_name = "plot_vpc_cont"),
    regexp = "loq|mode"
  )
})

#####check_single_cmt####

test_that("check_single_cmt warns when multiple CMT values present", {
  df <- data.frame(CMT = c(2, 2, 3, 3), EVID = 0)
  expect_warning(pmxhelpr:::check_single_cmt(df),
                 regexp = "Multiple unique values of `CMT`")
})

test_that("check_single_cmt is silent when only one CMT present", {
  df <- data.frame(CMT = c(2, 2, 2), EVID = 0)
  expect_no_warning(pmxhelpr:::check_single_cmt(df))
})

test_that("check_single_cmt is silent when CMT column is absent", {
  df <- data.frame(TIME = 1:3, EVID = 0)
  expect_no_warning(pmxhelpr:::check_single_cmt(df))
})

test_that("check_single_cmt is silent on empty data", {
  df <- data.frame(CMT = integer(0), EVID = integer(0))
  expect_no_warning(pmxhelpr:::check_single_cmt(df))
})


##Test label_sort_num(): shared numeric sort key for display labels
test_that("label_sort_num honors the NA contract for unparseable input", {
  # Both callers gate on NA to decide whether to sort numerically at all, so a
  # sentinel (Inf, 0) here would silently change behavior at both call sites.
  got <- pmxhelpr:::label_sort_num(c(NA_character_, "", "-", "--", "  ", "Fasted"))
  expect_true(all(is.na(got)))
  expect_type(got, "double")
})

test_that("label_sort_num reads the first number, sign included", {
  expect_equal(
    pmxhelpr:::label_sort_num(c("-5 kg", "10 kg", "-20 kg", "-0.5",
                                "Age 65 y", "WT70", "2.5 mg", "70 kg")),
    c(-5, 10, -20, -0.5, 65, 70, 2.5, 70)
  )
})

test_that("label_sort_num treats a non-sign hyphen as a hyphen", {
  # Compound word and range separator: the number is positive in both.
  expect_equal(pmxhelpr:::label_sort_num(c("Weight-50 kg", "50-60 kg")), c(50, 50))
  # ... but a real leading sign still reads as negative.
  expect_equal(pmxhelpr:::label_sort_num("-50 kg"), -50)
})

test_that("label_sort_num agrees with the legacy leading-number regex", {
  # order_col_factor() used to gate on `^\\s*-?[0-9]` and parse with the regex
  # below. Dropping that gate is only safe because the shared key returns the
  # same value for every label the gate admitted -- pinned here so the
  # guarantee lives in the suite, not just in review.
  legacy <- function(x) {
    suppressWarnings(as.numeric(sub("^\\s*(-?[0-9]+\\.?[0-9]*).*$", "\\1", x)))
  }
  corpus <- c("5 mg", "20 mg", "100 mg", "2.5 mg", "0.5 mg", "-5 kg", "-20 kg",
              "100 mg (n=6)", "20 mg (n=12)", "5-10 mg", "70 kg", "0 kg",
              " 15 mg", "1000", "12.75 ng/mL", "-0.5", "3 mg/kg", "5 mg q12h",
              "-0", "0", "7.", "10e3")
  expect_true(all(grepl("^\\s*-?[0-9]", corpus)))   # all admitted by the old gate
  expect_equal(pmxhelpr:::label_sort_num(corpus), legacy(corpus))
})


##Test order_col_factor(): factor level order feeds ggstylekit (>= 0.3.0)
test_that("order_col_factor preserves an existing factor's declared levels", {
  f <- factor(c("b", "a", "b"), levels = c("b", "a"))
  expect_equal(levels(pmxhelpr:::order_col_factor(f)), c("b", "a"))
})

test_that("order_col_factor sorts numeric col_var by magnitude, not input order", {
  expect_equal(
    levels(pmxhelpr:::order_col_factor(c(100, 20, 5, 400, 50))),
    c("5", "20", "50", "100", "400")
  )
})

test_that("order_col_factor orders numeric-labelled character col_var by value", {
  # dose-like labels sort by the leading number, not lexicographically
  expect_equal(
    levels(pmxhelpr:::order_col_factor(c("200 mg", "5 mg", "20 mg", "100 mg"))),
    c("5 mg", "20 mg", "100 mg", "200 mg")
  )
  # trailing text (e.g. var_addn "(n=)" counts) does not disturb the ordering
  expect_equal(
    levels(pmxhelpr:::order_col_factor(c("100 mg (n=6)", "20 mg (n=12)", "5 mg (n=3)"))),
    c("5 mg (n=3)", "20 mg (n=12)", "100 mg (n=6)")
  )
  # decimals
  expect_equal(
    levels(pmxhelpr:::order_col_factor(c("2.5 mg", "10 mg", "0.5 mg"))),
    c("0.5 mg", "2.5 mg", "10 mg")
  )
})

test_that("order_col_factor sorts labels whose number is not leading", {
  # Previously these failed the `starts with a digit` gate and kept
  # first-appearance order, so "Dose 10 mg" sorted above "Dose 5 mg".
  expect_equal(
    levels(pmxhelpr:::order_col_factor(c("Dose 10 mg", "Dose 5 mg"))),
    c("Dose 5 mg", "Dose 10 mg")
  )
  expect_equal(
    levels(pmxhelpr:::order_col_factor(c("Week 2", "Week 10", "Week 1"))),
    c("Week 1", "Week 2", "Week 10")
  )
  # A hyphen in the label is not a minus sign.
  expect_equal(
    levels(pmxhelpr:::order_col_factor(c("Weight-50 kg", "Weight-10 kg"))),
    c("Weight-10 kg", "Weight-50 kg")
  )
})

test_that("order_col_factor falls back to first-appearance for non-numeric labels", {
  expect_equal(
    levels(pmxhelpr:::order_col_factor(c("High", "Low", "Medium"))),
    c("High", "Low", "Medium")
  )
  # mixed numeric / non-numeric is ambiguous -> first-appearance, never lexicographic
  expect_equal(
    levels(pmxhelpr:::order_col_factor(c("Placebo", "20 mg", "5 mg"))),
    c("Placebo", "20 mg", "5 mg")
  )
})

test_that("order_col_factor keeps first-appearance order when labels share a number", {
  lab <- c("Mild (60-89)", "Moderate (30-59)", "Severe (<30)")
  expect_equal(levels(pmxhelpr:::order_col_factor(lab)), lab)
})


##Test fmt_sig(): fixed-significant-digit label formatter
test_that("fmt_sig() keeps trailing zeros, rounds to sigdigits, and passes NA through", {
  expect_equal(fmt_sig(c(1, 0.8, 23312, 9969, 2.334, NA), 3),
               c("1.00", "0.800", "23300", "9970", "2.33", NA))
  expect_equal(fmt_sig(1.5, 2), "1.5")
  expect_equal(fmt_sig(0.0001234, 3), "0.000123")
  expect_equal(fmt_sig(c(0, -17.24), 3), c("0.00", "-17.2"))
  expect_equal(fmt_sig(0, 1), "0")
  # rounding up a magnitude takes the rounded value's decimals
  expect_equal(fmt_sig(c(0.9996, 9996), 3), c("1.00", "10000"))
})

test_that("fmt_sig() with a shared anchor gives every value the same decimal places", {
  expect_equal(fmt_sig(c(2.321, 0.903, 0.873), 3, anchor = 1.1), c("2.32", "0.90", "0.87"))
  expect_equal(fmt_sig(c(28812, 9470), 3, anchor = 12459), c("28800", "9500"))
  # a zero or non-finite anchor falls back to `sigdigits - 1` decimals
  expect_equal(fmt_sig(c(1.234, 0.5), 3, anchor = 0), c("1.23", "0.50"))
  expect_equal(fmt_sig(1.234, 3, anchor = NA), "1.23")
  # rounding to a negative zero prints without the sign
  expect_equal(fmt_sig(-0.001, 3, anchor = 1), "0.00")
})

