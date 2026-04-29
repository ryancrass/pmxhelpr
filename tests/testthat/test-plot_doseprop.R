#####mod_loglog#####

##Test Output
test_that("Output is a `lm` object", {
  expect_s3_class(mod_loglog(dplyr::filter(data_sad_nca, PPTESTCD == "aucinf.obs")),
                  "lm")
})


##Test Argument Handling
test_that("Error if argument `data` is not a `data.frame`", {
  expect_error(mod_loglog("data"),
               regexp = "argument `data` must be a `data.frame`")
})

test_that("Error if variable specified in argument `exp_var` is not in `data`", {
  expect_error(mod_loglog(dplyr::filter(data_sad_nca, PPTESTCD == "aucinf.obs"), exp_var = "param"),
               regexp = "argument `exp_var` must be variable.*in `data`")
})

test_that("Error if variable specified in argument `dose_var` is not in `data`", {
  expect_error(mod_loglog(dplyr::filter(data_sad_nca, PPTESTCD == "aucinf.obs"), dose_var = "DOSEN"),
               regexp = "argument `dose_var` must be variable.*in `data`")
})


#####df_loglog#####

##Test Output
test_that("Output is a `data.frame` object", {
  fit <- mod_loglog(dplyr::filter(data_sad_nca, PPTESTCD == "aucinf.obs"))

  expect_s3_class(df_loglog(fit),
                  "data.frame")
})


##Test Argument Handling
test_that("Error if argument `fit` is not a `lm` object", {
  fit <- mod_loglog(dplyr::filter(data_sad_nca, PPTESTCD == "aucinf.obs"))

  expect_error(df_loglog("fit"),
               regexp = "argument `fit` must be class `lm`")
})

test_that("Error if argument `method` not one of normal or tdist", {
  fit <- mod_loglog(dplyr::filter(data_sad_nca, PPTESTCD == "aucinf.obs"))

  expect_error(df_loglog(fit, method = 1),
               regexp = "argument `method` must be `normal` or `tdist`")
})

test_that("Error if argument `ci` is not numeric between 0 and 1", {
  fit <- mod_loglog(dplyr::filter(data_sad_nca, PPTESTCD == "aucinf.obs"))

  expect_error(df_loglog(fit, ci = 1.1),
               regexp = "argument `ci` must be a numeric value between 0 and 1")
})

test_that("Error if argument `sigdigits` is not coercible to an integer", {
  fit <- mod_loglog(dplyr::filter(data_sad_nca, PPTESTCD == "aucinf.obs"))

  expect_error(df_loglog(fit, sigdigits = "$"),
               regexp = "argument `sigdigits` must be coercible to class `integer`")
})


#####df_doseprop#####
##Test Output
test_that("Output is a `data.frame`", {
  expect_s3_class(df_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax")),
                  class = "data.frame")
})

test_that("Output countains variable specified in `metric_var`", {
  expect_named(df_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"), metric_var = "PPTESTCD") |>
                 dplyr::select(PPTESTCD),
                  "PPTESTCD")
})

test_that("Output countains anticipated variables", {
  expect_named(df_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"), metric_var = "PPTESTCD"),
               c("Intercept", "StandardError", "CI", "Power", "LCL", "UCL",
                 "Proportional","PowerCI", "Interpretation", "PPTESTCD"))
})

test_that("Output rounds values as specified in the `sigdigits` argument", {

  signdig <- function(x){length(gregexpr("[[:digit:]]", as.character(x))[[1]])}
  test <- df_doseprop(data_sad_nca, metrics = c("cmax"), metric_var = "PPTESTCD",
                      sigdigits = 4)$Intercept
  result <- signdig(test)
  expect_equal(result,4)
})



#####plot_doseprop#####

##Test Output
test_that("Output is a `ggplot` plot object", {
  expect_s3_class(plot_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax")),
               class = "ggplot")
})

test_that("Output plot maps variable specified in `dose_var` to the x aesthetic", {
  expect_equal(
    rlang::quo_name(plot_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"), dose_var = "DOSE")$mapping$x),
    "DOSE"
  )
})

test_that("Output plot maps variable specified in `exp_var` to the x aesthetic", {
  expect_equal(
    rlang::quo_name(plot_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"), exp_var = "PPORRES")$mapping$y),
    "PPORRES"
  )
})

test_that("Output plot dose nor include a confidence interval when argument `se` = FALSE", {
  expect_false(
    plot_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"), se = FALSE)$layers[[2]]$stat_params$se)
})


##Test Argument Handling
test_that("Error if incorrect class for argument `data`", {
  expect_error(plot_doseprop("data_sad_nca", metrics = c("aucinf.obs", "cmax")),
               regexp = "argument `data` must be a `data.frame`")
})

test_that("Error if variables specified in argument `metrics` are not levels in `data[[metric_var]]`", {
  expect_error(plot_doseprop(data_sad_nca, metrics = c("auc")),
               regexp = "argument `metrics` must be levels in variable `metric_var`")
})

test_that("Error if variable specified in `metric_var` is not in `data`", {
  expect_error(plot_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"), metric_var = "METRIC"),
               regexp = "argument `metric_var` must be variable.*in `data`")
})

test_that("Error if variable specified in `dose_var` is not in `data`", {
  expect_error(plot_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"), dose_var = "DOSEN"),
               regexp = "argument `dose_var` must be variable.*in `data`")
})

test_that("Error if argument `method` is not one of normal or tdist", {
  expect_error(plot_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"), method = 1),
               regexp = "argument `method` must be `normal` or `tdist`")
})

test_that("Error if argument `ci` is not numeric between 0 and 1", {
  expect_error(plot_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"), ci = 1.1),
               regexp = "argument `ci` must be a numeric value between 0 and 1")
})

test_that("Error if argument `sigdigits` is not coercible to an integer", {
  expect_error(plot_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"), sigdigits = "$"),
               regexp = "argument `sigdigits` must be coercible to class `integer`")
})

##Test NSE Bare Names
test_that("mod_loglog accepts bare names and matches string output", {
  dat <- dplyr::filter(data_sad_nca, PPTESTCD == "aucinf.obs")
  m1 <- mod_loglog(dat, exp_var = PPORRES, dose_var = DOSE)
  m2 <- mod_loglog(dat, exp_var = "PPORRES", dose_var = "DOSE")
  expect_identical(coef(m1), coef(m2))
})

test_that("df_doseprop accepts bare names and matches string output", {
  t1 <- df_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"),
                     metric_var = PPTESTCD, exp_var = PPORRES, dose_var = DOSE)
  t2 <- df_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"),
                     metric_var = "PPTESTCD", exp_var = "PPORRES", dose_var = "DOSE")
  expect_identical(t1, t2)
})

test_that("plot_doseprop accepts bare names", {
  expect_s3_class(
    plot_doseprop(dplyr::filter(data_sad_nca, PART == "Part 1-SAD"),
                  metrics = c("aucinf.obs", "cmax"),
                  metric_var = PPTESTCD, exp_var = PPORRES, dose_var = DOSE),
    "ggplot")
})
