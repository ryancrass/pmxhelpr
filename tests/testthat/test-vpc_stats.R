##### Tests for the `vpc_stats` S3 class and validator #####

testsim <- sim_pk(5, carry_out = "FOOD")

test_that("df_vpcstats() output carries the vpc_stats and pmx_stats classes", {
  out <- df_vpcstats(testsim)
  expect_s3_class(out, "vpc_stats")
  expect_s3_class(out, "pmx_stats")
})

test_that("is_vpc_stats() distinguishes vpc_stats objects from inner data.frames", {
  out <- df_vpcstats(testsim)
  expect_true(is_vpc_stats(out))
  expect_false(is_vpc_stats(out$stats))
  expect_false(is_vpc_stats(out$obs))
  expect_false(is_vpc_stats(NULL))
  expect_false(is_vpc_stats(list(stats = data.frame(), obs = data.frame())))
})

test_that("is_vpc_stats(strict = TRUE) catches structurally invalid objects", {
  out <- df_vpcstats(testsim)
  expect_true(is_vpc_stats(out, strict = TRUE))
  bad <- structure(list(stats = data.frame(), obs = data.frame(), config = list()),
                   class = c("vpc_stats", "pmx_stats"))
  expect_true(is_vpc_stats(bad))               # cheap check still passes
  expect_false(is_vpc_stats(bad, strict = TRUE))
})

test_that("is_pmx_stats() detects the shared base class", {
  out <- df_vpcstats(testsim)
  expect_true(is_pmx_stats(out))
  expect_false(is_pmx_stats(out$stats))
  expect_false(is_pmx_stats(NULL))
})

#####validate_pmx_stats generic####

test_that("validate_pmx_stats() returns each real container invisibly", {
  containers <- list(df_vpcstats(testsim),
                     df_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax")),
                     df_forest(data_sad_pkforest))
  for (x in containers) {
    expect_invisible(validate_pmx_stats(x))
    expect_identical(validate_pmx_stats(x), x)
  }
})

test_that("validate_pmx_stats() applies each subclass contract", {
  vs <- df_vpcstats(testsim)
  vs$stats$obs_med <- NULL
  expect_error(validate_pmx_stats(vs), "`vs\\$stats` is missing required columns: `obs_med`",
               class = "pmxhelpr_error_bad_stats")
  fs <- df_forest(data_sad_pkforest)
  fs$stats$est <- as.character(fs$stats$est)
  expect_error(validate_pmx_stats(fs), "`fs\\$stats` column `est` must be numeric",
               class = "pmxhelpr_error_bad_stats")
  ds <- df_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"))
  ds$config$dose_var <- "NOPE"
  expect_error(validate_pmx_stats(ds), "`ds\\$obs`", class = "pmxhelpr_error_missing_var")
})

test_that("validate_pmx_stats() default method checks the shared shape", {
  expect_error(validate_pmx_stats(list()), "must be a <pmx_stats> object",
               class = "pmxhelpr_error_bad_stats")
  expect_error(validate_pmx_stats(mtcars), "`mtcars` must be a <pmx_stats> object",
               class = "pmxhelpr_error_bad_stats")
  other <- pmx_stats(data.frame(a = 1), subclass = "other_stats")
  expect_identical(validate_pmx_stats(other), other)
  other$config <- "x"
  expect_error(validate_pmx_stats(other), "`other\\$config` must be a <list>",
               class = "pmxhelpr_error_bad_stats")
})

test_that("validate_pmx_stats() errors name the caller", {
  fs <- df_forest(data_sad_pkforest)
  fs$stats$est <- as.character(fs$stats$est)
  f <- function(s) validate_pmx_stats(s)
  cnd <- rlang::catch_cnd(f(fs), classes = "error")
  expect_equal(as.character(cnd$call[[1]]), "f")
})

test_that("pmx_stats() and is_pmx_stats(strict = TRUE) apply the subclass contract", {
  expect_error(pmx_stats(data.frame(a = 1), subclass = "forest_stats"),
               "missing required columns", class = "pmxhelpr_error_bad_stats")
  fs <- df_forest(data_sad_pkforest)
  expect_true(is_pmx_stats(fs, strict = TRUE))
  fs$stats$est <- as.character(fs$stats$est)   # shared slots still valid
  expect_true(is_pmx_stats(fs))
  expect_false(is_pmx_stats(fs, strict = TRUE))
})

#####summary objects####

test_that("summary() returns a summary object per container class", {
  vs <- df_vpcstats(testsim)
  ds <- df_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"))
  fs <- df_forest(data_sad_pkforest)
  for (x in list(vs, ds, fs)) {
    s <- summary(x)
    expect_s3_class(s, c(paste0("summary.", class(x)[1]), "summary.pmx_stats"), exact = TRUE)
    expect_equal(s$header$class, class(x)[1])
    expect_equal(s$header$dim, dim(x$stats))
    expect_invisible(print(s))
  }
})

test_that("summary.vpc_stats holds the header config and column groups", {
  s <- summary(df_vpcstats(testsim, pi = c(0.1, 0.9)))
  expect_named(s$header$config, c("n_replicates", "ci", "pi", "mode", "loq", "strat_var"))
  expect_null(s$header$config$strat_var)
  expect_true("strat_var" %in% names(s$header$config))
  expect_equal(s$header$config$pi, c(0.1, 0.9))
  expect_equal(s$header$obs_rows, nrow(df_vpcstats(testsim)$obs))
  expect_equal(s$column_groups$counts, pmxhelpr:::.vpc_count_cols)
  expect_true("pc_obs_med" %in% s$column_groups$pc_observed)
  expect_identical(s$column_groups$sim_blq, character(0))
  s_loq <- summary(df_vpcstats(testsim, strat_var = "FOOD", loq = 1))
  expect_true("FOOD" %in% s_loq$column_groups$identifiers)
  expect_true("sim_prop_blq_med" %in% s_loq$column_groups$sim_blq)
})

test_that("summary.doseprop_stats holds one row per metric", {
  ds <- df_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"))
  m <- summary(ds)$metrics
  expect_named(m, c("metric", "Power", "LCL", "UCL", "PowerCI", "Interpretation"))
  expect_equal(m$metric, c("aucinf.obs", "cmax"))
  expect_equal(m$Power, ds$stats$Power)
  expect_equal(summary(ds)$header$obs_rows, nrow(ds$obs))
})

test_that("summary.forest_stats holds the metric table and reference layout", {
  fs <- df_forest(dplyr::filter(data_sad_pkforest, metric == "AUCRATIO"),
                  cov_level_ref = c(FOOD = "Fasted", WTBL = "70 kg"))
  s <- summary(fs)
  expect_equal(s$metrics$metric, "AUCRATIO")
  expect_equal(c(s$metrics$n_min, s$metrics$n_max), range(fs$stats$n))
  expect_equal(s$metrics$panels,
               "Reference | FOOD: Fasted*, Fed | WTBL: 50 kg, 60 kg, 70 kg*, 80 kg, 90 kg")
  expect_true(s$has_distributed)
  expect_false(summary(df_forest(data_sad_pkforest, cov_name_ref = NULL))$has_distributed)
  ## an n range prints as min-max; no n column prints no count
  fs$stats$n[2] <- 10L
  expect_output(print(summary(fs)), "\\[AUCRATIO\\]  n = 10-500  Reference")
  fs$stats$n <- NULL
  expect_true(is.na(summary(fs)$metrics$n_min))
  expect_output(print(summary(fs)), "\\[AUCRATIO\\]  Reference")
})

test_that("summary.pmx_stats is the fallback for other subclasses", {
  other <- pmx_stats(data.frame(a = 1:2), config = list(k = 1), subclass = "other_stats")
  s <- summary(other)
  expect_s3_class(s, "summary.pmx_stats", exact = TRUE)
  expect_equal(s$classes, c("other_stats", "pmx_stats"))
  expect_equal(capture.output(print(s)), capture.output(print(other)))
  expect_output(print(s), "config: k")
})

test_that("print.vpc_stats emits the headline summary and column groups", {
  out <- df_vpcstats(testsim, strat_var = "FOOD", loq = 1)
  txt <- paste(capture.output(print(out)), collapse = "\n")
  expect_match(txt, "<vpc_stats>")
  expect_match(txt, "stats: \\d+ rows x \\d+ columns")
  expect_match(txt, "obs:\\s+\\d+ rows")
  expect_match(txt, "n_replicates = 5")
  expect_match(txt, "loq = 1")
  expect_match(txt, "strat_var = FOOD")
  expect_match(txt, "std observed")
  expect_match(txt, "pc simulated")
  expect_match(txt, "Use `x\\$stats` and `x\\$obs`")
})

test_that("df_vpcstats records the run configuration in config", {
  sim <- dplyr::rename(testsim, TAD = TIME, REP = SIM)
  out <- df_vpcstats(sim, time_var = TAD, rep_var = "REP", pi = c(0.1, 0.9),
                     ci = 0.8, mode = "drop")
  expect_equal(out$config$ci, 0.8)
  expect_equal(out$config$pi, c(0.1, 0.9))
  expect_equal(out$config$mode, "drop")
  expect_equal(out$config[c("time_var", "ntime_var", "pred_var", "sim_dv_var",
                            "obs_dv_var", "rep_var")],
               list(time_var = "TAD", ntime_var = "NTIME", pred_var = "PRED",
                    sim_dv_var = "SIMDV", obs_dv_var = "OBSDV", rep_var = "REP"))
  expect_equal(df_vpcstats(testsim)$config$mode, "auto")
})

test_that("validate_vpc_stats requires ci and pi in config", {
  out <- df_vpcstats(testsim)
  out$config$ci <- NULL
  expect_error(pmxhelpr:::validate_vpc_stats(out), "missing required keys: `ci`",
               class = "pmxhelpr_error_bad_stats")
  expect_error(plot_build_vpc(out), class = "pmxhelpr_error_bad_stats")
})

test_that("print.vpc_stats shows ci, pi, and mode in the config line", {
  out <- df_vpcstats(testsim, pi = c(0.1, 0.9))
  expect_output(print(out, n_head = 0), "ci = 0.9, pi = c\\(0.1, 0.9\\), mode = auto")
})

test_that("print.vpc_stats(n_head = 0) suppresses the head preview", {
  out <- df_vpcstats(testsim)
  txt <- paste(capture.output(print(out, n_head = 0)), collapse = "\n")
  expect_no_match(txt, "head\\(stats")
})

test_that("summary.vpc_stats prints print(n_head = 0) plus the coverage tables", {
  out <- df_vpcstats(testsim)
  summary_lines <- capture.output(summary(out))
  print_lines <- capture.output(print(out, n_head = 0))
  cov_start <- grep("^  CI coverage", summary_lines)
  pi_start <- grep("^  PI coverage", summary_lines)
  expect_equal(pi_start, cov_start + 5) # CI title, column header, three quantiles
  cov_end <- pi_start + 2               # PI title, column header, one line
  expect_identical(summary_lines[-(cov_start:cov_end)], print_lines)
  expect_match(summary_lines[cov_start], "inside its 90% simulated CI")
  expect_match(summary_lines[pi_start],
               "outside the 5%-95% simulated PI; expected 5% below, 5% above")
})

test_that("summary.vpc_stats CI coverage counts bins inside the simulated CI", {
  ## Two bins, one per stratum. Std: low inside, med on the CI edge (counts as
  ## inside), hi outside. pc: observed low NA (not counted), med inside, and
  ## the hi CI bound NA (not counted).
  stats <- data.frame(
    BIN_MID = c(1, 1), FOOD = c("Fasted", "Fed"),
    obs_low = c(1, 1), sim_low_low = c(0, 0), sim_low_hi = c(2, 2),
    obs_med = c(5, 5), sim_med_low = c(5, 5), sim_med_hi = c(6, 6),
    obs_hi  = c(9, 9), sim_hi_low  = c(7, 7), sim_hi_hi  = c(8, 8),
    pc_obs_low = c(NA, 1), pc_sim_low_low = c(0, 0), pc_sim_low_hi = c(2, 2),
    pc_obs_med = c(5, 5),  pc_sim_med_low = c(4, 4), pc_sim_med_hi = c(6, 6),
    pc_obs_hi  = c(9, 9),  pc_sim_hi_low  = c(7, 7), pc_sim_hi_hi  = c(NA, NA)
  )
  cov <- pmxhelpr:::vpc_ci_coverage(stats, "FOOD", c(0.05, 0.95))
  expect_named(cov, c("bins", "strata", "overall"))

  ## Per-bin flags: NA where a side is NA.
  expect_named(cov$bins, c("BIN_MID", "FOOD", "quantile", "prob", "type", "pcvpc", "inside"))
  expect_equal(nrow(cov$bins), 12)
  fasted_bins <- cov$bins[cov$bins$FOOD == "Fasted", ]
  expect_equal(fasted_bins$quantile, rep(c("low", "med", "hi"), each = 2))
  expect_equal(fasted_bins$prob, rep(c(0.05, 0.5, 0.95), each = 2))
  expect_equal(fasted_bins$type, rep("cont", 6))
  expect_equal(fasted_bins$pcvpc, rep(c(FALSE, TRUE), 3))
  expect_equal(fasted_bins$inside, c(TRUE, NA, TRUE, TRUE, FALSE, NA))

  ## Per-stratum shares count only the non-NA flags.
  expect_named(cov$strata, c("FOOD", "quantile", "prob", "type", "pcvpc", "n_bins", "n_inside", "coverage"))
  fasted <- cov$strata[cov$strata$FOOD == "Fasted", ]
  expect_equal(fasted$n_bins,   c(1, 0, 1, 1, 1, 0))
  expect_equal(fasted$n_inside, c(1, 0, 1, 1, 0, 0))
  expect_equal(fasted$coverage, c(1, NA, 1, 1, 0, NA))
  expect_equal(cov$strata$n_bins[cov$strata$FOOD == "Fed" & cov$strata$quantile == "low"], c(1, 1))

  ## Overall pools both strata.
  expect_named(cov$overall, c("quantile", "prob", "type", "pcvpc", "n_bins", "n_inside", "coverage"))
  expect_equal(cov$overall$n_bins,   c(2, 1, 2, 2, 2, 0))
  expect_equal(cov$overall$n_inside, c(2, 1, 2, 2, 0, 0))
  expect_equal(cov$overall$coverage, c(1, 1, 1, 1, 0, NA))

  unstrat <- pmxhelpr:::vpc_ci_coverage(stats, NULL, c(0.05, 0.95))
  expect_named(unstrat$bins, c("BIN_MID", "quantile", "prob", "type", "pcvpc", "inside"))
  expect_null(unstrat$strata)
  expect_equal(unstrat$overall, cov$overall)
})

test_that("summary.vpc_stats CI coverage flags the cens VPC median line when a LOQ is given", {
  stats <- data.frame(
    BIN_MID = c(1, 2, 3),
    obs_low = 1, sim_low_low = 0, sim_low_hi = 2,
    obs_med = 5, sim_med_low = 4, sim_med_hi = 6,
    obs_hi  = 9, sim_hi_low  = 8, sim_hi_hi  = 10,
    pc_obs_low = 1, pc_sim_low_low = 0, pc_sim_low_hi = 2,
    pc_obs_med = 5, pc_sim_med_low = 4, pc_sim_med_hi = 6,
    pc_obs_hi  = 9, pc_sim_hi_low  = 8, pc_sim_hi_hi  = 10,
    obs_prop_blq     = c(0.2, 0.6, NaN),
    sim_prop_blq_low = c(0.1, 0.1, 0.1),
    sim_prop_blq_med = c(0.3, 0.3, 0.3),
    sim_prop_blq_hi  = c(0.5, 0.5, 0.5)
  )
  cov <- pmxhelpr:::vpc_ci_coverage(stats, NULL, c(0.05, 0.95))
  cens <- cov$bins[cov$bins$type == "cens", ]
  expect_equal(cens$inside, c(TRUE, FALSE, NA))
  expect_true(all(cens$quantile == "med"))
  expect_true(all(is.na(cens$prob)))
  expect_false(any(cens$pcvpc))
  row <- cov$overall[cov$overall$type == "cens", ]
  expect_equal(nrow(row), 1)
  expect_equal(c(row$n_bins, row$n_inside, row$coverage), c(2, 1, 0.5))
  expect_equal(paste(cov$overall$quantile, cov$overall$type, cov$overall$pcvpc),
               c("low cont FALSE", "low cont TRUE", "med cont FALSE", "med cont TRUE",
                 "med cens FALSE", "hi cont FALSE", "hi cont TRUE"))

  ## Without the sim_prop_blq_* columns (no LOQ) there is no cens row.
  no_loq <- pmxhelpr:::vpc_ci_coverage(stats[!grepl("prop_blq", names(stats))], NULL, c(0.05, 0.95))
  expect_false("cens" %in% no_loq$bins$type)

  ## Printed as a cens column filled on the median line only.
  txt <- capture.output(pmxhelpr:::cat_vpc_ci_coverage(cov, 0.9, NULL))
  expect_match(txt[2], "quantile +std +pc +cens$")
  expect_match(txt[3], "^    5% .*%$")
  expect_match(txt[4], "^    50% .*1/2 +50%$")
  expect_no_match(capture.output(pmxhelpr:::cat_vpc_ci_coverage(no_loq, 0.9, NULL))[2], "cens")
})

test_that("summary.vpc_stats CI coverage flags every bin and prints strata then Overall", {
  out <- df_vpcstats(testsim, strat_var = "FOOD", pi = c(0.1, 0.9))
  cov <- summary(out)$ci_coverage
  expect_equal(nrow(cov$bins), nrow(out$stats) * 3 * 2)
  expect_equal(nrow(cov$strata), length(unique(out$stats$FOOD)) * 3 * 2)
  expect_equal(nrow(cov$overall), 3 * 2)
  expect_equal(sort(unique(cov$overall$prob)), c(0.1, 0.5, 0.9))
  expect_false("cens" %in% cov$overall$type)
  cov_loq <- summary(df_vpcstats(testsim, strat_var = "FOOD", loq = 1))$ci_coverage
  expect_equal(nrow(cov_loq$strata), length(unique(out$stats$FOOD)) * 7)
  expect_equal(sum(cov_loq$overall$type == "cens"), 1)
  expect_true(all(cov$strata$n_inside <= cov$strata$n_bins))
  ## Overall is the sum of the strata.
  pooled <- stats::aggregate(cbind(n_bins, n_inside) ~ quantile + type + pcvpc, cov$strata, sum)
  key <- function(d) paste(d$quantile, d$type, d$pcvpc)
  expect_equal(pooled$n_bins[match(key(cov$overall), key(pooled))], cov$overall$n_bins)
  expect_equal(pooled$n_inside[match(key(cov$overall), key(pooled))], cov$overall$n_inside)
  txt <- capture.output(print(summary(out)))
  expect_true(any(grepl("^    FOOD     quantile", txt)))
  expect_equal(sum(grepl("^    Overall +[0-9.]+% ", txt)), 3)  # CI table lines
  expect_true(any(grepl("10% ", txt)))
})

test_that("summary.vpc_stats PI coverage counts observations outside the simulated PI", {
  ## Three bins, PI 2-8 on both scales. Bin 1: one below, two on the bounds
  ## (inside), one above, one BLQ. Bins 2 and 3: one inside, two BLQ each; bin
  ## 3 has no pc interval.
  stats <- data.frame(
    BIN_MID = c(1, 2, 3), FOOD = c("Fasted", "Fasted", "Fed"),
    obs_n = c(5L, 3L, 3L), obs_n_blq = c(1L, 2L, 2L),
    sim_low_med = c(2, 2, 0.5), sim_hi_med = c(8, 8, 8),
    pc_sim_low_med = c(2, 2, NA), pc_sim_hi_med = c(8, 8, NA)
  )
  obs <- data.frame(
    BIN_MID = c(1, 1, 1, 1, 2, 3), FOOD = c(rep("Fasted", 5), "Fed"),
    OBSDV = c(1.5, 2, 8, 9, 5, 5), PC_OBSDV = c(5, 5, 9, 9, 1, 5), LOQ = 1
  )
  cov <- pmxhelpr:::vpc_pi_coverage(stats, obs, "FOOD", loq = 1)
  expect_named(cov, c("bins", "strata", "overall"))
  expect_named(cov$bins, c("BIN_MID", "FOOD", "pcvpc", "n_obs", "n_below", "n_above",
                           "prop_below", "prop_above"))
  std <- cov$bins[!cov$bins$pcvpc, ]
  ## BLQ are below where the lower bound (2) is at or above the LLOQ (1), and
  ## inside where it is under (0.5).
  expect_equal(std$n_obs,   c(5, 3, 3))
  expect_equal(std$n_below, c(2, 2, 0))
  expect_equal(std$n_above, c(1, 0, 0))
  expect_equal(std$prop_below, c(2 / 5, 2 / 3, 0))
  ## pc: quantifiable observations only, and no interval means not counted.
  pc <- cov$bins[cov$bins$pcvpc, ]
  expect_equal(pc$n_obs,   c(4, 1, 0))
  expect_equal(pc$n_below, c(0, 1, 0))
  expect_equal(pc$n_above, c(2, 0, 0))
  expect_equal(pc$prop_above, c(0.5, 0, NA))

  expect_named(cov$strata, c("FOOD", "pcvpc", "n_obs", "n_below", "n_above",
                             "prop_below", "prop_above"))
  fasted <- cov$strata[cov$strata$FOOD == "Fasted", ]
  expect_equal(fasted$n_obs,   c(8, 5))
  expect_equal(fasted$n_below, c(4, 1))
  expect_equal(cov$overall$n_obs,   c(11, 5))
  expect_equal(cov$overall$n_below, c(4, 1))
  expect_equal(cov$overall$n_above, c(1, 2))
  expect_equal(cov$overall$prop_below, c(4 / 11, 1 / 5))

  ## Without a LOQ only quantifiable observations are counted.
  no_loq <- pmxhelpr:::vpc_pi_coverage(stats, obs[names(obs) != "LOQ"], "FOOD", loq = NULL)
  expect_equal(no_loq$bins$n_obs[!no_loq$bins$pcvpc],   c(4, 1, 1))
  expect_equal(no_loq$bins$n_below[!no_loq$bins$pcvpc], c(1, 0, 0))

  unstrat <- pmxhelpr:::vpc_pi_coverage(stats[names(stats) != "FOOD"],
                                        obs[names(obs) != "FOOD"], NULL, loq = 1)
  expect_null(unstrat$strata)
  expect_named(unstrat$bins, c("BIN_MID", "pcvpc", "n_obs", "n_below", "n_above",
                               "prop_below", "prop_above"))
  expect_equal(unstrat$overall$n_obs, cov$overall$n_obs)

  ## Printed one line per stratum, then Overall; "-" where nothing is counted.
  txt <- capture.output(pmxhelpr:::cat_vpc_pi_coverage(cov, c(0.05, 0.95), "FOOD"))
  expect_match(txt[1], "outside the 5%-95% simulated PI; expected 5% below, 5% above")
  expect_match(txt[2], "^    FOOD +std below +std above +pc below +pc above$")
  expect_match(txt[3], "^    Fasted +4/8 +50% +1/8 +12% +1/5 +20% +2/5 +40%$")
  expect_match(txt[4], "^    Fed +0/3 +0% +0/3 +0% +- +-$")
  expect_match(txt[5], "^    Overall ")
  expect_length(txt, 5)
  expect_length(capture.output(pmxhelpr:::cat_vpc_pi_coverage(unstrat, c(0.1, 0.9), NULL)), 3)
})

test_that("summary.vpc_stats PI coverage counts every observation when there is a LOQ", {
  out <- df_vpcstats(testsim, strat_var = "FOOD", loq = 1)
  cov <- summary(out)$pi_coverage
  expect_equal(nrow(cov$bins), nrow(out$stats) * 2)
  expect_equal(nrow(cov$strata), length(unique(out$stats$FOOD)) * 2)
  expect_equal(cov$overall$pcvpc, c(FALSE, TRUE))
  expect_equal(cov$overall$n_obs[1], sum(out$stats$obs_n))
  expect_true(all(cov$bins$n_below + cov$bins$n_above <= cov$bins$n_obs))
  ## Overall is the sum of the strata.
  pooled <- stats::aggregate(cbind(n_obs, n_below, n_above) ~ pcvpc, cov$strata, sum)
  expect_equal(pooled$n_obs, cov$overall$n_obs)
  expect_equal(pooled$n_below, cov$overall$n_below)
  expect_equal(pooled$n_above, cov$overall$n_above)
  ## The std quantifiable counts match a direct count on the observations.
  j <- merge(out$obs, out$stats[c("BIN_MID", "FOOD", "sim_low_med", "sim_hi_med")])
  expect_equal(cov$overall$n_above[1], sum(j$OBSDV > j$sim_hi_med, na.rm = TRUE))
  no_loq <- summary(df_vpcstats(testsim, strat_var = "FOOD"))$pi_coverage
  expect_equal(no_loq$overall$n_below[1], sum(j$OBSDV < j$sim_low_med, na.rm = TRUE))
})

test_that("summary.vpc_stats output is stable (snapshot)", {
  ## Snapshots `summary()` rather than `print()` because `summary()` skips
  ## the head preview: it emits the run config, the column-group structure,
  ## and coverage counts (fixed by the fixture's seed).
  out <- df_vpcstats(testsim, strat_var = "FOOD", loq = 1)
  expect_snapshot(summary(out))
})

test_that("print.vpc_stats returns the input invisibly", {
  out <- df_vpcstats(testsim)
  ret <- withr::with_output_sink(tempfile(), print(out))
  expect_identical(ret, out)
})

test_that("as.data.frame.vpc_stats returns the stats data.frame, dropping the class", {
  out <- df_vpcstats(testsim)
  df  <- as.data.frame(out)
  expect_s3_class(df, "data.frame")
  expect_false(inherits(df, "vpc_stats"))
  expect_equal(nrow(df), nrow(out$stats))
  expect_equal(colnames(df), colnames(out$stats))
})

##### Tests for validate_vpc_stats() #####

test_that("validate_vpc_stats accepts a valid df_vpcstats() output", {
  out <- df_vpcstats(testsim)
  expect_silent(pmxhelpr:::validate_vpc_stats(out))
  expect_identical(pmxhelpr:::validate_vpc_stats(out), out)
})

test_that("validate_vpc_stats rejects a non-vpc_stats object", {
  expect_error(pmxhelpr:::validate_vpc_stats(list(stats = data.frame(),
                                                  obs   = data.frame())),
               regexp = "must be a <vpc_stats> object")
  expect_error(pmxhelpr:::validate_vpc_stats(NULL),
               regexp = "must be a <vpc_stats> object")
})

test_that("validate_vpc_stats rejects a vpc_stats list missing stats or obs", {
  bad_no_obs <- structure(list(stats = data.frame(BIN_MID = 1),
                               obs   = NULL,
                               config = list(n_replicates = 1, loq = NULL,
                                             strat_var = NULL)),
                          class = c("vpc_stats", "pmx_stats"))
  expect_error(pmxhelpr:::validate_vpc_stats(bad_no_obs),
               regexp = "must contain `stats` and `obs`")
})

test_that("validate_vpc_stats rejects a vpc_stats list missing required stats columns", {
  out <- df_vpcstats(testsim)
  bad <- out
  bad$stats <- dplyr::select(bad$stats, -dplyr::any_of(c("pc_obs_med")))
  expect_error(pmxhelpr:::validate_vpc_stats(bad),
               regexp = "missing required columns: `pc_obs_med`")
})

test_that("validate_vpc_stats rejects a vpc_stats list missing required obs columns", {
  out <- df_vpcstats(testsim)
  bad <- out
  bad$obs <- dplyr::select(bad$obs, -dplyr::any_of("PC_OBSDV"))
  expect_error(pmxhelpr:::validate_vpc_stats(bad),
               regexp = "missing required columns: `PC_OBSDV`")
})

test_that("validate_vpc_stats rejects a vpc_stats container missing config keys", {
  out <- df_vpcstats(testsim)
  bad <- out
  bad$config$loq <- NULL
  bad$config[["loq"]] <- NULL
  bad$config <- bad$config[setdiff(names(bad$config), "loq")]
  expect_error(pmxhelpr:::validate_vpc_stats(bad),
               regexp = "missing required keys: `loq`")
})

test_that("validate_vpc_stats rejects a container with loq set but sim_prop_blq_* missing", {
  out <- df_vpcstats(testsim, loq = 1)
  bad <- out
  bad$stats <- dplyr::select(bad$stats,
                             -dplyr::any_of(grep("^sim_prop_blq_",
                                                 colnames(bad$stats),
                                                 value = TRUE)))
  expect_error(pmxhelpr:::validate_vpc_stats(bad),
               regexp = "missing `sim_prop_blq_\\*` columns")
})

test_that("plot_vpc_cont via the precomputed path surfaces validator errors", {
  bad <- structure(list(stats = data.frame(BIN_MID = 1),
                        obs   = data.frame(),
                        config = list(n_replicates = 1, loq = NULL,
                                      strat_var = NULL)),
                   class = c("vpc_stats", "pmx_stats"))
  expect_error(plot_vpc_cont(bad),
               regexp = "missing required columns")
})

##### Tests for the public plot_build_vpc() #####

test_that("plot_build_vpc() is callable on a df_vpcstats() result", {
  out <- df_vpcstats(testsim)
  p <- plot_build_vpc(out)
  expect_s3_class(p, "ggplot")
})

test_that("plot_build_vpc() facets by config$strat_var and takes no strat_var argument", {
  testsim_strat <- sim_pk(5, carry_out = "FOOD")
  testsim_strat <- dplyr::mutate(testsim_strat, FOOD_f = factor(FOOD))
  out <- df_vpcstats(testsim_strat, strat_var = FOOD_f)
  p <- plot_build_vpc(out)
  expect_s3_class(p, "ggplot")
  expect_true("FacetWrap" %in% class(p$facet))
  layout <- suppressWarnings(ggplot2::ggplot_build(p))$layout$layout
  expect_setequal(as.character(layout$FOOD_f), levels(testsim_strat$FOOD_f))
  expect_error(plot_build_vpc(out, strat_var = "FOOD_f"), "unused argument")
  expect_error(plot_build_vpc(out, bin_var = "BIN_MID"), "unused argument")
})

test_that("plot_build_vpc() respects the pcvpc toggle on the same compute output", {
  out <- df_vpcstats(testsim, loq = 1)
  p_std <- plot_build_vpc(out, pcvpc = FALSE)
  p_pc  <- plot_build_vpc(out, pcvpc = TRUE)
  has_hline <- function(p) {
    any(vapply(p$layers, function(l) inherits(l$geom, "GeomHline"), logical(1)))
  }
  expect_true(has_hline(p_std))
  expect_false(has_hline(p_pc))
})

test_that("print.vpc_stats shows NULL loq / strat_var in the config line", {
  out <- df_vpcstats(testsim)
  out$config$loq <- NULL
  out$config$strat_var <- NULL
  expect_output(print(out, n_head = 0),
                "config: n_replicates = .*, loq = NULL, strat_var = NULL")
})


##### pmx_vpc_plot facet guard #####

vpc_plot <- function() suppressMessages(plot_vpc_cont(sim_pk(carry_out = "FOOD")))
facet_msg <- "incorrect VPC statistics"
n_panels <- function(p) nrow(suppressWarnings(ggplot2::ggplot_build(p))$layout$layout)

test_that("is_pmx_vpc_plot() identifies VPC plots only", {
  expect_true(is_pmx_vpc_plot(vpc_plot()))
  expect_true(is_pmx_vpc_plot(suppressMessages(plot_vpc_cens(sim_pk(carry_out = "LLOQ")))))
  expect_false(is_pmx_vpc_plot(ggplot2::ggplot()))
  expect_false(is_pmx_vpc_plot(sim_pk()))
})

test_that("adding a facet to a VPC warns, directly or inside a list()", {
  p <- vpc_plot()
  expect_warning(p + ggplot2::facet_wrap(~FOOD), facet_msg)
  expect_warning(p + ggplot2::facet_grid(FOOD ~ .), facet_msg)
  expect_warning(p + list(ggplot2::facet_wrap(~FOOD)), facet_msg)
})

test_that("non-facet additions do not warn and keep the class", {
  p <- vpc_plot()
  expect_no_warning(p1 <- p + ggplot2::theme_bw())
  expect_no_warning(p2 <- p + list(ggplot2::labs(x = "Time (h)")))
  expect_no_warning(p3 <- p + ggplot2::labs(x = "t") + ggplot2::scale_y_log10())
  expect_true(all(vapply(list(p1, p2, p3), is_pmx_vpc_plot, logical(1))))
  expect_warning(p3 + ggplot2::facet_wrap(~FOOD), facet_msg)
})

test_that("restyle_plot() keeps the class and warns when it sets a facet", {
  p <- vpc_plot()
  expect_no_warning(p_log <- restyle_plot(p, logy = TRUE))
  expect_true(is_pmx_vpc_plot(p_log))
  expect_warning(p_log + ggplot2::facet_wrap(~FOOD), facet_msg)
  expect_warning(p_fac <- restyle_plot(p, facet = "FOOD"), facet_msg)
  expect_true(is_pmx_vpc_plot(p_fac))
})

test_that("reveal() keeps the class, warns once for as = 'facet', and maps the column", {
  p <- vpc_plot()
  expect_warning(p_fac <- reveal(p, FOOD, as = "facet"), facet_msg)
  expect_true(is_pmx_vpc_plot(p_fac))
  expect_equal(n_panels(p_fac), 2)
  expect_no_warning(p_col <- reveal(p, FOOD, as = "colors"))
  expect_true(is_pmx_vpc_plot(p_col))
  expect_no_warning(reveal(p, "FOOD", as = "colors"))
})

##### irep_name -> rep_var (deprecated in 0.8.0) #####

test_that("irep_name is a deprecated alias of rep_var in the VPC functions", {
  withr::local_options(lifecycle_verbosity = "warning")
  simR <- dplyr::rename(sim_pk(5), REP = SIM)
  lifecycle::expect_deprecated(old <- df_vpcstats(simR, irep_name = "REP"), "rep_var")
  expect_identical(old, df_vpcstats(simR, rep_var = "REP"))
  lifecycle::expect_deprecated(old_bare <- df_vpcstats(simR, irep_name = REP))
  expect_identical(old_bare, old)
  expect_identical(old$config$rep_var, "REP")
  for (f in list(plot_vpc_cont, plot_vpc_cens)) {
    w <- testthat::capture_warnings(p_old <- f(simR, irep_name = "REP", loq = 1))
    expect_length(w, 1)                      # the wrapper warns once, not twice
    expect_equal(capture.output(layer_summary(p_old)),
                 capture.output(layer_summary(f(simR, rep_var = "REP", loq = 1))))
  }
  expect_match(conditionMessage(rlang::catch_cnd(plot_vpc_cont(simR, irep_name = "REP"),
                                                 classes = "warning")),
               "`plot_vpc_cont()`", fixed = TRUE)
  expect_error(df_vpcstats(simR, rep_var = "REP", irep_name = "REP"),
               "Pass `rep_var` only", class = "pmxhelpr_error_bad_arg")
})

test_that("irep_name is a deprecated alias of rep_var in df_mrgsim_replicate", {
  withr::local_options(lifecycle_verbosity = "warning")
  d <- dplyr::filter(data_sad, CMT %in% c(1, 2), ID %in% unique(ID)[1:3])
  mod <- model_mread_load(model = "pkmodel")
  lifecycle::expect_deprecated(
    old <- df_mrgsim_replicate(d, mod, 2, dv_var = "ODV", irep_name = "IREP"))
  expect_identical(old, df_mrgsim_replicate(d, mod, 2, dv_var = "ODV", rep_var = "IREP"))
  expect_true("IREP" %in% names(old))
})

test_that("irep_name is a deprecated alias of rep_var in df_forest_preprocess", {
  withr::local_options(lifecycle_verbosity = "warning")
  wide <- data.frame(SCN = rep(1:4, 2), REP = rep(1:2, each = 4),
                     FOOD = rep(c(0, 1, 0, 0), 2), WTBL = rep(c(70, 70, 50, 90), 2),
                     AUC = c(100, 200, 125, 80, 110, 253, 132, 88))
  pre <- function(...) df_forest_preprocess(wide, metric_vars = "AUC", cov_vars = c("FOOD", "WTBL"),
                                            cov_val_ref = c(FOOD = 0, WTBL = 70), ...)
  lifecycle::expect_deprecated(old <- pre(irep_name = REP))
  expect_identical(old, pre(rep_var = "REP"))
})


##### Parameter uncertainty: PRED differs by replicate #####

test_that("pc statistics are NA and pcvpc errors when PRED differs by replicate", {
  data <- dplyr::filter(data_sad, CMT %in% c(1, 2))
  model <- model_mread_load("pkmodel")
  draws <- data.frame(TVCL = c(15, 20, 25), TVVC = c(30, 35.7, 40))
  sim <- df_mrgsim_replicate(data, model, params = draws, dv_var = ODV)
  out <- df_vpcstats(sim, loq = 1)

  expect_false(out$config$pred_corrected)
  expect_true(all(is.na(out$stats[grep("^pc_", names(out$stats))])))
  expect_true(all(is.na(out$obs$PC_OBSDV)))
  expect_output(print(out), "pc columns are NA")
  expect_output(print(summary(out)), "pc columns are NA")

  ## The standard statistics do not depend on PRED.
  sim_base <- sim
  sim_base$PRED <- rep(sim$PRED[sim$SIM == 1], 3)
  base <- df_vpcstats(sim_base, loq = 1)
  expect_true(base$config$pred_corrected)
  std_cols <- grep("^pc_", names(out$stats), value = TRUE, invert = TRUE)
  expect_identical(out$stats[std_cols], base$stats[std_cols])

  expect_s3_class(plot_vpc_cont(out), "pmx_vpc_plot")
  expect_error(plot_vpc_cont(out, pcvpc = TRUE), "differs by replicate",
               class = "pmxhelpr_error_bad_stats")
  expect_error(plot_vpc_cont(sim, pcvpc = TRUE), "differs by replicate",
               class = "pmxhelpr_error_bad_stats")
})

test_that("pc statistics are computed when PRED is the same in every replicate", {
  data <- dplyr::filter(data_sad, CMT %in% c(1, 2))
  sim <- df_mrgsim_replicate(data, model_mread_load("pkmodel"), 3, dv_var = ODV)
  out <- df_vpcstats(sim)
  expect_true(out$config$pred_corrected)
  expect_false(all(is.na(out$stats$pc_obs_med)))
})


test_that("replicate ids need not start at 1", {
  sim <- sim_pk(3)
  shifted <- sim
  shifted$SIM <- shifted$SIM + 10L
  base <- df_vpcstats(sim, loq = 1)
  out <- df_vpcstats(shifted, loq = 1)
  expect_identical(out$stats, base$stats)
  expect_equal(nrow(out$obs), nrow(base$obs))
  expect_equal(out$config$n_replicates, 3)
})
