##### Tests for the `vpc_stats` S3 class and validator #####

testsim <- df_mrgsim_replicate(data = data_sad,
                               model = model_mread_load("pkmodel"),
                               replicates = 5,
                               dv_var = "ODV")

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

test_that("print.vpc_stats(n_head = 0) suppresses the head preview", {
  out <- df_vpcstats(testsim)
  txt <- paste(capture.output(print(out, n_head = 0)), collapse = "\n")
  expect_no_match(txt, "head\\(stats")
})

test_that("summary.vpc_stats matches print(n_head = 0)", {
  out <- df_vpcstats(testsim)
  summary_txt <- paste(capture.output(summary(out)), collapse = "\n")
  print_no_head_txt <- paste(capture.output(print(out, n_head = 0)),
                             collapse = "\n")
  expect_identical(summary_txt, print_no_head_txt)
})

test_that("summary.vpc_stats output is stable (snapshot)", {
  ## Snapshots `summary()` rather than `print()` because `summary()` skips
  ## the head preview and is therefore deterministic across simulation
  ## seeds — only run config and column-group structure are emitted.
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
               regexp = "must be a `vpc_stats` object")
  expect_error(pmxhelpr:::validate_vpc_stats(NULL),
               regexp = "must be a `vpc_stats` object")
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
               regexp = "missing required columns: pc_obs_med")
})

test_that("validate_vpc_stats rejects a vpc_stats list missing required obs columns", {
  out <- df_vpcstats(testsim)
  bad <- out
  bad$obs <- dplyr::select(bad$obs, -dplyr::any_of("PC_OBSDV"))
  expect_error(pmxhelpr:::validate_vpc_stats(bad),
               regexp = "missing required columns: PC_OBSDV")
})

test_that("validate_vpc_stats rejects a vpc_stats container missing config keys", {
  out <- df_vpcstats(testsim)
  bad <- out
  bad$config$loq <- NULL
  bad$config[["loq"]] <- NULL
  bad$config <- bad$config[setdiff(names(bad$config), "loq")]
  expect_error(pmxhelpr:::validate_vpc_stats(bad),
               regexp = "missing required keys: loq")
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

test_that("plot_build_vpc() inherits strat_var from the stats attribute when not passed", {
  testsim_strat <- df_mrgsim_replicate(data = data_sad,
                                       model = model_mread_load("pkmodel"),
                                       replicates = 5,
                                       dv_var = "ODV",
                                       char_vars = "FOOD")
  testsim_strat <- dplyr::mutate(testsim_strat, FOOD_f = factor(FOOD))
  out <- df_vpcstats(testsim_strat, strat_var = FOOD_f)
  p <- plot_build_vpc(out)  # no strat_var passed
  expect_s3_class(p, "ggplot")
  expect_true("FacetWrap" %in% class(p$facet))
})

test_that("plot_build_vpc() accepts strat_var as a bare name (NSE)", {
  testsim_strat <- df_mrgsim_replicate(data = data_sad,
                                       model = model_mread_load("pkmodel"),
                                       replicates = 5,
                                       dv_var = "ODV",
                                       char_vars = "FOOD")
  testsim_strat <- dplyr::mutate(testsim_strat, FOOD_f = factor(FOOD))
  out <- df_vpcstats(testsim_strat, strat_var = FOOD_f)
  p_bare <- plot_build_vpc(out, strat_var = FOOD_f)
  p_str  <- plot_build_vpc(out, strat_var = "FOOD_f")
  expect_s3_class(p_bare, "ggplot")
  expect_s3_class(p_str,  "ggplot")
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
