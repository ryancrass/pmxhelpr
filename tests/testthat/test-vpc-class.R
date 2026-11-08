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
