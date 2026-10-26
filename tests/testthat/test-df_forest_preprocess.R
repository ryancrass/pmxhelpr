## Minimal wide replicate-simulation fixture: 2 replicates x 4 scenarios
## (reference, one FOOD level, two WTBL levels), 2 metrics.
sim_wide <- data.frame(
  SCN = rep(1:4, times = 2),
  SIM = rep(1:2, each = 4),
  FOOD = rep(c(0, 1, 0, 0), times = 2),
  WTBL = rep(c(70, 70, 50, 90), times = 2),
  AUC = c(100, 200, 125, 80, 110, 253, 132, 88),
  CMAX = c(10, 17, 13, 8, 11, 18, 14, 9)
)

ref_vals <- c(FOOD = 0, WTBL = 70)
labs <- list(
  FOOD = c("0" = "Fasted", "1" = "Fed"),
  WTBL = function(x) paste(x, "kg")
)

pre <- function(...) {
  df_forest_preprocess(
    sim_wide,
    metric_vars = c("AUC", "CMAX"),
    cov_vars = c("FOOD", "WTBL"),
    cov_val_ref = ref_vals,
    ...
  )
}


#####df_forest_preprocess -- output shape#####

test_that("Output is a long data.frame with passthrough and canonical columns", {
  out <- pre(cov_labels = labs)
  expect_s3_class(out, "data.frame")
  expect_equal(
    colnames(out),
    c("SCN", "SIM", "FOOD", "WTBL", "metric", "value", "cov_var", "cov_val")
  )
  # 8 input rows x (2 metrics + 2 ratio metrics)
  expect_equal(nrow(out), 8 * 4)
})

test_that("Each input row's metrics stay together, in the order requested", {
  out <- pre(cov_labels = labs)
  expect_equal(out$metric[1:4], c("AUC", "CMAX", "AUCRATIO", "CMAXRATIO"))
  expect_equal(unique(out$SCN[1:4]), 1)
})

test_that("Non-metric columns pass through unchanged", {
  out <- pre(cov_labels = labs)
  first <- out[out$metric == "AUC", ]
  expect_equal(first$SCN, sim_wide$SCN)
  expect_equal(first$SIM, sim_wide$SIM)
  expect_equal(first$FOOD, sim_wide$FOOD)
  expect_equal(first$WTBL, sim_wide$WTBL)
  expect_equal(first$value, sim_wide$AUC)
})


#####df_forest_preprocess -- scenario derivation#####

test_that("Reference rows are labeled with `cov_name_ref`", {
  out <- pre(cov_labels = labs)
  ref <- out[out$SCN == 1, ]
  expect_equal(unique(ref$cov_var), "Reference")
  expect_equal(unique(ref$cov_val), "Reference")
  expect_false(any(out$cov_var[out$SCN != 1] == "Reference"))
})

test_that("`cov_name_ref` renames the reference label", {
  out <- pre(cov_labels = labs, cov_name_ref = "Typical")
  expect_equal(unique(out$cov_var[out$SCN == 1]), "Typical")
  expect_equal(unique(out$cov_val[out$SCN == 1]), "Typical")
})

test_that("Each scenario is keyed to the one covariate it varies", {
  out <- pre(cov_labels = labs)
  scn <- unique(out[, c("SCN", "cov_var", "cov_val")])
  expect_equal(scn$cov_var, c("Reference", "FOOD", "WTBL", "WTBL"))
  expect_equal(scn$cov_val, c("Reference", "Fed", "50 kg", "90 kg"))
})

test_that("Covariate values match `cov_val_ref` on their character form", {
  chr <- sim_wide
  chr$FOOD <- as.character(chr$FOOD)
  out <- df_forest_preprocess(
    chr,
    metric_vars = "AUC", cov_vars = c("FOOD", "WTBL"),
    cov_val_ref = c(FOOD = 0, WTBL = 70)
  )
  expect_equal(sum(out$cov_var == "Reference" & out$metric == "AUC"), 2)
})

test_that("Large numeric covariate values match and label in fixed notation", {
  big <- data.frame(SIM = rep(1:2, each = 3), WT = rep(c(100000L, 50000L, 150000L), 2),
                    AUC = c(10, 8, 12, 11, 9, 13))
  out <- df_forest_preprocess(big, "AUC", "WT", c(WT = 100000))
  expect_equal(unique(out$cov_val), c("Reference", "50000", "150000"))
  expect_equal(attr(out, "cov_level_ref"), c(WT = "100000"))
  big$WT <- as.numeric(big$WT)
  out <- df_forest_preprocess(big, "AUC", "WT", c(WT = 100000))
  expect_equal(attr(out, "cov_level_ref"), c(WT = "100000"))
})

test_that("A scenario differing in two covariates aborts", {
  bad <- sim_wide
  bad$WTBL[bad$SCN == 2] <- 90
  expect_error(
    df_forest_preprocess(bad, metric_vars = "AUC", cov_vars = c("FOOD", "WTBL"),
                         cov_val_ref = ref_vals),
    "differ in more than one"
  )
})

test_that("A missing reference scenario aborts", {
  expect_error(
    df_forest_preprocess(sim_wide, metric_vars = "AUC", cov_vars = c("FOOD", "WTBL"),
                         cov_val_ref = c(FOOD = 0, WTBL = 60)),
    "no reference scenario found"
  )
})

test_that("A replicate without exactly one reference row aborts", {
  dup <- rbind(sim_wide, sim_wide[sim_wide$SCN == 1, ])
  expect_error(
    df_forest_preprocess(dup, metric_vars = "AUC", cov_vars = c("FOOD", "WTBL"),
                         cov_val_ref = ref_vals),
    "exactly one reference-scenario row"
  )
})

test_that("`NA` in a covariate column aborts", {
  na_cov <- sim_wide
  na_cov$FOOD[3] <- NA
  expect_error(
    df_forest_preprocess(na_cov, metric_vars = "AUC", cov_vars = c("FOOD", "WTBL"),
                         cov_val_ref = ref_vals),
    "must not contain `NA`"
  )
})


#####df_forest_preprocess -- ratios#####

test_that("Ratios divide by the same replicate's reference value", {
  out <- pre(cov_labels = labs)
  auc_ratio <- out[out$metric == "AUCRATIO", ]
  expect_equal(auc_ratio$value, sim_wide$AUC / rep(c(100, 110), each = 4))
  expect_equal(auc_ratio$value[auc_ratio$cov_var == "Reference"], c(1, 1))
})

test_that("`ratio = FALSE` emits absolute metrics only", {
  out <- pre(cov_labels = labs, ratio = FALSE)
  expect_equal(sort(unique(out$metric)), c("AUC", "CMAX"))
  expect_equal(nrow(out), 8 * 2)
})

test_that("`ratio_suffix` names the ratio metrics", {
  out <- pre(cov_labels = labs, ratio_suffix = "_REL")
  expect_true(all(c("AUC_REL", "CMAX_REL") %in% out$metric))
})

test_that("A ratio name already in `data` aborts, whether a metric or a passthrough column", {
  clash <- sim_wide
  clash$AUCRATIO <- 999
  expect_error(
    df_forest_preprocess(clash, metric_vars = c("AUC", "AUCRATIO"), cov_vars = c("FOOD", "WTBL"),
                         cov_val_ref = ref_vals),
    "would overwrite these columns of `data`: AUCRATIO"
  )
  expect_error(
    df_forest_preprocess(clash, metric_vars = "AUC", cov_vars = c("FOOD", "WTBL"),
                         cov_val_ref = ref_vals),
    "would overwrite these columns of `data`: AUCRATIO"
  )
  expect_no_error(
    df_forest_preprocess(clash, metric_vars = "AUC", cov_vars = c("FOOD", "WTBL"),
                         cov_val_ref = ref_vals, ratio = FALSE)
  )
})

test_that("`irep_name` is only required when ratios are requested", {
  no_sim <- sim_wide[sim_wide$SIM == 1, setdiff(names(sim_wide), "SIM")]
  expect_s3_class(
    df_forest_preprocess(no_sim, metric_vars = "AUC", cov_vars = c("FOOD", "WTBL"),
                         cov_val_ref = ref_vals, ratio = FALSE),
    "data.frame"
  )
  expect_error(
    df_forest_preprocess(no_sim, metric_vars = "AUC", cov_vars = c("FOOD", "WTBL"),
                         cov_val_ref = ref_vals),
    "irep_name"
  )
})


#####df_forest_preprocess -- labels#####

test_that("A named-vector labeller and a function labeller both apply", {
  out <- pre(cov_labels = labs)
  expect_equal(unique(out$cov_val[out$cov_var == "FOOD"]), "Fed")
  expect_equal(sort(unique(out$cov_val[out$cov_var == "WTBL"])), c("50 kg", "90 kg"))
})

test_that("Without `cov_labels` levels fall back to their character form", {
  out <- pre()
  expect_equal(unique(out$cov_val[out$cov_var == "FOOD"]), "1")
  expect_equal(sort(unique(out$cov_val[out$cov_var == "WTBL"])), c("50", "90"))
})

test_that("An unmapped covariate value warns and falls back", {
  expect_warning(
    out <- pre(cov_labels = list(FOOD = c("0" = "Fasted"))),
    "no entry for these covariate values"
  )
  expect_equal(unique(out$cov_val[out$cov_var == "FOOD"]), "1")
})


#####df_forest_preprocess -- cov_level_ref attribute#####

test_that("The `cov_level_ref` attribute labels each covariate's reference value", {
  out <- pre(cov_labels = labs)
  expect_equal(attr(out, "cov_level_ref"), c(FOOD = "Fasted", WTBL = "70 kg"))
})

test_that("The attribute falls back to the character value with no labeller", {
  out <- pre()
  expect_equal(attr(out, "cov_level_ref"), c(FOOD = "0", WTBL = "70"))
})

test_that("The attribute feeds `df_forest()` as `cov_level_ref`", {
  out <- pre(cov_labels = labs)
  s <- pmxhelpr:::forest_rows(df_forest(out, cov_level_ref = attr(out, "cov_level_ref")))
  expect_equal(s$cov_val[s$metric == "AUCRATIO" & s$cov_var == "FOOD"],
               c("Fasted", "Fed"))
  expect_equal(s$cov_val[s$metric == "AUCRATIO" & s$cov_var == "WTBL"],
               c("50 kg", "70 kg", "90 kg"))
})


#####df_forest_preprocess -- argument validation#####

test_that("Column-name arguments accept bare names or strings", {
  expect_equal(pre(cov_labels = labs, irep_name = SIM),
               pre(cov_labels = labs, irep_name = "SIM"))
})

test_that("Output column names are configurable", {
  out <- pre(cov_labels = labs, metric_name_var = "PARAM",
             metric_value_var = "VAL")
  expect_true(all(c("PARAM", "VAL") %in% colnames(out)))
  expect_false("metric" %in% colnames(out))
})

test_that("An output name colliding with an input column warns", {
  clash <- sim_wide
  clash$metric <- "x"
  expect_warning(
    df_forest_preprocess(clash, metric_vars = "AUC", cov_vars = c("FOOD", "WTBL"),
                         cov_val_ref = ref_vals),
    "replaced by output columns"
  )
})

test_that("Malformed arguments abort", {
  expect_error(
    df_forest_preprocess(sim_wide, metric_vars = c("AUC", "AUC"),
                         cov_vars = c("FOOD", "WTBL"), cov_val_ref = ref_vals),
    "duplicate entries"
  )
  expect_error(
    df_forest_preprocess(sim_wide, metric_vars = "AUC", cov_vars = c("FOOD", "WTBL"),
                         cov_val_ref = c(FOOD = 0)),
    "missing a reference value"
  )
  expect_error(
    df_forest_preprocess(sim_wide, metric_vars = "AUC", cov_vars = "FOOD",
                         cov_val_ref = c(FOOD = 0, WTBL = 70)),
    "not in `cov_vars`"
  )
  expect_error(pre(cov_labels = list(FOOD = 1:2)), "must be a function or a")
  expect_error(pre(cov_labels = labs, ratio = "yes"), "`TRUE` or `FALSE`")
  expect_error(pre(cov_labels = labs, ratio_suffix = ""), "non-empty character string")
  expect_error(
    df_forest_preprocess(sim_wide, metric_vars = "SCN", cov_vars = c("FOOD", "WTBL"),
                         cov_val_ref = ref_vals, ratio = FALSE),
    NA
  )
  chr_metric <- sim_wide
  chr_metric$AUC <- as.character(chr_metric$AUC)
  expect_error(
    df_forest_preprocess(chr_metric, metric_vars = "AUC", cov_vars = c("FOOD", "WTBL"),
                         cov_val_ref = ref_vals),
    "must name numeric columns"
  )
})


#####df_forest_preprocess -- round trip#####

test_that("Output feeds `df_forest()` with its default column names", {
  out <- pre(cov_labels = labs)
  s <- df_forest(out)
  expect_s3_class(s, "forest_stats")
  expect_true(all(s$stats$is_ref[s$stats$cov_var == "Reference"]))
})

test_that("The shipped dataset is reproduced by the shipped raw data", {
  out <- df_forest_preprocess(
    data_sad_pkforest_sim,
    metric_vars = c("CMAX", "CMIN", "AUC", "CAVG"),
    cov_vars = c("FOOD", "WTBL"),
    cov_val_ref = c(FOOD = 0, WTBL = 70),
    cov_labels = list(
      FOOD = c("0" = "Fasted", "1" = "Fed"),
      WTBL = function(x) paste(x, "kg")
    )
  )
  expect_equal(out, data_sad_pkforest)
})
