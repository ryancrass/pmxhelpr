#####autoplot methods for the stats containers####

## Same layers and labels as the builder called with the same arguments.
expect_same_plot <- function(p, q) {
  expect_equal(capture.output(layer_summary(p)), capture.output(layer_summary(q)))
  expect_equal(lapply(p$labels, unclass), lapply(q$labels, unclass))
}

test_that("autoplot() on a vpc_stats matches plot_build_vpc()", {
  vs <- df_vpcstats(sim_pk(), loq = 1)
  expect_same_plot(autoplot(vs), plot_build_vpc(vs))
  expect_same_plot(autoplot(vs, type = "cens"), plot_build_vpc(vs, type = "cens"))
  expect_same_plot(autoplot(vs, pcvpc = TRUE), plot_build_vpc(vs, pcvpc = TRUE))
  expect_same_plot(autoplot(vs, loq = NULL), plot_build_vpc(vs, loq = NULL))
  expect_true(is_pmx_vpc_plot(autoplot(vs)))
})

test_that("autoplot() on a doseprop_stats matches plot_build_doseprop()", {
  ds <- df_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"))
  expect_same_plot(autoplot(ds), plot_build_doseprop(ds))
  expect_same_plot(autoplot(ds, se = FALSE), plot_build_doseprop(ds, se = FALSE))
})

test_that("autoplot() on a forest_stats matches plot_build_forest()", {
  fs <- df_forest(data_sad_pkforest)
  expect_same_plot(autoplot(fs, metric = "AUCRATIO"),
                   plot_build_forest(fs, metric = "AUCRATIO"))
  expect_same_plot(autoplot(fs, metric = "CMAX", ref_band = c(0.8, 1.25)),
                   plot_build_forest(fs, metric = "CMAX", ref_band = c(0.8, 1.25)))
})

test_that("autoplot() propagates the builder's classed errors", {
  vs <- df_vpcstats(sim_pk())
  expect_error(autoplot(vs, type = "x"))
  expect_error(autoplot(df_forest(data_sad_pkforest)), "metric",
               class = "pmxhelpr_error_bad_arg")
})

test_that("autoplot is re-exported from ggplot2", {
  expect_true("autoplot" %in% getNamespaceExports("pmxhelpr"))
  expect_identical(pmxhelpr::autoplot, ggplot2::autoplot)
})
