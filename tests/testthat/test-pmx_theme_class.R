##### pmx_theme class predicates #####

test_that("each plot_*_theme() factory satisfies its specific predicate AND is_pmx_theme()", {
  cases <- list(
    plot_dvtime_theme   = list(obj = plot_dvtime_theme(),   pred = is_plot_dvtime_theme),
    plot_gof_theme      = list(obj = plot_gof_theme(),      pred = is_plot_gof_theme),
    plot_dvconc_theme   = list(obj = plot_dvconc_theme(),   pred = is_plot_dvconc_theme),
    plot_doseprop_theme = list(obj = plot_doseprop_theme(), pred = is_plot_doseprop_theme),
    plot_vpc_theme      = list(obj = plot_vpc_theme(),      pred = is_plot_vpc_theme)
  )
  for (nm in names(cases)) {
    obj <- cases[[nm]]$obj
    pred <- cases[[nm]]$pred
    expect_true(pred(obj),         info = paste0("specific predicate failed on ", nm))
    expect_true(is_pmx_theme(obj), info = paste0("is_pmx_theme failed on ", nm))
  }
})

test_that("is_pmx_theme() distinguishes themes from elements", {
  expect_false(is_pmx_theme(pmx_point()))
  expect_false(is_pmx_theme(pmx_line()))
  expect_false(is_pmx_theme(list(obs_point = pmx_point())))
  expect_false(is_pmx_theme(NULL))
})

test_that("is_pmx_element() rejects pmx_theme objects", {
  expect_false(is_pmx_element(plot_dvtime_theme()))
  expect_false(is_pmx_element(plot_vpc_theme()))
})

test_that("per-type theme predicates are mutually exclusive", {
  expect_false(is_plot_dvtime_theme(plot_vpc_theme()))
  expect_false(is_plot_vpc_theme(plot_dvtime_theme()))
  expect_false(is_plot_doseprop_theme(plot_gof_theme()))
})


##### print.pmx_theme #####

test_that("print.pmx_theme() writes a banner and per-key element lines", {
  out <- capture.output(print(plot_dvtime_theme()))
  expect_true(any(grepl("<plot_dvtime_theme>", out, fixed = TRUE)))
  expect_true(any(grepl("obs_point", out, fixed = TRUE)))
  expect_true(any(grepl("<pmx_point>", out, fixed = TRUE)))
})

test_that("print.pmx_theme() works on every theme factory", {
  factories <- list(plot_dvtime_theme, plot_gof_theme, plot_dvconc_theme,
                    plot_doseprop_theme, plot_vpc_theme)
  for (factory in factories) {
    expect_no_error(capture.output(print(factory())))
  }
})

test_that("print.pmx_theme() shows VPC ribbon entries with their type", {
  out <- capture.output(print(plot_vpc_theme()))
  expect_true(any(grepl("<plot_vpc_theme>", out, fixed = TRUE)))
  expect_true(any(grepl("<pmx_ribbon>", out, fixed = TRUE)))
})


##### Class propagation through merge_theme #####

test_that("merge_theme preserves the class of the default theme", {
  defaults <- plot_dvtime_theme()
  merged <- pmxhelpr:::merge_theme(list(ref_line = pmx_line(linewidth = 99)),
                                   defaults)
  expect_s3_class(merged, "plot_dvtime_theme")
  expect_s3_class(merged, "pmx_theme")
})
