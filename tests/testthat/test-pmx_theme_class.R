##### pmx_theme class predicates #####

test_that("each plot_*_theme() factory satisfies its specific class AND is_pmx_theme()", {
  cases <- list(
    plot_dvtime_theme   = "plot_dvtime_theme",
    plot_gof_theme      = "plot_gof_theme",
    plot_dvconc_theme   = "plot_dvconc_theme",
    plot_doseprop_theme = "plot_doseprop_theme",
    plot_vpc_theme      = "plot_vpc_theme"
  )
  for (nm in names(cases)) {
    obj <- get(nm)()
    cls <- cases[[nm]]
    expect_true(inherits(obj, cls), info = paste0("specific class check failed on ", nm))
    expect_true(is_pmx_theme(obj),  info = paste0("is_pmx_theme failed on ", nm))
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

test_that("per-type theme classes are mutually exclusive", {
  expect_false(inherits(plot_vpc_theme(),    "plot_dvtime_theme"))
  expect_false(inherits(plot_dvtime_theme(), "plot_vpc_theme"))
  expect_false(inherits(plot_gof_theme(),    "plot_doseprop_theme"))
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


##### pmx_theme() public factory #####

test_that("pmx_theme() with no args returns an empty pmx_theme", {
  out <- pmx_theme()
  expect_s3_class(out, "pmx_theme")
  expect_length(out, 0)
})

test_that("pmx_theme() carries the requested subclass", {
  out <- pmx_theme(list(obs_point = pmx_point(color = "red")),
                   subclass = "plot_vpc_theme")
  expect_s3_class(out, "plot_vpc_theme")
  expect_s3_class(out, "pmx_theme")
  expect_equal(out$obs_point$color, "red")
})

test_that("pmx_theme() compacts NULL entries", {
  out <- pmx_theme(list(obs_point = pmx_point(size = 2),
                        loq_line = NULL))
  expect_named(out, "obs_point")
})

test_that("pmx_theme() rejects non-pmx_element entries", {
  expect_error(pmx_theme(list(obs_point = list(shape = 1))),
               regexp = "must be a `pmx_element`")
})

test_that("pmx_theme() rejects an unnamed list", {
  expect_error(pmx_theme(list(pmx_point())),
               regexp = "fully-named")
})


##### is_pmx_theme(strict = TRUE) #####

test_that("is_pmx_theme(strict = TRUE) accepts every plot_*_theme() output", {
  for (factory in list(plot_dvtime_theme, plot_gof_theme, plot_dvconc_theme,
                       plot_doseprop_theme, plot_vpc_theme)) {
    expect_true(is_pmx_theme(factory(), strict = TRUE))
  }
})

test_that("is_pmx_theme(strict = TRUE) rejects a malformed theme", {
  bad <- structure(list(obs_point = list(shape = 1)),
                   class = c("plot_vpc_theme", "pmx_theme"))
  expect_true(is_pmx_theme(bad))               # cheap check still passes
  expect_false(is_pmx_theme(bad, strict = TRUE))
})


##### +.pmx_theme #####

test_that("`+.pmx_theme` returns left side unchanged when right side is NULL", {
  base <- plot_vpc_theme()
  expect_identical(base + NULL, base)
})

test_that("`+.pmx_theme` overrides only the keys present on the right side", {
  base <- plot_vpc_theme()
  patch <- pmx_theme(list(obs_point = pmx_point(color = "red")))
  out   <- base + patch
  expect_equal(out$obs_point$color, "red")
  expect_equal(out$sim_pi_ci$fill, base$sim_pi_ci$fill)
})

test_that("`+.pmx_theme` preserves the class of the left side", {
  base <- plot_vpc_theme()
  patch <- pmx_theme(list(obs_point = pmx_point(color = "red")))
  out <- base + patch
  expect_s3_class(out, "plot_vpc_theme")
  expect_s3_class(out, "pmx_theme")
})

test_that("`+.pmx_theme` accepts a same-typed full theme on the right", {
  base <- plot_vpc_theme()
  out  <- base + plot_vpc_theme(obs_point = pmx_point(color = "navy"))
  expect_equal(out$obs_point$color, "navy")
  expect_s3_class(out, "plot_vpc_theme")
})

test_that("`+.pmx_theme` aborts on a non-pmx_theme right side", {
  expect_error(plot_vpc_theme() + 1L,
               regexp = "must be a `pmx_theme`")
})

test_that("`pmx_theme + pmx_element` errors via R's Ops dispatch", {
  ## Dispatch ambiguity: `+.pmx_theme` (LHS) and `+.pmx_element` (RHS) differ,
  ## so R falls back to the base `+`, which errors. The exact message comes
  ## from base R (not the custom abort), but the operation must not silently
  ## succeed.
  expect_error(suppressWarnings(plot_vpc_theme() + pmx_point()))
})
