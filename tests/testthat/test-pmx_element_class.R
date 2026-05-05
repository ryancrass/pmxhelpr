##### pmx_element class predicates #####

test_that("each pmx_* constructor satisfies its specific predicate AND is_pmx_element()", {
  cases <- list(
    pmx_point    = list(obj = pmx_point(),    pred = is_pmx_point),
    pmx_line     = list(obj = pmx_line(),     pred = is_pmx_line),
    pmx_ribbon   = list(obj = pmx_ribbon(),   pred = is_pmx_ribbon),
    pmx_errorbar = list(obj = pmx_errorbar(), pred = is_pmx_errorbar),
    pmx_trend    = list(obj = pmx_trend(),    pred = is_pmx_trend),
    pmx_style    = list(obj = pmx_style(),    pred = is_pmx_style),
    pmx_color    = list(obj = pmx_color(),    pred = is_pmx_color)
  )
  for (nm in names(cases)) {
    obj <- cases[[nm]]$obj
    pred <- cases[[nm]]$pred
    expect_true(pred(obj),             info = paste0("specific predicate failed on ", nm))
    expect_true(is_pmx_element(obj),   info = paste0("is_pmx_element failed on ", nm))
  }
})

test_that("is_pmx_element() rejects plain lists and unrelated objects", {
  expect_false(is_pmx_element(list(shape = 1)))
  expect_false(is_pmx_element(mtcars))
  expect_false(is_pmx_element(NULL))
})

test_that("per-type predicates are mutually exclusive across element types", {
  expect_false(is_pmx_point(pmx_line()))
  expect_false(is_pmx_line(pmx_point()))
  expect_false(is_pmx_ribbon(pmx_trend()))
  expect_false(is_pmx_errorbar(pmx_style()))
})


##### print.pmx_element #####

test_that("print.pmx_element() writes a banner and the set fields", {
  out <- capture.output(print(pmx_point(shape = 16, size = 2)))
  expect_true(any(grepl("<pmx_point>", out, fixed = TRUE)))
  expect_true(any(grepl("shape = 16", out, fixed = TRUE)))
  expect_true(any(grepl("size = 2", out, fixed = TRUE)))
})

test_that("print.pmx_element() reports (no fields set) for empty elements", {
  out <- capture.output(print(pmx_line()))
  expect_true(any(grepl("<pmx_line>", out, fixed = TRUE)))
  expect_true(any(grepl("(no fields set)", out, fixed = TRUE)))
})

test_that("print.pmx_element() works on every element type", {
  ctors <- list(pmx_point, pmx_line, pmx_ribbon, pmx_errorbar,
                pmx_trend, pmx_style, pmx_color)
  for (ctor in ctors) {
    expect_no_error(capture.output(print(ctor())))
  }
})
