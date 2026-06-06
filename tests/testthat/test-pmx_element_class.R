##### pmx_element class predicates #####

test_that("each pmx_* constructor satisfies its specific class AND is_pmx_element()", {
  cases <- list(
    pmx_point    = list(obj = pmx_point(),    cls = "pmx_point"),
    pmx_line     = list(obj = pmx_line(),     cls = "pmx_line"),
    pmx_ribbon   = list(obj = pmx_ribbon(),   cls = "pmx_ribbon"),
    pmx_errorbar = list(obj = pmx_errorbar(), cls = "pmx_errorbar"),
    pmx_trend    = list(obj = pmx_trend(),    cls = "pmx_trend"),
    pmx_style    = list(obj = pmx_style(),    cls = "pmx_style"),
    pmx_color    = list(obj = pmx_color(),    cls = "pmx_color")
  )
  for (nm in names(cases)) {
    obj <- cases[[nm]]$obj
    cls <- cases[[nm]]$cls
    expect_true(inherits(obj, cls),    info = paste0("specific class check failed on ", nm))
    expect_true(is_pmx_element(obj),   info = paste0("is_pmx_element failed on ", nm))
  }
})

test_that("is_pmx_element() rejects plain lists and unrelated objects", {
  expect_false(is_pmx_element(list(shape = 1)))
  expect_false(is_pmx_element(mtcars))
  expect_false(is_pmx_element(NULL))
})

test_that("per-type classes are mutually exclusive across element types", {
  expect_false(inherits(pmx_line(),  "pmx_point"))
  expect_false(inherits(pmx_point(), "pmx_line"))
  expect_false(inherits(pmx_trend(), "pmx_ribbon"))
  expect_false(inherits(pmx_style(), "pmx_errorbar"))
})


##### pmx_color() variadic #####

test_that("pmx_color() accepts arbitrary named entries", {
  pc <- pmx_color(FOOD = "firebrick", WTBL = "steelblue", Reference = "grey20")
  expect_s3_class(pc, "pmx_color")
  expect_equal(pc$FOOD, "firebrick")
  expect_equal(pc$WTBL, "steelblue")
  expect_equal(pc$Reference, "grey20")
})

test_that("pmx_color() preserves the legacy dv/pred/ipred form", {
  pc <- pmx_color(dv = "blue", pred = "red", ipred = "green")
  expect_equal(pc$dv, "blue")
  expect_equal(pc$pred, "red")
  expect_equal(pc$ipred, "green")
})

test_that("pmx_color() rejects unnamed arguments", {
  expect_error(pmx_color("red", "blue"), regexp = "must be named")
})

test_that("pmx_color() validates color strings", {
  expect_error(pmx_color(FOOD = "not_a_color"))
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

test_that("print.pmx_element output is stable (snapshot)", {
  expect_snapshot(print(pmx_point(shape = 16, size = 2)))
  expect_snapshot(print(pmx_line()))
  expect_snapshot(print(pmx_ribbon(fill = "grey60", alpha = 0.4)))
})


##### +.pmx_element #####

test_that("`+.pmx_element` returns left side unchanged when right side is NULL", {
  base <- pmx_point(size = 2)
  expect_identical(base + NULL, base)
})

test_that("`+.pmx_element` overlays right side fields onto left side", {
  out <- pmx_point(size = 2) + pmx_point(color = "red")
  expect_equal(out$size, 2)
  expect_equal(out$color, "red")
  expect_s3_class(out, "pmx_point")
})

test_that("`+.pmx_element` right side wins when fields collide", {
  out <- pmx_point(size = 2, color = "blue") + pmx_point(color = "red")
  expect_equal(out$color, "red")
  expect_equal(out$size, 2)
})

test_that("`+.pmx_element` aborts on a non-pmx_element right side", {
  expect_error(pmx_point() + 1L,
               regexp = "must be a `pmx_element`")
})

test_that("`+.pmx_element` aborts on different subclasses", {
  expect_error(pmx_point() + pmx_line(),
               regexp = "subclasses must match")
  expect_error(pmx_style() + pmx_point(),
               regexp = "subclasses must match")
})
