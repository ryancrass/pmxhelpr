#####element constructors####

#####pmx_point####

test_that("pmx_point returns correct class", {
  el <- pmx_point(shape = 1, size = 2, alpha = 0.5, color = "red")
  expect_s3_class(el, "pmx_point")
})

test_that("pmx_point sets all fields", {
  el <- pmx_point(shape = 16, size = 1.5, alpha = 0.8, color = "blue")
  expect_equal(el$shape, 16)
  expect_equal(el$size, 1.5)
  expect_equal(el$alpha, 0.8)
  expect_equal(el$color, "blue")
})

test_that("pmx_point with no args returns empty named structure", {
  el <- pmx_point()
  expect_s3_class(el, "pmx_point")
  expect_length(el, 0)
})

test_that("pmx_point compact removes NULL fields", {
  el <- pmx_point(shape = 1, size = NULL, alpha = 0.5)
  expect_true("shape" %in% names(el))
  expect_false("size" %in% names(el))
  expect_true("alpha" %in% names(el))
})

test_that("pmx_point rejects unknown arguments", {
  expect_error(pmx_point(fake = 1), regexp = "unused argument")
})

#####pmx_line####

test_that("pmx_line returns correct class", {
  el <- pmx_line(linewidth = 1, linetype = 2, alpha = 0.5, color = "black")
  expect_s3_class(el, "pmx_line")
})

test_that("pmx_line sets all fields", {
  el <- pmx_line(linewidth = 0.75, linetype = "dashed", alpha = 1, color = "#FF0000")
  expect_equal(el$linewidth, 0.75)
  expect_equal(el$linetype, "dashed")
  expect_equal(el$alpha, 1)
  expect_equal(el$color, "#FF0000")
})

test_that("pmx_line with no args returns empty named structure", {
  el <- pmx_line()
  expect_s3_class(el, "pmx_line")
  expect_length(el, 0)
})

test_that("pmx_line compact removes NULL fields", {
  el <- pmx_line(linewidth = 1, color = NULL)
  expect_true("linewidth" %in% names(el))
  expect_false("color" %in% names(el))
})

test_that("pmx_line rejects unknown arguments", {
  expect_error(pmx_line(shape = 1), regexp = "unused argument")
})

#####pmx_ribbon####

test_that("pmx_ribbon returns correct class", {
  el <- pmx_ribbon(fill = "blue", alpha = 0.3, color = "black",
                   linetype = "dashed", linewidth = 1)
  expect_s3_class(el, "pmx_ribbon")
})

test_that("pmx_ribbon sets all fields", {
  el <- pmx_ribbon(fill = "#0000FF", alpha = 0.15, color = "#000000",
                   linetype = "dotted", linewidth = 0.5)
  expect_equal(el$fill, "#0000FF")
  expect_equal(el$alpha, 0.15)
  expect_equal(el$color, "#000000")
  expect_equal(el$linetype, "dotted")
  expect_equal(el$linewidth, 0.5)
})

test_that("pmx_ribbon with no args returns empty named structure", {
  el <- pmx_ribbon()
  expect_s3_class(el, "pmx_ribbon")
  expect_length(el, 0)
})

test_that("pmx_ribbon rejects unknown arguments", {
  expect_error(pmx_ribbon(shape = 1), regexp = "unused argument")
})

#####pmx_style####

test_that("pmx_style returns correct class", {
  el <- pmx_style(color = "red", alpha = 0.5)
  expect_s3_class(el, "pmx_style")
})

test_that("pmx_style sets color and alpha", {
  el <- pmx_style(color = "purple", alpha = 0.3)
  expect_equal(el$color, "purple")
  expect_equal(el$alpha, 0.3)
})

test_that("pmx_style with no args returns empty named structure", {
  el <- pmx_style()
  expect_s3_class(el, "pmx_style")
  expect_length(el, 0)
})

test_that("pmx_style compact removes NULL fields", {
  el <- pmx_style(color = "red")
  expect_true("color" %in% names(el))
  expect_false("alpha" %in% names(el))
})

test_that("pmx_style rejects unknown arguments", {
  expect_error(pmx_style(shape = 1), regexp = "unused argument")
})

#####pmx_errorbar####

test_that("pmx_errorbar returns correct class", {
  el <- pmx_errorbar(linewidth = 1, linetype = 1, alpha = 1, width = 0.5)
  expect_s3_class(el, "pmx_errorbar")
})

test_that("pmx_errorbar sets all fields", {
  el <- pmx_errorbar(linewidth = 0.75, linetype = 2, alpha = 0.8, width = 1.5)
  expect_equal(el$linewidth, 0.75)
  expect_equal(el$linetype, 2)
  expect_equal(el$alpha, 0.8)
  expect_equal(el$width, 1.5)
})

test_that("pmx_errorbar with no args returns structure with width slot", {
  el <- pmx_errorbar()
  expect_s3_class(el, "pmx_errorbar")
  expect_length(el, 1)
  expect_true("width" %in% names(el))
  expect_null(el$width)
})

test_that("pmx_errorbar accepts color argument", {
  el <- pmx_errorbar(color = "red")
  expect_equal(el$color, "red")
})

#####pmx_trend####

test_that("pmx_trend returns correct class", {
  el <- pmx_trend(linewidth = 1, linetype = 1, color = "black",
                  se_color = "grey", se_alpha = 0.4)
  expect_s3_class(el, "pmx_trend")
})

test_that("pmx_trend sets all fields", {
  el <- pmx_trend(linewidth = 2, linetype = "dashed", color = "red",
                  se_color = "pink", se_alpha = 0.2)
  expect_equal(el$linewidth, 2)
  expect_equal(el$linetype, "dashed")
  expect_equal(el$color, "red")
  expect_equal(el$se_color, "pink")
  expect_equal(el$se_alpha, 0.2)
})

test_that("pmx_trend with no args returns empty named structure", {
  el <- pmx_trend()
  expect_s3_class(el, "pmx_trend")
  expect_length(el, 0)
})

test_that("pmx_trend rejects unknown arguments", {
  expect_error(pmx_trend(shape = 1), regexp = "unused argument")
})

#####pmx_color####

test_that("pmx_color returns correct class", {
  el <- pmx_color(dv = "blue", pred = "red", ipred = "green")
  expect_s3_class(el, "pmx_color")
})

test_that("pmx_color sets all fields", {
  el <- pmx_color(dv = "blue", pred = "red", ipred = "green")
  expect_equal(el$dv, "blue")
  expect_equal(el$pred, "red")
  expect_equal(el$ipred, "green")
})

test_that("pmx_color with no args returns empty named structure", {
  el <- pmx_color()
  expect_s3_class(el, "pmx_color")
  expect_length(el, 0)
})

test_that("pmx_color partial override", {
  el <- pmx_color(pred = "purple")
  expect_equal(el$pred, "purple")
  expect_null(el$dv)
  expect_null(el$ipred)
})

test_that("pmx_color accepts arbitrary named arguments (variadic)", {
  el <- pmx_color(obs = "black", FOOD = "red")
  expect_equal(el$obs, "black")
  expect_equal(el$FOOD, "red")
})

test_that("pmx_color rejects unnamed arguments", {
  expect_error(pmx_color("black"), regexp = "must be named")
})

#####pmx_* input validation####

test_that("pmx_point rejects invalid color", {
  expect_error(pmx_point(color = "saalmon"),
               regexp = "must be a valid color name or hex string")
})

test_that("pmx_point accepts size = 0 (hide-layer idiom)", {
  expect_no_error(pmx_point(size = 0))
})

test_that("pmx_point rejects negative size", {
  expect_error(pmx_point(size = -1), regexp = "non-negative numeric")
})

test_that("pmx_point rejects out-of-range shape", {
  expect_error(pmx_point(shape = 99), regexp = "integer in 0:25")
})

test_that("pmx_line rejects invalid color and negative linewidth", {
  expect_error(pmx_line(color = "garbage"),
               regexp = "must be a valid color name or hex string")
  expect_error(pmx_line(linewidth = -1), regexp = "non-negative numeric")
})

test_that("pmx_ribbon rejects invalid fill, color, and negative linewidth", {
  expect_error(pmx_ribbon(fill = "saalmon"),
               regexp = "must be a valid color name or hex string")
  expect_error(pmx_ribbon(color = "garbage"),
               regexp = "must be a valid color name or hex string")
  expect_error(pmx_ribbon(linewidth = -0.5), regexp = "non-negative numeric")
})

test_that("pmx_errorbar rejects negative width", {
  expect_error(pmx_errorbar(width = -1), regexp = "non-negative numeric")
})

test_that("pmx_trend rejects invalid se_color", {
  expect_error(pmx_trend(se_color = "saalmon"),
               regexp = "must be a valid color name or hex string")
})

test_that("pmx_color rejects invalid color in any field", {
  expect_error(pmx_color(dv = "blue", pred = "garbage", ipred = "green"),
               regexp = "must be a valid color name")
})

test_that("pmx_style rejects invalid color", {
  expect_error(pmx_style(color = "saalmon"),
               regexp = "must be a valid color name or hex string")
})
