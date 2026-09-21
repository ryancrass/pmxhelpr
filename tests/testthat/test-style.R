#####style presets + internals#####

##Presets return valid style_spec objects
test_that("style_* presets return a ggstylekit_style_spec", {
  for (f in list(style_dvtime, style_gof, style_dvconc, style_doseprop, style_vpc)) {
    s <- f()
    expect_s3_class(s, "ggstylekit_style_spec")
    expect_no_error(ggstylekit::validate_style_spec(s))
  }
})

##build_style: per-series maps merge entry-wise; other fields replace
test_that("style preset overrides merge per-series maps entry-wise", {
  s <- style_vpc(colors = c(obs_point = "#111111"))
  # overridden entry wins
  expect_equal(s$colors[["obs_point"]], "#111111")
  # other roles keep their defaults (not wiped)
  expect_equal(s$colors[["obs_median_line"]], "#FF0000")
})

test_that("style preset non-map fields replace wholesale", {
  s <- style_dvtime(title = "My Title")
  expect_equal(s$title, "My Title")
})

test_that("a palette function replaces a preset's default per-series map", {
  # Palettes carry no names, so they cannot merge entry-wise onto a set default
  # (`style_gof()` ships a `colors` map); they replace it instead.
  pal <- function(n) grDevices::hcl.colors(n, "Viridis")
  s <- style_gof(colors = pal)
  expect_true(is.function(s$colors))
  expect_null(names(s$colors))

  # A preset whose default for that map is unset behaves the same way.
  expect_true(is.function(style_dvtime(colors = pal)$colors))

  # Other maps still merge entry-wise alongside the palette.
  s2 <- style_vpc(fill = pal, alphas = c(obs_point = 0.2))
  expect_true(is.function(s2$fill))
  expect_equal(s2$alphas[["obs_point"]], 0.2)
  expect_equal(s2$alphas[["sim_pi_ci"]], 0.15)
})

test_that("build_style rejects unnamed overrides", {
  expect_error(pmxhelpr:::build_style(list(colors = c(a = "red")), list("red")),
               regexp = "must be named")
})

##series_aes: reads set fields, guards missing keys
test_that("series_aes returns set aesthetics and drops unset ones", {
  s <- style_vpc()
  a <- pmxhelpr:::series_aes(s, "obs_median_line")
  expect_equal(a$colour, "#FF0000")
  expect_equal(a$linetype, "solid")
  expect_null(a$shape)   # obs_median_line has no shape
})

test_that("series_aes does not error on a role absent from every map", {
  expect_type(pmxhelpr:::series_aes(style_vpc(), "not_a_role"), "list")
  expect_length(pmxhelpr:::series_aes(style_vpc(), "not_a_role"), 0)
})

##pmx_house_theme
test_that("pmx_house_theme blanks minor + vertical-major gridlines", {
  th <- pmxhelpr:::pmx_house_theme()
  expect_s3_class(th, "theme")
  expect_s3_class(th$panel.grid.minor, "element_blank")
  expect_s3_class(th$panel.grid.major.x, "element_blank")
})

test_that("pmx_house_theme(white_panel = TRUE) sets a white panel background", {
  th <- pmxhelpr:::pmx_house_theme(white_panel = TRUE)
  expect_s3_class(th$panel.background, "element_rect")
})

##merge_element still merges the shown visibility lists
test_that("merge_element merges a partial shown list over defaults", {
  out <- pmxhelpr:::merge_element(list(obs = FALSE), plot_gof_shown())
  expect_false(out$obs)
  expect_true(out$dv)
})

##resolve_style: default on NULL, pass-through on a spec, abort otherwise
test_that("resolve_style returns the preset default when style is NULL", {
  s <- pmxhelpr:::resolve_style(NULL, style_dvtime)
  expect_s3_class(s, "ggstylekit_style_spec")
  expect_equal(s$shapes, style_dvtime()$shapes)
})

test_that("resolve_style passes a style_spec through unchanged", {
  s <- style_vpc(colors = c(obs_point = "#000000"))
  expect_identical(pmxhelpr:::resolve_style(s, style_dvtime), s)
})

test_that("resolve_style aborts on a non-style_spec `style`", {
  expect_error(pmxhelpr:::resolve_style(list(colors = "red"), style_dvtime),
               regexp = "argument `style` must be a `ggstylekit::style_spec\\(\\)` object.*got <list>")
  expect_error(pmxhelpr:::resolve_style("blue", style_dvtime),
               regexp = "got <character>")
})
