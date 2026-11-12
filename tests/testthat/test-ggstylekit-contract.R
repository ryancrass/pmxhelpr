#####ggstylekit contract####
# What pmxhelpr relies on from ggstylekit (floor: ggstylekit >= 0.4.1 in
# DESCRIPTION). A ggstylekit release that renames or reshapes any of it would
# otherwise make plots silently lose a color or setting instead of erroring:
#   1. every style_spec field pmxhelpr reads is a style_spec() argument;
#   2. every ggstylekit:: function pmxhelpr calls is exported;
#   3. unset fields read as NULL and per-series maps are stored as given;
#   4. restyle_plot() and reveal() are S3 generics carrying pmxhelpr's methods.
# Blocks 1 and 2 scan the package sources, so they skip when no .R sources sit
# beside the tests: under R CMD check there is no R/ there, and under covr it is
# the installed package's R/, which holds only the lazy-load database.

src_files <- list.files(test_path("..", "..", "R"), pattern = "[.]R$", full.names = TRUE)

src_text <- function() unlist(lapply(src_files, readLines, warn = FALSE))

## The per-series maps read by series_aes() and merged by build_style().
map_fields <- c("colors", "fill", "alphas", "shapes", "sizes", "linetypes", "linewidths")

spec_args <- names(formals(ggstylekit::style_spec))

test_that("every style_spec field pmxhelpr reads is a style_spec() argument", {
  expect_true(all(map_fields %in% spec_args))
  skip_if(length(src_files) == 0, "package sources not available")
  hits <- regmatches(src_text(), gregexpr("\\b[a-z_]*style[a-z_]*\\$[A-Za-z_.]+", src_text()))
  read <- unique(sub("^[a-z_]*\\$", "", unlist(hits)))
  expect_gt(length(read), 0)
  expect_identical(setdiff(read, spec_args), character(0))
})

test_that("every ggstylekit function pmxhelpr calls is exported", {
  skip_if(length(src_files) == 0, "package sources not available")
  hits <- regmatches(src_text(), gregexpr("ggstylekit::[A-Za-z_.]+", src_text()))
  called <- unique(sub("^ggstylekit::", "", unlist(hits)))
  expect_gt(length(called), 0)
  expect_identical(setdiff(called, getNamespaceExports("ggstylekit")), character(0))
})

test_that("style_spec() leaves unset fields NULL and stores maps as given", {
  s <- ggstylekit::style_spec()
  expect_s3_class(s, "ggstylekit_style_spec")
  read_fields <- c(map_fields, "errorbar_width", "logy", "legend.title.position",
                   "legend.title.hjust", "facet_scales", "facet_nrow", "facet_ncol")
  for (f in read_fields) expect_null(s[[f]], label = f)
  expect_identical(ggstylekit::style_spec(colors = c(a = "red"))$colors, c(a = "red"))
  expect_identical(ggstylekit::style_spec(linewidths = c(a = 0.5))$linewidths, c(a = 0.5))
  pal <- function(n) rep("red", n)
  expect_true(is.function(ggstylekit::style_spec(colors = pal)$colors))
})

test_that("restyle_plot() and reveal() are S3 generics with the pmx_vpc_plot methods", {
  for (g in c("restyle_plot", "reveal")) {
    expect_true(utils::isS3stdGeneric(getExportedValue("ggstylekit", g)), label = g)
    expect_false(is.null(utils::getS3method(g, "pmx_vpc_plot", optional = TRUE)), label = g)
  }
})
