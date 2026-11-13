##### plot_vpc_cens #####

##Test Output
test_that("Output is a `ggplot`/`pmx_vpc_plot` object", {
  testsim <- sim_pk()
  p <- plot_vpc_cens(data = testsim, loq = 1)
  expect_s3_class(p, "pmx_vpc_plot")
  expect_s3_class(p, "ggplot")
})

test_that("Output plot contains a caption with number of replicates by default", {
  testsim <- sim_pk()
  expect_equal(plot_vpc_cens(data = testsim, loq = 1)$labels$caption,
               "Replicates = 10")
})

test_that("show_rep = FALSE removes the replicates caption", {
  testsim <- sim_pk()
  expect_no_match(names(plot_vpc_cens(data = testsim, loq = 1,
                                      show_rep = FALSE)$labels),
                  regexp = "caption")
})

##Test LOQ requirement
test_that("plot_vpc_cens errors when neither loq arg nor LLOQ column is available", {
  testsim <- sim_pk()
  expect_false("LLOQ" %in% colnames(testsim))
  expect_error(plot_vpc_cens(data = testsim),
               regexp = "requires a LOQ source")
})

test_that("plot_vpc_cens accepts scalar loq arg", {
  testsim <- sim_pk()
  expect_s3_class(plot_vpc_cens(testsim, loq = 1), "ggplot")
})

test_that("plot_vpc_cens inherits loq from LLOQ column when not supplied", {
  testsim <- sim_pk(carry_out = "LLOQ")
  expect_message(p <- plot_vpc_cens(data = testsim),
                 regexp = "Inheriting per-row `loq` from `LLOQ` column")
  expect_s3_class(p, "ggplot")
})

##Test Argument Handling
test_that("Error if incorrect class for argument `data`", {
  expect_error(plot_vpc_cens(data = "simdata", loq = 1),
               regexp = "`data` must be a <data.frame>")
})

test_that("Error if time_var/ntime_var/pred_var/sim_dv_var/obs_dv_var missing in `data`", {
  testsim <- sim_pk()
  expect_error(plot_vpc_cens(data = testsim, loq = 1, time_var = "ATFD"),
               regexp = "must name variable.*in `data`")
  expect_error(plot_vpc_cens(data = testsim, loq = 1, ntime_var = "NTFD"),
               regexp = "must name variable.*in `data`")
  expect_error(plot_vpc_cens(data = testsim, loq = 1, pred_var = "CPRED"),
               regexp = "must name variable.*in `data`")
  expect_error(plot_vpc_cens(data = testsim, loq = 1, sim_dv_var = "DVSIM"),
               regexp = "must name variable.*in `data`")
  expect_error(plot_vpc_cens(data = testsim, loq = 1, obs_dv_var = "DVOBS"),
               regexp = "must name variable.*in `data`")
})

##Test layer composition
test_that("Default cens plot draws ribbon + obs line + obs points (no sim median line)", {
  testsim <- sim_pk()
  p <- plot_vpc_cens(testsim, loq = 1)
  geom_class <- function(L) class(L$geom)[1]
  classes <- vapply(p$layers, geom_class, character(1))
  expect_true("GeomRibbon" %in% classes)  # sim CI
  expect_true("GeomLine"   %in% classes)  # obs line
  expect_true("GeomPoint"  %in% classes)  # obs points per bin
  ## Only one GeomLine layer (obs line); sim_median_line is off by default.
  expect_equal(sum(classes == "GeomLine"), 1L)
})

test_that("No GeomHline LOQ ref line is drawn on a cens plot", {
  testsim <- sim_pk()
  p <- plot_vpc_cens(testsim, loq = 1)
  has_hline <- any(vapply(p$layers,
                          function(L) inherits(L$geom, "GeomHline"),
                          logical(1)))
  expect_false(has_hline)
})

test_that("sim_median_line defaults OFF in cens; opt-in via shown adds the layer", {
  testsim <- sim_pk()
  ## Default: one line layer (obs only).
  p_default <- plot_vpc_cens(testsim, loq = 1)
  line_default <- Filter(function(L) inherits(L$geom, "GeomLine"), p_default$layers)
  expect_length(line_default, 1)

  ## Enabling sim_median_line adds a second line layer.
  p_sim_on <- plot_vpc_cens(testsim, loq = 1,
                            shown = plot_vpc_shown(sim_median_line = TRUE))
  line_sim_on <- Filter(function(L) inherits(L$geom, "GeomLine"), p_sim_on$layers)
  expect_length(line_sim_on, 2)
})

test_that("shown toggles disable individual cens layers", {
  testsim <- sim_pk()
  geom_class <- function(L) class(L$geom)[1]

  ## obs_point off → no GeomPoint layers
  p1 <- plot_vpc_cens(testsim, loq = 1, shown = plot_vpc_shown(obs_point = FALSE))
  expect_false("GeomPoint" %in% vapply(p1$layers, geom_class, character(1)))

  ## sim_median_ci off → no GeomRibbon layers
  p2 <- plot_vpc_cens(testsim, loq = 1, shown = plot_vpc_shown(sim_median_ci = FALSE))
  expect_false("GeomRibbon" %in% vapply(p2$layers, geom_class, character(1)))
})

test_that("Cens obs points inherit color from obs_median_line for color coherence", {
  testsim <- sim_pk()
  custom_style <- style_vpc(
    colors     = c(obs_median_line = "#00AA00", obs_point = "#FF00FF"),
    linetypes  = c(obs_median_line = "solid"),
    linewidths = c(obs_median_line = 1),
    shapes     = c(obs_point = 16),
    sizes      = c(obs_point = 2),
    alphas     = c(obs_point = 0.9)
  )
  p <- plot_vpc_cens(testsim, loq = 1, style = custom_style)
  pt_layer <- Filter(function(L) inherits(L$geom, "GeomPoint"), p$layers)[[1]]
  ## Point color follows obs_median_line, NOT obs_point.
  expect_equal(pt_layer$aes_params$colour, "#00AA00")
  ## Shape/alpha/size still come from obs_point.
  expect_equal(pt_layer$aes_params$shape, 16)
  expect_equal(pt_layer$aes_params$alpha, 0.9)
  expect_equal(pt_layer$aes_params$size,  2)
})

test_that("style overrides apply to cens layers via mapped keys", {
  testsim <- sim_pk()
  custom_style <- style_vpc(
    fill       = c(sim_median_ci = "#00FF00"),
    alphas     = c(sim_median_ci = 0.5),
    colors     = c(obs_median_line = "#123456"),
    linetypes  = c(obs_median_line = "dotted"),
    linewidths = c(obs_median_line = 2)
  )
  p <- plot_vpc_cens(testsim, loq = 1, style = custom_style)
  ribbon_layer <- Filter(function(L) inherits(L$geom, "GeomRibbon"), p$layers)[[1]]
  expect_equal(ribbon_layer$aes_params$fill,  "#00FF00")
  expect_equal(ribbon_layer$aes_params$alpha, 0.5)

  ## Locate the obs-median line: aesthetic is mapped to obs_prop_blq.
  line_layers <- Filter(function(L) inherits(L$geom, "GeomLine"), p$layers)
  obs_layers <- Filter(function(L) {
    rhs <- rlang::quo_get_expr(L$mapping$y)
    !is.null(rhs) && identical(rlang::as_string(rhs[[3]]), "obs_prop_blq")
  }, line_layers)
  expect_length(obs_layers, 1)
  expect_equal(obs_layers[[1]]$aes_params$colour, "#123456")
})

##Test stratification
test_that("Stratified cens VPC produces a faceted plot", {
  testsim <- sim_pk(carry_out = "FOOD")
  testsim <- dplyr::mutate(testsim, FOOD_f = factor(FOOD))
  p <- plot_vpc_cens(data = testsim, loq = 1, strat_var = FOOD_f)
  expect_s3_class(p, "ggplot")
  expect_true("FacetWrap" %in% class(p$facet))
})

##Test min_bin_count semantics (cens uses obs_n, not obs_n - obs_n_blq)
test_that("min_bin_count filters cens layers on obs_n (total obs)", {
  testsim <- sim_pk()
  out <- df_vpcstats(testsim, loq = 1)
  threshold <- max(out$stats$obs_n) + 1L
  p <- plot_vpc_cens(testsim, loq = 1, min_bin_count = threshold)
  built <- ggplot2::ggplot_build(p)
  ## All ribbon/line/point layers should be empty when no bin meets the threshold.
  nonempty <- vapply(built$data, function(d) nrow(d) > 0, logical(1))
  ## Strip layers may exist; just ensure no plotted data rows remain.
  expect_false(any(nonempty))
})

##Test default y-axis range
test_that("Default y-axis range is [0, 1] via coord_cartesian", {
  testsim <- sim_pk()
  p <- plot_vpc_cens(testsim, loq = 1)
  expect_equal(p$coordinates$limits$y, c(0, 1))
})

##Test y-axis range
test_that("Plotted proportion data lies in [0, 1]", {
  testsim <- sim_pk()
  p <- plot_vpc_cens(testsim, loq = 1)
  built <- ggplot2::ggplot_build(p)
  for (d in built$data) {
    if ("y" %in% colnames(d) && nrow(d) > 0) {
      expect_true(all(d$y >= 0 & d$y <= 1, na.rm = TRUE))
    }
    if ("ymin" %in% colnames(d) && nrow(d) > 0) {
      expect_true(all(d$ymin >= 0 & d$ymin <= 1, na.rm = TRUE))
      expect_true(all(d$ymax >= 0 & d$ymax <= 1, na.rm = TRUE))
    }
  }
})

##### Test plot_vpc_cens() with precomputed df_vpcstats() output #####

test_that("plot_vpc_cens accepts a precomputed vpc_stats container", {
  testsim <- sim_pk()
  out <- df_vpcstats(testsim, loq = 1)
  expect_s3_class(out, "vpc_stats")
  p <- plot_vpc_cens(out)
  expect_s3_class(p, "ggplot")
})

test_that("plot_vpc_cens errors when precomputed vpc_stats lacks sim_prop_blq_*", {
  testsim <- sim_pk()
  out_no_loq <- df_vpcstats(testsim)
  expect_false("sim_prop_blq_med" %in% colnames(out_no_loq$stats))
  expect_error(plot_vpc_cens(out_no_loq),
               regexp = "requires a LOQ source")
})

test_that("plot_vpc_cens aborts when pipeline args are passed on the precomputed path", {
  testsim <- sim_pk()
  out <- df_vpcstats(testsim, loq = 1)
  expect_error(plot_vpc_cens(out, strat_var = "FOOD"),
               regexp = "cannot accept pipeline arguments")
  expect_error(plot_vpc_cens(out, loq = 1),
               regexp = "cannot accept pipeline arguments")
  expect_error(plot_vpc_cens(out, ci = 0.95),
               regexp = "cannot accept pipeline arguments")
})

test_that("plot_vpc_cens accepts plot-only args on the precomputed path", {
  testsim <- sim_pk()
  out <- df_vpcstats(testsim, loq = 1)
  expect_s3_class(plot_vpc_cens(out, min_bin_count = 2), "ggplot")
  expect_s3_class(plot_vpc_cens(out, show_rep = FALSE), "ggplot")
  expect_s3_class(plot_vpc_cens(out, style = style_vpc()), "ggplot")
  expect_s3_class(plot_vpc_cens(out, shown = plot_vpc_shown()), "ggplot")
})

test_that("precomputed and raw paths produce structurally equivalent cens plots", {
  testsim <- sim_pk()
  out <- df_vpcstats(testsim, loq = 1)
  p_pre <- plot_vpc_cens(out)
  p_raw <- plot_vpc_cens(testsim, loq = 1)
  built_pre <- ggplot2::ggplot_build(p_pre)
  built_raw <- ggplot2::ggplot_build(p_raw)
  expect_equal(length(built_pre$data), length(built_raw$data))
  expect_equal(vapply(built_pre$data, nrow, integer(1)),
               vapply(built_raw$data, nrow, integer(1)))
})

##### Test plot_build_vpc(type = "cens") guards #####

test_that("plot_build_vpc(type = 'cens', pcvpc = TRUE) is rejected", {
  testsim <- sim_pk()
  out <- df_vpcstats(testsim, loq = 1)
  expect_error(plot_build_vpc(out, type = "cens", pcvpc = TRUE),
               regexp = "not supported for `type = \"cens\"`")
})

test_that("plot_build_vpc(type = 'cens') errors when LOQ source is absent", {
  testsim <- sim_pk()
  out_no_loq <- df_vpcstats(testsim)
  expect_error(plot_build_vpc(out_no_loq, type = "cens"),
               regexp = "requires a LOQ source")
})

##Test style facet layout fields reach the internal facet_wrap()
test_that("plot_vpc_cens takes its stratification facet layout from the style", {
  testsim <- sim_pk(carry_out = "FOOD")
  p_fixed <- plot_vpc_cens(testsim, loq = 1, strat_var = FOOD)
  expect_true("FacetWrap" %in% class(p_fixed$facet))
  expect_false(p_fixed$facet$params$free$y)

  p_free <- plot_vpc_cens(testsim, loq = 1, strat_var = FOOD,
                          style = style_vpc(facet_scales = "free_y",
                                            facet_ncol = 1))
  expect_true(p_free$facet$params$free$y)
  expect_equal(p_free$facet$params$ncol, 1)
})

test_that("plot_vpc_cens() rejects a non-logical show_rep before computing", {
  expect_error(plot_vpc_cens(data_sad, show_rep = 1),
               "`show_rep` must be `TRUE` or `FALSE`")
})


test_that("cens VPC points and the cens legend Obs key take the obs line colour", {
  p <- suppressMessages(plot_vpc_cens(sim_pk(carry_out = "LLOQ")))
  geoms <- vapply(p$layers, function(L) class(L$geom)[1], "")
  line_col <- style_vpc()$colors[["obs_median_line"]]
  expect_equal(unique(ggplot2::layer_data(p, which(geoms == "GeomPoint"))$colour), line_col)
  leg <- plot_vpc_legend(type = "cens")
  leg_geoms <- vapply(leg$layers, function(L) class(L$geom)[1], "")
  expect_equal(unique(ggplot2::layer_data(leg, which(leg_geoms == "GeomPoint"))$colour), line_col)
  ## Restyling obs_point directly still recolours the cens points
  r <- restyle_plot(p, colors = c(obs_point = "purple"))
  expect_equal(unique(ggplot2::layer_data(r, which(geoms == "GeomPoint"))$colour), "purple")
})

test_that("restyling the cens obs line recolours the cens points with it", {
  p <- suppressMessages(plot_vpc_cens(sim_pk(carry_out = "LLOQ")))
  geoms <- vapply(p$layers, function(L) class(L$geom)[1], "")
  pt_col <- function(x) unique(ggplot2::layer_data(x, which(geoms == "GeomPoint"))$colour)
  r <- restyle_plot(p, colors = c(obs_median_line = "purple"))
  expect_equal(pt_col(r), "purple")
  expect_true(is_pmx_vpc_plot(r))
  ## An obs_point colour in the same call still wins
  r2 <- restyle_plot(p, colors = c(obs_median_line = "purple", obs_point = "orange"))
  expect_equal(pt_col(r2), "orange")
  ## The continuous VPC keeps its own obs_point colour
  pc <- suppressMessages(plot_vpc_cont(sim_pk(carry_out = "LLOQ")))
  pc_geoms <- vapply(pc$layers, function(L) class(L$geom)[1], "")
  rc <- restyle_plot(pc, colors = c(obs_median_line = "purple"))
  expect_equal(unique(ggplot2::layer_data(rc, which(pc_geoms == "GeomPoint"))$colour),
               style_vpc()$colors[["obs_point"]])
})

test_that("logy is ignored with a warning on a cens VPC", {
  stats <- df_vpcstats(sim_pk(), loq = 1)
  w <- testthat::capture_warnings(p <- plot_vpc_cens(stats, style = style_vpc(logy = TRUE)))
  expect_length(w, 1)
  expect_warning(plot_vpc_cens(stats, style = style_vpc(logy = TRUE)),
                 "is ignored for a censored VPC", class = "pmxhelpr_warning_ignored_arg")
  expect_no_error(ggplot2::ggplot_build(p))
  expect_warning(r <- restyle_plot(plot_vpc_cens(stats), logy = TRUE),
                 class = "pmxhelpr_warning_ignored_arg")
  expect_no_error(ggplot2::ggplot_build(r))
  expect_no_warning(plot_vpc_cens(stats))
  ## The continuous VPC keeps its log axis
  expect_no_warning(plot_vpc_cont(stats, style = style_vpc(logy = TRUE)),
                    class = "pmxhelpr_warning_ignored_arg")
})
