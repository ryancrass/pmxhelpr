#####plot_gof####

##Test Output
test_that("Output is a `ggplot` plot object", {
  expect_s3_class(plot_gof(data_sad_pkfit, dv_var = "ODV"),
               class = "ggplot")
})

test_that("plot_gof rejects invalid `cent`", {
  expect_error(plot_gof(data_sad_pkfit, dv_var = "ODV", cent = "wat"),
               regexp = "should be one of")
})

test_that("Output plot maps variable TIME to the x aesthetic", {
  expect_equal(
    rlang::quo_name(plot_gof(data_sad_pkfit, dv_var = "ODV")$mapping$x),
    "TIME"
  )
})

test_that("Output plot maps variable DV to the y aesthetic", {
  expect_equal(
    rlang::quo_name(plot_gof(data_sad_pkfit, dv_var = "ODV")$mapping$y),
    "DV"
  )
})

test_that("Output plot contains a caption when argument `show_caption` = TRUE", {
  expect_equal(
    plot_gof(data_sad_pkfit, dv_var = "ODV")$labels$caption,
    "Thick lines are the mean"
  )
})

test_that("Output plot does not contain a caption when `show_caption = FALSE`", {
  expect_no_match(
    names(plot_gof(data_sad_pkfit, dv_var = "ODV", show_caption = FALSE)$labels),
    "caption"
  )
})

##Test Argument Handling
test_that("Error if incorrect class for arugmument `data`", {
  expect_error(plot_gof(data = "data_sad_pkfit", dv_var = "ODV"),
               regexp = "`data` must be a <data.frame>")
})

test_that("Error if time_var does not exist in `data`", {
  expect_error(plot_gof(data = data_sad_pkfit, dv_var = "ODV", time_var = "ATFD"),
               regexp = "must name variable.*in `data`")
})

test_that("Error if ntime_var does not exist in `data`", {
  expect_error(plot_gof(data = data_sad_pkfit, dv_var = "ODV", ntime_var = "NTFD"),
               regexp = "must name variable.*in `data`")
})

test_that("No error if time_var and ntime_var specified as the same variable", {
  expect_no_error(plot_gof(data = data_sad_pkfit, dv_var = "ODV",
                           time_var = "NTIME", ntime_var = "NTIME"))
})

test_that("Error if `id_var` does not exist in `data`", {
  expect_error(plot_gof(data = data_sad_pkfit, dv_var = "ODV", id_var = "SUBJID"),
               regexp = "`id_var` must name variable.*in `data`")
})

test_that("No error when `id_var` is NULL (default)", {
  expect_no_error(plot_gof(data = data_sad_pkfit, dv_var = "ODV"))
})

test_that("Error if `dose_var` does not exist in `data` and `dosenorm'` == TRUE", {
  expect_error(plot_gof(data = data_sad_pkfit, dv_var = "ODV", dose_var = "DOSEN", dosenorm = TRUE),
               regexp = "`dose_var` must name variable.*in `data`")
})

test_that("No error if `dose_var` does not exist in `data` and `dosenorm'` == FALSE", {
  expect_no_error(suppressWarnings(plot_gof(data = data_sad_pkfit, dv_var = "ODV", dose_var = "DOSEN", dosenorm = FALSE)))
})

test_that("Error if dv_var does not exist in `data`", {
  expect_error(plot_gof(data = data_sad_pkfit, dv_var = "NONEXIST"),
               regexp = "must name variable.*in `data`")
})

##Test NSE Bare Names
test_that("plot_gof accepts bare names", {
  expect_s3_class(plot_gof(data_sad_pkfit, dv_var = "ODV",
                               id_var = ID, dose_var = DOSE, dosenorm = TRUE),
                  "ggplot")
})

test_that("plot_gof warns when shown has no active elements", {
  shown <- plot_gof_shown(obs = FALSE, dv = FALSE, pred = FALSE, ipred = FALSE)
  expect_warning(plot_gof(data_sad_pkfit, dv_var = "ODV", shown = shown),
                 regexp = "no overlay layers")
  # empty active set must still yield a buildable ggplot (empty color scale).
  suppressWarnings(
    p <- plot_gof(data_sad_pkfit, dv_var = "ODV", shown = shown))
  expect_s3_class(p, "ggplot")
})


##Test color styling contract (ggstylekit style$colors is the single source)
# `style_plot()` injects `style$colors` into the placeholder-valued manual
# scale, while `series_layer` fills the role-keyed fixed aesthetics and leaves
# the mapped color channel to that scale. `ggplot_build()` trains the scale;
# the benign data-driven "Removed rows" warnings are unrelated and suppressed.
test_that("plot_gof color legend is driven by style$colors", {
  sg <- style_gof()
  p  <- plot_gof(data_sad_pkfit, dv_var = "ODV")
  sc <- colour_scale(p)
  brk <- as.character(sc$get_breaks())

  expect_equal(brk, c("OBS", "DV", "IPRED", "PRED"))
  expect_equal(unname(sc$map(brk)), unname(sg$colors[brk]))
  expect_equal(sc$name, "Legend")

  # role-keyed fixed aesthetics (shape/alpha) coexist with label-keyed color
  b   <- suppressWarnings(ggplot2::ggplot_build(p))
  idx <- which(vapply(b$plot$layers, function(L)
    inherits(L$geom, "GeomPoint") && inherits(L$stat, "StatIdentity"),
    logical(1)))[1]
  obs <- b$data[[idx]]
  expect_equal(unique(obs$colour), unname(sg$colors["OBS"]))
  expect_equal(unique(obs$shape),  unname(sg$shapes["obs_point"]))
  expect_equal(unique(obs$alpha),  unname(sg$alphas["obs_point"]))
})

test_that("plot_gof honors style_gof(colors=) overrides through the style", {
  p    <- plot_gof(data_sad_pkfit, dv_var = "ODV",
                   style = style_gof(colors = c(DV = "black")))
  sc   <- colour_scale(p)
  brk  <- as.character(sc$get_breaks())
  vals <- stats::setNames(sc$map(brk), brk)
  expect_equal(unname(vals["DV"]),  "black")     # override flows through
  expect_equal(unname(vals["OBS"]), "darkgrey")  # other roles keep defaults
})

test_that("plot_gof legend is restricted to the active `shown` layers", {
  p  <- plot_gof(data_sad_pkfit, dv_var = "ODV",
                 shown = plot_gof_shown(obs = FALSE, ipred = FALSE))
  sc <- colour_scale(p)
  # OBS and IPRED hidden -> only DV and PRED keep their keys, order preserved
  expect_equal(as.character(sc$get_breaks()), c("DV", "PRED"))
})


##Test BLQ handling
test_that("plot_gof accepts loq_method = 1 and renders the LOQ ref line", {
  p <- plot_gof(data_sad_pkfit, dv_var = "ODV", loq = 1, loq_method = 1)
  expect_s3_class(p, "ggplot")
  hline_layers <- Filter(function(L) inherits(L$geom, "GeomHline"), p$layers)
  expect_gte(length(hline_layers), 1)
})

test_that("plot_gof accepts loq_method = 2 and renders the LOQ ref line", {
  p <- plot_gof(data_sad_pkfit, dv_var = "ODV", loq = 1, loq_method = 2)
  expect_s3_class(p, "ggplot")
  hline_layers <- Filter(function(L) inherits(L$geom, "GeomHline"), p$layers)
  expect_gte(length(hline_layers), 1)
})

test_that("default blq_mode = 'obs' leaves PRED / IPRED untouched", {
  prep <- pmxhelpr:::df_prep_dvtime(
    data_sad_pkfit,
    time_var_str = "TIME", ntime_var_str = "NTIME",
    dv_var_str = "ODV",
    pred_var_str = "PRED", ipred_var_str = "IPRED",
    loq = 1, loq_method = 2
  )
  prepped <- prep$data
  # Reference original values restricted to the same rows that survived
  # the EVID==0 filter inside df_prep_dvtime.
  orig <- dplyr::filter(data_sad_pkfit, EVID == 0)
  blq_rows <- which(prepped$MDV == 1L)
  expect_gt(length(blq_rows), 0)
  # DV (renamed from ODV) was imputed for BLQ rows
  expect_true(all(prepped$DV[blq_rows] == 0.5 * 1))
  # PRED / IPRED retain their original values (no imputation in obs mode)
  expect_identical(prepped$PRED[blq_rows], orig$PRED[blq_rows])
  expect_identical(prepped$IPRED[blq_rows], orig$IPRED[blq_rows])
})

test_that("blq_mode = 'all' imputes PRED / IPRED alongside DV", {
  prep <- pmxhelpr:::df_prep_dvtime(
    data_sad_pkfit,
    time_var_str = "TIME", ntime_var_str = "NTIME",
    dv_var_str = "ODV",
    pred_var_str = "PRED", ipred_var_str = "IPRED",
    loq = 1, loq_method = 2,
    blq_mode = "all"
  )
  prepped <- prep$data
  orig    <- dplyr::filter(data_sad_pkfit, EVID == 0)
  loq_val <- 1
  # Rows with original PRED < LOQ get imputed to 0.5 * LOQ; rows >= LOQ are
  # preserved verbatim.
  pred_below <- which(orig$PRED < loq_val)
  pred_above <- which(orig$PRED >= loq_val)
  expect_gt(length(pred_below), 0)
  expect_true(all(prepped$PRED[pred_below] == 0.5 * loq_val))
  expect_identical(prepped$PRED[pred_above], orig$PRED[pred_above])
  # Same invariant for IPRED.
  ipred_below <- which(orig$IPRED < loq_val)
  if (length(ipred_below) > 0) {
    expect_true(all(prepped$IPRED[ipred_below] == 0.5 * loq_val))
  }
})

## Per-row LLOQ imputation fixture: three subjects at TIME 0/1/2, LLOQ 1 for
## ID 1 and 4 for IDs 2-3, BLQ records (MDV = 1) at and after time zero, and
## predictions below, at, and above each row's LLOQ.
blq_fixture <- data.frame(
  ID    = rep(1:3, each = 3),
  TIME  = rep(c(0, 1, 2), 3),
  NTIME = rep(c(0, 1, 2), 3),
  DV    = c(NA, 5, NA,   NA, NA, 6,   4.5, 8, NA),
  MDV   = c(1, 0, 1,     1, 1, 0,     0, 0, 1),
  LLOQ  = rep(c(1, 4, 4), each = 3),
  PRED  = c(0.2, 1, 0.5, 0.3, 4, 3,   5, 6, 2),
  IPRED = c(0.1, 3, 0.8, 0.2, 2, 4,   6, 7, 4.5),
  EVID  = 0, CMT = 2
)

prep_blq <- function(loq_method) {
  suppressMessages(pmxhelpr:::df_prep_dvtime(
    blq_fixture, time_var_str = "TIME", ntime_var_str = "NTIME",
    dv_var_str = "DV", pred_var_str = "PRED", ipred_var_str = "IPRED",
    loq = NULL, loq_method = loq_method, blq_mode = "all"))
}

## Prediction imputation written out independently of df_prep_blq()
pred_ref <- function(x, loq, time, zero_at_dose) {
  out <- ifelse(x >= loq, x, 0.5 * loq)
  if (zero_at_dose) out[time <= 0] <- 0
  out
}

test_that("loq_method = 1 imputes BLQ DV per row: 0 at time zero, else half that row's LLOQ", {
  d <- prep_blq(1)$data
  f <- blq_fixture
  expected <- ifelse(f$MDV == 0, f$DV, ifelse(f$TIME <= 0, 0, 0.5 * f$LLOQ))
  expect_equal(d$DV, expected)
})

test_that("loq_method = 1 with blq_mode = 'all' imputes PRED / IPRED per row", {
  d <- prep_blq(1)$data
  f <- blq_fixture
  expect_equal(d$PRED,  pred_ref(f$PRED,  f$LLOQ, f$TIME, zero_at_dose = TRUE))
  expect_equal(d$IPRED, pred_ref(f$IPRED, f$LLOQ, f$TIME, zero_at_dose = TRUE))
})

test_that("loq_method = 2 imputes BLQ DV and predictions to half each row's LLOQ", {
  d <- prep_blq(2)$data
  f <- blq_fixture
  expect_equal(d$DV, ifelse(f$MDV == 0, f$DV, 0.5 * f$LLOQ))
  expect_equal(d$PRED,  pred_ref(f$PRED,  f$LLOQ, f$TIME, zero_at_dose = FALSE))
  expect_equal(d$IPRED, pred_ref(f$IPRED, f$LLOQ, f$TIME, zero_at_dose = FALSE))
})

test_that("the LLOQ line is drawn at the highest per-row LLOQ", {
  expect_equal(prep_blq(1)$lloq, 4)
  expect_equal(prep_blq(2)$lloq, 4)
})

test_that("plot_gof cent = 'mean_sdl' error bars are mean +/- SD of DV by NTIME", {
  d <- data.frame(ID = rep(1:4, each = 2), NTIME = rep(c(1, 2), 4),
                  TIME = rep(c(1, 2), 4), ODV = c(1, 4, 2, 6, 3, 5, 6, 9),
                  PRED = 1, IPRED = 1, CMT = 2, MDV = 0, EVID = 0)
  p <- plot_gof(d, dv_var = "ODV", cent = "mean_sdl", show_caption = FALSE)
  b <- ggplot2::ggplot_build(p)
  idx <- which(vapply(b$plot$layers, function(L) inherits(L$geom, "GeomErrorbar"), logical(1)))[1]
  eb <- b$data[[idx]]
  eb <- eb[order(eb$x), ]
  by_time <- function(f) as.vector(tapply(d$ODV, d$NTIME, f))
  expect_equal(eb$ymin, by_time(function(v) mean(v) - stats::sd(v)))
  expect_equal(eb$ymax, by_time(function(v) mean(v) + stats::sd(v)))
})

##Test errorbar_width (style field)
test_that("plot_gof errorbar_width from the style reaches the errorbar layer", {
  p <- plot_gof(data_sad_pkfit, dv_var = "ODV", cent = "mean_sdl",
                style = style_gof(errorbar_width = 12, linewidths = c(cent_errorbar = 1.5)))
  b   <- suppressWarnings(ggplot2::ggplot_build(p))
  idx <- which(vapply(b$plot$layers, function(L) inherits(L$geom, "GeomErrorbar"), logical(1)))[1]
  eb  <- b$data[[idx]]
  expect_equal(eb$xmax - eb$xmin, rep(12, nrow(eb)))
  expect_equal(unique(eb$linewidth), 1.5)
  # colour stays with the manual overlay scale (style$colors["DV"])
  expect_equal(unique(eb$colour), unname(style_gof()$colors["DV"]))
  expect_error(style_gof(errorbar_width = "wide"), regexp = "non-negative number")
})

test_that("plot_gof() rejects non-logical flags", {
  expect_error(plot_gof(data_sad_pkfit, dv_var = "ODV", log_y = NA),
               "`log_y` must be `TRUE` or `FALSE`")
  expect_error(plot_gof(data_sad_pkfit, dv_var = "ODV", dosenorm = "yes"),
               "`dosenorm` must be `TRUE` or `FALSE`")
  expect_error(plot_gof(data_sad_pkfit, dv_var = "ODV", show_caption = 1),
               "`show_caption` must be `TRUE` or `FALSE`")
})

test_that("plot_gof show_loq_value toggles the LLOQ legend value", {
  lt_on <- ggplot2::get_guide_data(
    plot_gof(data_sad_pkfit, dv_var = "ODV", loq = 1, loq_method = 1), "linetype")
  expect_equal(lt_on$.label, "1")
  p_off <- plot_gof(data_sad_pkfit, dv_var = "ODV", loq = 1, loq_method = 1,
                    show_loq_value = FALSE)
  expect_equal(ggplot2::get_guide_data(p_off, "linetype")$.label, "LLOQ")
  expect_null(p_off$scales$get_scales("linetype")$name)
  expect_error(plot_gof(data_sad_pkfit, dv_var = "ODV", show_loq_value = NA),
               regexp = "`show_loq_value` must be `TRUE` or `FALSE`")
})

test_that("plot_gof log_y falls back to the style's logy; a passed log_y wins", {
  p_style <- plot_gof(data_sad_pkfit, dv_var = "ODV", style = style_gof(logy = TRUE))
  expect_equal(y_trans(p_style), "log-10")
  expect_match(p_style$labels$caption, "[Gg]eometric mean")
  p_arg <- plot_gof(data_sad_pkfit, dv_var = "ODV", log_y = FALSE,
                    style = style_gof(logy = TRUE))
  expect_equal(y_trans(p_arg), "identity")
  expect_equal(y_trans(plot_gof(data_sad_pkfit, dv_var = "ODV")), "identity")
})
