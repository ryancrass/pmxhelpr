##### Tests for the bins_*() helpers, function bin_breaks, and bin_style #####

times <- c(0.5, 1, 1, 2, 4, 4, 8, 24, 24, 48, 72, 168)

test_that("bins_equal() and bins_quantile() split the time range", {
  expect_equal(bins_equal(4)(c(0, 100)), c(0, 25, 50, 75, 100))
  b <- bins_quantile(4)(times)
  expect_equal(b, unname(stats::quantile(times, seq(0, 1, by = 0.25))))
  expect_equal(range(b), range(times))
})

test_that("bins_kmeans() breaks between sorted cluster centres and is reproducible", {
  b <- bins_kmeans(3)(times)
  expect_length(b, 4)
  expect_equal(range(b), range(times))
  expect_false(is.unsorted(b, strictly = TRUE))
  expect_identical(bins_kmeans(3)(times), b)
  ## More bins than distinct times are capped at the distinct times.
  expect_length(bins_kmeans(10)(c(1, 1, 2, 2, 5)), 4)
  expect_error(bins_kmeans(0), class = "pmxhelpr_error_bad_arg")
  expect_error(bins_quantile(2.5), class = "pmxhelpr_error_bad_arg")
  expect_error(bins_equal("a"), class = "pmxhelpr_error_bad_arg")
})

test_that("function bin_breaks keep every record and record the resolved breaks", {
  sim <- sim_pk(5)
  expect_no_warning(out <- df_vpcstats(sim, loq = 1, bin_breaks = bins_equal(5)))
  expect_equal(sum(out$stats$obs_n), sum(sim$SIM == 1 & sim$EVID == 0))
  obs_times <- sim$TIME[sim$SIM == 1 & sim$EVID == 0]
  expect_equal(out$config$bin_breaks, bins_equal(5)(obs_times))
  ## Outer breaks are widened to the data's time range.
  narrow <- df_vpcstats(sim, loq = 1, bin_breaks = function(x) c(10, 20, 30))
  expect_equal(range(narrow$config$bin_breaks), range(obs_times))
  expect_equal(sum(narrow$stats$obs_n), length(obs_times))
})

test_that("function bin_breaks run per stratum; numeric breaks are shared", {
  sim <- sim_pk(5, carry_out = "FOOD")
  sim$TIME[sim$FOOD == 1] <- sim$TIME[sim$FOOD == 1] / 2
  per <- df_vpcstats(sim, loq = 1, strat_var = FOOD, bin_breaks = bins_equal(4))
  expect_type(per$config$bin_breaks, "list")
  expect_named(per$config$bin_breaks, c("0", "1"))
  expect_false(isTRUE(all.equal(per$config$bin_breaks[["0"]], per$config$bin_breaks[["1"]])))
  for (lvl in c(0, 1)) {
    rows <- per$stats[per$stats$FOOD == lvl, ]
    expect_true(all(rows$BIN_LOW %in% per$config$bin_breaks[[as.character(lvl)]]))
  }
  shared <- df_vpcstats(sim, loq = 1, strat_var = FOOD, bin_breaks = c(0, 24, 200))
  expect_equal(shared$config$bin_breaks, c(0, 24, 200))
  expect_setequal(unique(shared$stats$BIN_MID), c(12, 112))
})

test_that("interval-binned stats carry BIN_LOW and BIN_HI around BIN_MID", {
  sim <- sim_pk(5)
  out <- df_vpcstats(sim, loq = 1, bin_breaks = bins_quantile(5))
  expect_equal(names(out$stats)[1:3], c("BIN_MID", "BIN_LOW", "BIN_HI"))
  expect_true(all(out$stats$BIN_LOW <= out$stats$BIN_MID & out$stats$BIN_MID <= out$stats$BIN_HI))
  expect_equal(out$stats$BIN_MID, (out$stats$BIN_LOW + out$stats$BIN_HI) / 2)
  expect_true(all(c("BIN_LOW", "BIN_HI") %in% summary(out)$column_groups$identifiers))
  nominal <- df_vpcstats(sim, loq = 1)
  expect_false(any(c("BIN_LOW", "BIN_HI") %in% names(nominal$stats)))
})

test_that("a bin_breaks function that returns too few breaks errors, naming the stratum", {
  sim <- sim_pk(5, carry_out = "FOOD")
  expect_error(df_vpcstats(sim, bin_breaks = function(x) 1), class = "pmxhelpr_error_bad_arg")
  expect_error(df_vpcstats(sim, strat_var = FOOD, bin_breaks = function(x) NA_real_),
               "for FOOD = 0")
})

test_that("bin_style = 'rect' draws simulated intervals as rectangles and keeps lines", {
  sim <- sim_pk(5)
  vs <- df_vpcstats(sim, loq = 1, bin_breaks = bins_quantile(5))
  roles_of <- function(p) unname(vapply(p$layers, function(l) attr(l, "ggstylekit_series_key") %||% NA_character_, ""))
  geoms_of <- function(p) unname(vapply(p$layers, function(l) class(l$geom)[1], ""))
  all_shown <- plot_vpc_shown(sim_pi_line = TRUE, sim_pi_area = TRUE, sim_median_line = TRUE)

  line_roles <- c("sim_pi_line", "sim_median_line", "obs_median_line", "obs_pi_line")
  interval_roles <- c("sim_pi_area", "sim_pi_ci", "sim_median_ci")

  line <- plot_vpc_cont(vs, shown = all_shown)
  rect <- plot_vpc_cont(vs, shown = all_shown, bin_style = "rect")
  rg <- geoms_of(rect)
  rr <- roles_of(rect)
  expect_true(all(rg[rr %in% interval_roles] == "GeomRect"))
  expect_false("GeomSegment" %in% rg)
  ## Each line keeps its lines and gains one point layer per line, under its role.
  for (role in line_roles) {
    expect_equal(sum(rg == "GeomLine" & rr == role), sum(geoms_of(line) == "GeomLine" & roles_of(line) == role))
    expect_equal(sum(rg == "GeomPoint" & rr == role), sum(rg == "GeomLine" & rr == role))
  }
  ## The points sit on the bin midpoints and take their line's colour.
  built <- ggplot2::ggplot_build(rect)
  med_line <- which(rr == "obs_median_line" & rg == "GeomLine")
  med_point <- which(rr == "obs_median_line" & rg == "GeomPoint")
  pts <- built$data[[med_point]]
  expect_setequal(pts$x[!is.na(pts$y)], vs$stats$BIN_MID[!is.na(vs$stats$obs_med)])
  expect_equal(unique(built$data[[med_point]]$colour), unique(built$data[[med_line]]$colour))
  ## Line style is unchanged: no point layers besides the observations.
  expect_equal(sum(geoms_of(line) == "GeomPoint"), 1)

  cens <- plot_vpc_cens(vs, bin_style = "rect", shown = all_shown)
  cg <- geoms_of(cens)
  cr <- roles_of(cens)
  expect_equal(cg[cr == "sim_median_ci"], "GeomRect")
  expect_equal(cg[cr == "sim_median_line"], c("GeomLine", "GeomPoint"))
  ## The obs BLQ line's midpoints are already marked by obs_point.
  expect_equal(cg[cr == "obs_median_line"], "GeomLine")
  expect_no_error(ggplot2::ggplot_build(cens))
  no_obs_pts <- plot_vpc_cens(vs, bin_style = "rect",
                              shown = plot_vpc_shown(obs_point = FALSE))
  expect_equal(geoms_of(no_obs_pts)[roles_of(no_obs_pts) == "obs_median_line"],
               c("GeomLine", "GeomPoint"))

  ## Raw-data path and builder/autoplot.
  expect_true("GeomRect" %in% geoms_of(plot_vpc_cont(sim, bin_breaks = c(0, 4, 24, 200),
                                                      bin_style = "rect")))
  expect_true("GeomRect" %in% geoms_of(plot_build_vpc(vs, bin_style = "rect")))
  expect_true("GeomRect" %in% geoms_of(ggplot2::autoplot(vs, bin_style = "rect")))
})

test_that("bin_style = 'rect' needs interval bins", {
  vs <- df_vpcstats(sim_pk(5))
  expect_error(plot_vpc_cont(vs, bin_style = "rect"), class = "pmxhelpr_error_bad_arg")
  expect_error(plot_build_vpc(vs, bin_style = "rect"), "needs interval bins")
  expect_error(plot_vpc_cont(vs, bin_style = "bars"))
})
