#####VPC statistics against reference computations#####

## Small synthetic replicate set, so every expected value can be computed
## directly. PRED varies within and between bins, so per-bin and global
## prediction correction differ; all values are quantifiable (no LOQ).
ref_sim <- local({
  set.seed(42)
  base <- expand.grid(ID = 1:20, NTIME = c(1, 2, 4))
  base$TIME <- base$NTIME
  base$PRED <- 10 * exp(-0.3 * base$NTIME) * (1 + 0.5 * (base$ID %% 4))
  base$OBSDV <- base$PRED * exp(stats::rnorm(nrow(base), 0, 0.2))
  reps <- lapply(1:50, function(r) {
    d <- base
    d$SIM <- r
    d$SIMDV <- d$PRED * exp(stats::rnorm(nrow(d), 0, 0.3))
    d
  })
  out <- do.call(rbind, reps)
  out$MDV <- 0
  out$EVID <- 0
  out
})

# Stats columns keep quantile() names ("5%"); compare values only
expect_ref <- function(actual, expected) expect_equal(unname(actual), unname(expected))

# Per-bin reference values, in BIN_MID order
by_bin <- function(x, bin, f) unname(sapply(split(x, bin), f))

# Prediction correction within each bin, written out independently
pc_ref <- function(x, pred, bin, lb = 0) {
  med <- ave(pred, bin, FUN = stats::median)
  lb + (x - lb) * (med - lb) / (pred - lb)
}

test_that("observed quantiles equal per-bin quantile() of the first replicate", {
  s <- df_vpcstats(ref_sim)$stats
  r1 <- ref_sim[ref_sim$SIM == 1, ]
  expect_ref(s$obs_low, by_bin(r1$OBSDV, r1$NTIME, function(v) stats::quantile(v, 0.05)))
  expect_ref(s$obs_med, by_bin(r1$OBSDV, r1$NTIME, stats::median))
  expect_ref(s$obs_hi,  by_bin(r1$OBSDV, r1$NTIME, function(v) stats::quantile(v, 0.95)))
})

test_that("`pi` is forwarded to the observed and simulated quantiles", {
  s <- df_vpcstats(ref_sim, pi = c(0.1, 0.9))$stats
  r1 <- ref_sim[ref_sim$SIM == 1, ]
  expect_equal(unique(s$pi_low), 0.1)
  expect_ref(s$obs_low, by_bin(r1$OBSDV, r1$NTIME, function(v) stats::quantile(v, 0.1)))
  expect_ref(s$obs_hi,  by_bin(r1$OBSDV, r1$NTIME, function(v) stats::quantile(v, 0.9)))
})

test_that("simulated CIs are the `ci` quantiles of the per-replicate statistics", {
  two_stage <- function(stat, p) {
    per_rep <- stats::aggregate(SIMDV ~ NTIME + SIM, ref_sim, stat)
    by_bin(per_rep$SIMDV, per_rep$NTIME, function(v) stats::quantile(v, p))
  }
  s <- df_vpcstats(ref_sim)$stats
  expect_ref(s$sim_med_low, two_stage(stats::median, 0.05))
  expect_ref(s$sim_med_hi,  two_stage(stats::median, 0.95))
  expect_ref(s$sim_low_low, two_stage(function(v) stats::quantile(v, 0.05), 0.05))
  expect_ref(s$sim_hi_hi,   two_stage(function(v) stats::quantile(v, 0.95), 0.95))
  s80 <- df_vpcstats(ref_sim, ci = 0.8)$stats
  expect_ref(s80$sim_med_low, two_stage(stats::median, 0.1))
})

test_that("prediction correction normalizes by the bin's median PRED, not the global one", {
  s <- df_vpcstats(ref_sim)$stats
  r1 <- ref_sim[ref_sim$SIM == 1, ]
  pc_all <- pc_ref(ref_sim$OBSDV, ref_sim$PRED, ref_sim$NTIME)
  pc_r1 <- pc_all[ref_sim$SIM == 1]
  expect_ref(s$pc_obs_med, by_bin(pc_r1, r1$NTIME, stats::median))
  pc_sim <- data.frame(NTIME = ref_sim$NTIME, SIM = ref_sim$SIM,
                       PC = pc_ref(ref_sim$SIMDV, ref_sim$PRED, ref_sim$NTIME))
  per_rep <- stats::aggregate(PC ~ NTIME + SIM, pc_sim, stats::median)
  expect_ref(s$pc_sim_med_med, by_bin(per_rep$PC, per_rep$NTIME, stats::median))
})

test_that("`lower_bound` is forwarded to prediction correction", {
  s <- df_vpcstats(ref_sim, lower_bound = -1)$stats
  r1 <- ref_sim[ref_sim$SIM == 1, ]
  pc_r1 <- pc_ref(ref_sim$OBSDV, ref_sim$PRED, ref_sim$NTIME, lb = -1)[ref_sim$SIM == 1]
  expect_ref(s$pc_obs_med, by_bin(pc_r1, r1$NTIME, stats::median))
  expect_false(isTRUE(all.equal(s$pc_obs_med, df_vpcstats(ref_sim)$stats$pc_obs_med)))
})

test_that("pcVPC observed points carry prediction-corrected values", {
  obs <- df_vpcstats(ref_sim)$obs
  r1 <- ref_sim[ref_sim$SIM == 1, ]
  r1$PC_REF <- pc_ref(ref_sim$OBSDV, ref_sim$PRED, ref_sim$NTIME)[ref_sim$SIM == 1]
  m <- merge(obs, r1[, c("ID", "NTIME", "PC_REF")],
             by.x = c("ID", "BIN_MID"), by.y = c("ID", "NTIME"))
  expect_equal(nrow(m), nrow(r1))
  expect_equal(m$PC_OBSDV, m$PC_REF)
})
