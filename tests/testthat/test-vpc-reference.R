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


#####BLQ/LOQ paths against reference computations#####

## Censored variant of ref_sim: per-row LLOQ (1 for odd IDs, 2 for even IDs),
## two MDV = 1 records per bin (ID 3 keeps a quantifiable-looking value so only
## MDV flags it; ID 4 is NA), ID 5 observed exactly at its LLOQ before
## NTIME 12 (quantifiable, not BLQ), and an NTIME = 12 bin whose observations
## are all below LLOQ.
ref_blq <- local({
  set.seed(42)
  base <- expand.grid(ID = 1:20, NTIME = c(1, 2, 4, 12))
  base$TIME <- base$NTIME
  base$PRED <- 10 * exp(-0.3 * base$NTIME) * (1 + 0.5 * (base$ID %% 4))
  base$OBSDV <- base$PRED * exp(stats::rnorm(nrow(base), 0, 0.2))
  base$LLOQ <- ifelse(base$ID %% 2 == 1, 1, 2)
  base$MDV <- ifelse(base$ID %in% c(3, 4), 1, 0)
  base$OBSDV[base$ID == 3] <- 50
  base$OBSDV[base$ID == 4] <- NA
  at_lloq <- base$ID == 5 & base$NTIME != 12
  base$OBSDV[at_lloq] <- base$LLOQ[at_lloq]
  reps <- lapply(1:50, function(r) {
    d <- base
    d$SIM <- r
    d$SIMDV <- d$PRED * exp(stats::rnorm(nrow(d), 0, 0.3))
    d
  })
  out <- do.call(rbind, reps)
  out$EVID <- 0
  out
})
blq_r1 <- ref_blq[ref_blq$SIM == 1, ]

# Reference BLQ flag, rank-mode and drop-mode quantiles
blq_ref <- function(x, loq, mdv) is.na(x) | x < loq | mdv == 1
q_rank <- function(x, blq, p) {
  q <- unname(stats::quantile(ifelse(blq, -Inf, x), p))
  if (is.infinite(q)) NA_real_ else q
}
q_drop <- function(x, blq, p) unname(stats::quantile(x[!blq], p))

# Per-bin statistic of a function applied to (x, blq) pairs
by_bin2 <- function(x, blq, bin, f) {
  unname(sapply(split(seq_along(x), bin), function(i) f(x[i], blq[i])))
}

# Two-stage summary: `stat` per replicate x bin, then quantile `p` across replicates
two_stage_ref <- function(x, bin, rep, stat, p) {
  per_rep <- tapply(x, list(bin, rep), stat)
  unname(apply(per_rep, 1, function(v) stats::quantile(v, p, na.rm = TRUE)))
}

vpc_blq <- function(...) suppressMessages(df_vpcstats(ref_blq, ...))

r1_blq <- blq_ref(blq_r1$OBSDV, blq_r1$LLOQ, blq_r1$MDV)

test_that("ref_blq covers partly censored, fully censored, and both LLOQ values", {
  frac <- tapply(r1_blq, blq_r1$NTIME, mean)
  expect_true(any(frac > 0 & frac < 1))
  expect_equal(unname(frac[["12"]]), 1)
  expect_setequal(unique(ref_blq$LLOQ), c(1, 2))
})

test_that("BLQ counts use each row's LLOQ and include MDV = 1 records", {
  out <- vpc_blq()
  s <- out$stats
  expect_ref(s$obs_n,        as.vector(table(blq_r1$NTIME)))
  expect_ref(s$obs_n_blq,    as.vector(tapply(r1_blq, blq_r1$NTIME, sum)))
  expect_ref(s$obs_prop_blq, as.vector(tapply(r1_blq, blq_r1$NTIME, mean)))
  expect_equal(out$config$loq, c(1, 2))
})

test_that("rank mode: observed quantiles rank BLQ records lowest; censored quantiles are NA", {
  s <- vpc_blq()$stats
  expect_ref(s$obs_low, by_bin2(blq_r1$OBSDV, r1_blq, blq_r1$NTIME, function(x, b) q_rank(x, b, 0.05)))
  expect_ref(s$obs_med, by_bin2(blq_r1$OBSDV, r1_blq, blq_r1$NTIME, function(x, b) q_rank(x, b, 0.5)))
  expect_ref(s$obs_hi,  by_bin2(blq_r1$OBSDV, r1_blq, blq_r1$NTIME, function(x, b) q_rank(x, b, 0.95)))
  expect_true(is.na(s$obs_hi[s$BIN_MID == 12]))
})

test_that("drop mode: observed quantiles use quantifiable records only", {
  s <- vpc_blq(mode = "drop")$stats
  partial <- s$BIN_MID != 12
  ref_low <- by_bin2(blq_r1$OBSDV, r1_blq, blq_r1$NTIME, function(x, b) q_drop(x, b, 0.05))
  ref_med <- by_bin2(blq_r1$OBSDV, r1_blq, blq_r1$NTIME, function(x, b) q_drop(x, b, 0.5))
  expect_ref(s$obs_low[partial], ref_low[partial])
  expect_ref(s$obs_med[partial], ref_med[partial])
  expect_true(is.na(s$obs_med[!partial]))
})

test_that("sim_prop_blq_* are the ci quantiles of per-replicate BLQ proportions", {
  s <- vpc_blq()$stats
  sim_blq <- ref_blq$SIMDV < ref_blq$LLOQ | is.na(ref_blq$SIMDV)
  ref_p <- function(p) two_stage_ref(sim_blq, ref_blq$NTIME, ref_blq$SIM, mean, p)
  expect_ref(s$sim_prop_blq_low, ref_p(0.05))
  expect_ref(s$sim_prop_blq_med, ref_p(0.5))
  expect_ref(s$sim_prop_blq_hi,  ref_p(0.95))
})

test_that("pcVPC censors BLQ before prediction correction and masks fully censored bins", {
  s <- vpc_blq()$stats
  obs_c <- ifelse(blq_ref(ref_blq$OBSDV, ref_blq$LLOQ, ref_blq$MDV), NA_real_, ref_blq$OBSDV)
  sim_c <- ifelse(is.na(ref_blq$SIMDV) | ref_blq$SIMDV < ref_blq$LLOQ, NA_real_, ref_blq$SIMDV)
  pc_obs <- pc_ref(obs_c, ref_blq$PRED, ref_blq$NTIME)
  pc_sim <- pc_ref(sim_c, ref_blq$PRED, ref_blq$NTIME)
  r1 <- ref_blq$SIM == 1
  partial <- s$BIN_MID != 12
  ref_obs_med <- by_bin(pc_obs[r1], ref_blq$NTIME[r1],
                        function(v) stats::quantile(v, 0.5, na.rm = TRUE))
  ref_sim_med <- two_stage_ref(pc_sim, ref_blq$NTIME, ref_blq$SIM,
                               function(v) stats::quantile(v, 0.5, na.rm = TRUE), 0.5)
  expect_ref(s$pc_obs_med[partial], ref_obs_med[partial])
  expect_ref(s$pc_sim_med_med[partial], ref_sim_med[partial])
  pc_sim_cols <- paste0("pc_", pmxhelpr:::.vpc_sim_quantile_cols)
  expect_true(all(is.na(as.matrix(s[!partial, pc_sim_cols]))))
  expect_false(is.na(s$sim_med_med[!partial]))
})

test_that("a scalar `loq` overrides the per-row LLOQ column", {
  s <- vpc_blq(loq = 1.5)$stats
  blq15 <- blq_ref(blq_r1$OBSDV, 1.5, blq_r1$MDV)
  expect_ref(s$obs_n_blq, as.vector(tapply(blq15, blq_r1$NTIME, sum)))
  sim_blq <- ref_blq$SIMDV < 1.5 | is.na(ref_blq$SIMDV)
  expect_ref(s$sim_prop_blq_med, two_stage_ref(sim_blq, ref_blq$NTIME, ref_blq$SIM, mean, 0.5))
})
