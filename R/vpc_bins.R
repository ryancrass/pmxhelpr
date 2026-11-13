#' Binning functions for interval-binned VPCs
#'
#' @description
#' These return a binning function for the `bin_breaks` argument of
#' [df_vpcstats()], [plot_vpc_cont()], and [plot_vpc_cens()]. Each gives at
#' most `n` time-interval bins, placed from the observation times:
#'
#' * `bins_kmeans()` clusters the times (k-means) and breaks halfway between
#'   neighbouring cluster centres, so sampling clusters stay together. With
#'   `log_time = TRUE` the clustering is on log time, which suits sampling
#'   that is dense early and sparse late.
#' * `bins_quantile()` gives bins with about equal numbers of observations.
#' * `bins_equal()` gives bins of equal width.
#'
#' With `strat_var`, the binning function runs on each stratum's times, so
#' the bins can differ by stratum. The outer breaks are widened to the
#' data's time range, so no record is dropped.
#'
#' @param n Maximum number of bins. Tied times can give fewer. When `n` is
#'    not below the number of distinct times, each time is its own bin, with a
#'    warning.
#' @param seed Random seed for the k-means starting centres, so the bins are
#'    reproducible. Default is `123456789`.
#' @param log_time Cluster on log time? Default is `FALSE`. Times at or below
#'    zero form their own bin, counted in `n`, and the positive times are
#'    clustered into the rest.
#'
#' @return A function of a numeric vector of times that returns the bin
#'    breaks.
#' @family vpc
#' @export
#'
#' @examples
#' times <- c(0.5, 1, 1, 2, 4, 4, 8, 24, 24, 48, 72, 168)
#' bins_kmeans(4)(times)
#' bins_kmeans(4, log_time = TRUE)(times)
#' bins_quantile(4)(times)
#' bins_equal(4)(times)
#'
#' model <- model_mread_load(model = "pkmodel")
#' simout <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT %in% c(1, 2)),
#'                               model = model, replicates = 10,
#'                               dv_var = ODV, carry_out = "FOOD")
#' plot_vpc_cont(simout, strat_var = FOOD, bin_breaks = bins_kmeans(6),
#'               bin_style = "rect")

bins_kmeans <- function(n, seed = 123456789, log_time = FALSE) {
  check_positive_integer(n, "n")
  check_integer(seed, "seed")
  check_boolean(log_time, "log_time")
  function(x) {
    edges <- range(x)
    ## On log time, times at or below zero have no log: they form the first
    ## bin, closed by `lead` (the earliest positive time), and the positive
    ## times are clustered into the remaining bins.
    lead <- NULL
    if (log_time) {
      positive <- x[x > 0]
      if (length(positive) == 0) return(edges)
      if (edges[1] <= 0) lead <- min(positive)
      x <- log(positive)
    }
    distinct <- sort(unique(x))
    k <- min(n - length(lead), length(distinct))
    if (k < 1) return(edges)
    if (k < 2) return(c(edges[1], lead, edges[2]))
    ## With as many clusters as distinct times there is nothing to cluster:
    ## each time is its own bin (kmeans() also needs fewer centres than rows).
    if (k == length(distinct)) {
      pmx_warn(c(
        "{.code n = {n}} is not below the number of distinct times, so nothing is clustered: each time is its own bin.",
        "i" = "Use a smaller {.arg n} to group times into bins."
      ), "ignored_arg")
      centers <- distinct
    } else {
      fit <- withr::with_seed(seed, stats::kmeans(x, centers = k, nstart = 25))
      centers <- sort(unname(fit$centers[, 1]))
    }
    mids <- (centers[-k] + centers[-1]) / 2
    if (log_time) mids <- exp(mids)
    c(edges[1], lead, mids, edges[2])
  }
}


#' @rdname bins_kmeans
#' @export
bins_quantile <- function(n) {
  check_positive_integer(n, "n")
  function(x) unname(stats::quantile(x, seq(0, 1, length.out = n + 1)))
}


#' @rdname bins_kmeans
#' @export
bins_equal <- function(n) {
  check_positive_integer(n, "n")
  function(x) seq(min(x), max(x), length.out = n + 1)
}
