#' Binning functions for interval-binned VPCs
#'
#' @description
#' These return a binning function for the `bin_breaks` argument of
#' [df_vpcstats()], [plot_vpc_cont()], and [plot_vpc_cens()]. Each gives at
#' most `n` time-interval bins, placed from the observation times:
#'
#' * `bins_kmeans()` clusters the times (k-means) and breaks halfway between
#'   neighbouring cluster centres, so sampling clusters stay together.
#' * `bins_quantile()` gives bins with about equal numbers of observations.
#' * `bins_equal()` gives bins of equal width.
#'
#' With `strat_var`, the binning function runs on each stratum's times, so
#' the bins can differ by stratum. The outer breaks are widened to the
#' data's time range, so no record is dropped.
#'
#' @param n Maximum number of bins. Tied times can give fewer.
#' @param seed Random seed for the k-means starting centres, so the bins are
#'    reproducible. Default is `123456789`.
#'
#' @return A function of a numeric vector of times that returns the bin
#'    breaks.
#' @family vpc
#' @export
#'
#' @examples
#' times <- c(0.5, 1, 1, 2, 4, 4, 8, 24, 24, 48, 72, 168)
#' bins_kmeans(4)(times)
#' bins_quantile(4)(times)
#' bins_equal(4)(times)
#'
#' model <- model_mread_load(model = "pkmodel")
#' simout <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT %in% c(1, 2)),
#'                               model = model, replicates = 10,
#'                               dv_var = ODV, carry_out = "FOOD")
#' plot_vpc_cont(simout, strat_var = FOOD, bin_breaks = bins_kmeans(6),
#'               bin_style = "rect")

bins_kmeans <- function(n, seed = 123456789) {
  check_positive_integer(n, "n")
  check_integer(seed, "seed")
  function(x) {
    k <- min(n, length(unique(x)))
    if (k < 2) return(range(x))
    fit <- withr::with_seed(seed, stats::kmeans(x, centers = k, nstart = 25))
    centers <- sort(fit$centers[, 1])
    c(min(x), (centers[-k] + centers[-1]) / 2, max(x))
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
