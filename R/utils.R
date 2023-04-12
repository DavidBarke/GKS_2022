#' Price of one-period equity claim (eq. 8)
#'
#' @export
P_t1 <- function(A_t, rho, theta, gamma, sigma, b) {

}

#' Empirical distribution of b
#'
#' @param trend_adjusted Use trend-adjusted contraction data
#'
#' @export
b_freq <- function(trend_adjusted = FALSE) {
  if (trend_adjusted) {
    tibble::tibble(
      contraction = seq(0.17, 0.67, by = 0.05),
      freq = c(2, 12, 14, 7, 9, 5, 1, 2, 4, 0, 4)
    )
  } else {
    tibble::tibble(
      contraction = seq(0.17, 0.67, by = 0.05),
      freq = c(20,13, 3, 9, 5, 0, 2, 3, 3, 2, 0)
    )
  }
}

#' Expected value of transformation of b
#'
#' @param transform Function taking frequencies of b as input. `identity`
#' computes the expected value of b.
#' @inheritParams b_freq
#'
#' @export
E_b_trans <- function(transform = identity, trend_adjusted = FALSE) {
  tbl <- b_freq(trend_adjusted)
  weighted.mean(transform(tbl$contraction), tbl$freq)
}
