#' Posterior belief
#'
#' @param pi_t Prior belief
#' @param lambda_t1 Current intensity
#' @param lambda_t Prior intensity
#' @param params List of parameters as returned by [ghaderi_params]
#'
#' @export
pi_t1 <- function(pi_t, lambda_t1, lambda_t, params = ghaderi_params()) {
  r_l <- params$rho_lambda
  l_L <- params$lambda_L
  l_H <- params$lambda_H
  s_l <- params$sigma_lambda

  term_i <- exp(
    - (1 - r_l) *
      (l_H - l_L) *
      (lambda_t1 - r_l * lambda_t - (1 - r_l) * (l_H + l_L) / 2) /
      (s_l^2 * lambda_t)
  )

  term_ii <- (1 - pi_t1_t(pi_t, params)) / pi_t1_t(pi_t, params)

  1 / (1 + term_i * term_ii)
}



#' Probability of being in the depression state at t+1 given time-t information
#'
#' @param pi_t Probability of being in the depression state at time t
#' @inheritParams pi_t1
#'
#' @export
pi_t1_t <- function(pi_t, params = ghaderi_params()) {
  p_00 <- params$p_00
  p_11 <- params$p_11
  1 - p_00 + (p_00 + p_11 - 1) * pi_t
}
