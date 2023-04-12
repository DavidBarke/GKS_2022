#' Risk-free rate \eqn{r_{r,t}}
#'
#' @inheritParams pd_t
#'
#' @export
risk_free_rate <- function(lambda_t, pi_t, pc_grid, params = ghaderi_params()) {
  th <- params$theta
  d <- params$delta
  g <- params$gamma
  mu_c <- params$mu_c
  sigma_c <- params$sigma_c
  Phi_Z <- params$Phi_Z

  pc <- grid_to_function(pc_grid)

  expectation <- 1 # TODO

  - th * log(d) + g * mu_c - 1/2 * g^2 * mu_c^2 -
    lambda_t * (Phi_Z(-g) - 1) + (th - 1) * pc(lambda_t, pi_t) -
    log(expectation)
}
