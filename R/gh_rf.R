#' Risk-free rate \eqn{r_{r,t}}
#'
#' @export
risk_free_rate <- function(lambda_t, pi_t, pc_t, params = ghaderi_params()) {
  th <- params$theta
  d <- params$delta
  g <- params$gamma
  mu_c <- params$mu_c
  sigma_c <- params$sigma_c
  Phi_Z <- params$Phi_Z
}
