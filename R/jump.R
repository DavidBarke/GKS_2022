#' \eqn{J_{t+1}(\lambda_t)}
#'
#' @param lambda_t Jump intensity at time \eqn{t}.
#'
#' @export
J_t1 <- function(lambda_t, params = ghaderi_params()) {
  N_t1 <- rpois(1, lambda_t)
  Z_j <- -rexp(N_t1, 1 / params$mu_Z)
  sum(Z_j)
}
