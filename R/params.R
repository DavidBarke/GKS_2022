#' Parameters for Ghaderi et al. (2022)
#'
#' @param gamma Relative risk aversion
#' @param psi Elasticity of intertemporal substitution
#' @param delta Time discount
#' @param mu_c Mean consumption growth in the absence of jumps
#' @param sigma_c Volatility of consumption growth in the absence of jumps
#' @param phi Leverage parameter
#' @param sigma_lambda Conditional volatility of shocks to jump intensity
#' @param lambda_L Average jump intensity in normal times
#' @param lambda_H Average jump intensity in depression times
#' @param mu_Z Mean jump size
#' @param p_01 Transition probability from normal state to depression state
#' @param p_10 Transition probability from depression state to normal state
#'
#' @export
ghaderi_params <- function(
    gamma = 5,
    psi = 1.5,
    delta = 0.9990,
    mu_c = 0.0026,
    sigma_c = 0.0020,
    phi = 3,
    rho_lambda = 0.9933,
    sigma_lambda = 0.0083,
    lambda_L = 0.0417,
    lambda_H = 0.4167,
    mu_Z = 0.0200,
    p_01 = 0.0017,
    p_10 = 0.0208
) {
  list(
    gamma = gamma,
    psi = psi,
    delta = delta,
    mu_c = mu_c,
    sigma_c = sigma_c,
    phi = phi,
    rho_lambda = rho_lambda,
    sigma_lambda = sigma_lambda,
    lambda_L = lambda_L,
    lambda_H = lambda_H,
    mu_Z = mu_Z,
    p_01 = p_01,
    p_10 = p_10,
    p_00 = 1 - p_01,
    p_11 = 1 - p_10,
    theta = (1 - gamma) / (1 - 1 / psi),
    long_run_mean_lambda_t1 = function(s_t1) {
      (1 - s_t1) * lambda_L + s_t1 * lambda_H
    },
    Phi_Z = function(x) {
      - 1 / (1 - mu_Z * x)
    }
  )
}
