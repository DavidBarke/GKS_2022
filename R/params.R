#' @export
model_params <- function(
    theta = 4,
    sigma = 0.02,
    rho = 0.03,
    gamma = 0.025,
    p = 0.017,
    q = 0.4
) {
  list(
    theta = theta,
    sigma = sigma,
    rho = rho,
    gamma = gamma,
    p = p,
    q = q
  )
}
