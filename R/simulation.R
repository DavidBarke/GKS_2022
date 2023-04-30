#' Run a simulation of the model
#'
#' @param s_t State variable \eqn{s_t}. Vector of zeros and ones.
#' @inheritParams pi_t1
#'
#' @export
simulation <- function(s_t, pc_grid, pd_grid, params = ghaderi_params()) {
  stopifnot(all(s_t %in% c(0, 1)))
  stopifnot(length(s_t) >= 2)

  r_l <- params$rho_lambda
  s_l <- params$sigma_lambda
  mu_c <- params$mu_c
  s_c <- params$sigma_c
  phi <- params$phi

  # n: Number of discrete time periods
  n <- length(s_t)

  long_run_mean_lambda_t <- params$long_run_mean_lambda_t1(s_t)

  # Use precomputed values for pc(.) and pd(.)
  pc <- grid_to_function(pc_grid)
  pd <- grid_to_function(pd_grid)

  lambda_t <- numeric(n)
  delta_c_t <- numeric(n)
  pi_t <- numeric(n)
  C_t <- numeric(n)

  lambda_t[1] <- long_run_mean_lambda_t[1]
  delta_c_t[1] <- 0
  pi_t[1] <- s_t[1]
  C_t[1] <- exp(pd(lambda_t[1], pi_t[1]))^(-1/phi)

  shock_lambda_t <- rnorm(n)
  shock_c_t <- rnorm(n)

  for (i in 2:n) {
    lambda_t[i] <-
      (1 - r_l) * long_run_mean_lambda_t[i] +
      r_l * lambda_t[i-1] +
      s_l * sqrt(lambda_t[i-1]) * shock_lambda_t[i]

    pi_t[i] <- pi_t1(
      pi_t = pi_t[i-1],
      lambda_t1 = lambda_t[i],
      lambda_t = lambda_t[i-1],
      params = params
    )

    delta_c_t[i] <-
      mu_c +
      s_c * shock_c_t[i] +
      J_t1(lambda_t[i])

    C_t[i] <- C_t[i-1] * exp(delta_c_t[i])
  }

  list(
    data = tibble(
      t = seq_along(s_t),
      s_t = s_t,
      lambda_t = lambda_t,
      pi_t = pi_t,
      delta_c_t = delta_c_t,
      shock_lambda_t = shock_lambda_t,
      shock_c_t = shock_c_t,
      J_t = delta_c_t - mu_c - s_c * shock_c_t,
      C_t = C_t,
      pc_t = pc(lambda_t, pi_t),
      pd_t = pd(lambda_t, pi_t),
      PC_t = exp(pc_t),
      PD_t = exp(pd_t),
      P_C_t = PC_t * C_t,
      P_D_t = PD_t * C_t^params$phi
    ),
    params = params
  )
}
