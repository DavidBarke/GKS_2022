#' Price-dividend ratio
#'
#' @export
pd_t <- function(pd, lambda_t, pi_t, pc_grid, params = ghaderi_params()) {
  d <- params$delta
  g <- params$gamma
  phi <- params$phi
  mu_c <- params$mu_c
  s_c <- params$sigma_c
  th <- params$theta
  Phi_Z <- params$Phi_Z

  pc <- function(lambda_t, pi_t) {
    pracma::interp2(pc_grid$x, pc_grid$y, pc_grid$v, lambda_t, pi_t)
  }

  expectation <- expectation_eqn_9(
    pd = pd,
    pc = pc,
    lambda_t = lambda_t,
    pi_t = pi_t,
    params = params
  )

  th * log(d) + (phi - g) * mu_c + 1/2 * (phi - g)^2 * s_c^2 +
    lambda_t * (Phi_Z(phi - g) - 1) - (th - 1) * pc(lambda_t, pi_t) +
    log(expectation)
}



#' \eqn{E[(1 + \exp(pc_{t+1}))^{\theta - 1} (1 + \exp(pd_{t+1}))]}
#'
#' @export
expectation_eqn_9 <- function(
    pd, pc, lambda_t, pi_t, params = ghaderi_params()
) {
  cond_exp_1 <- conditional_expectation_eqn_9(
    pd = pd,
    pc = pc,
    s_t1 = 0,
    lambda_t = lambda_t,
    pi_t = pi_t,
    params = params
  )

  cond_exp_2 <- conditional_expectation_eqn_9(
    pd = pd,
    pc = pc,
    s_t1 = 1,
    lambda_t = lambda_t,
    pi_t = pi_t,
    params = params
  )

  (1 - pi_t1_t(pi_t, params)) * cond_exp_1 +
    pi_t1_t(pi_t, params) * cond_exp_2
}



#' Conditional expectation of equation 9
#'
#' @export
conditional_expectation_eqn_9 <- function(
    pd, pc, s_t1, lambda_t, pi_t, params = ghaderi_params()
) {
  theta <- params$theta
  s_l <- params$sigma_lambda
  l_L <- params$lambda_L
  l_H <- params$lambda_H

  pdf_l_t1 <- pdf_lambda_t1(s_t1, lambda_t, params)

  f <- function(lambda_t1) {
    pi_t1 <- pi_t1(
      pi_t = pi_t,
      lambda_t1 = lambda_t1,
      lambda_t = lambda_t
    )

    pd_t1 <- pd(lambda_t1, pi_t1)
    pd_t1[is.na(pd_t1)] <- 0

    pc_t1 <- pc(lambda_t1, pi_t1)
    pc_t1[is.na(pc_t1)] <- 0

    (1 + exp(pc_t1))^(theta - 1) *
      (1 + exp(pd_t1)) *
      pdf_l_t1(lambda_t1)
  }

  bounds <- q_lambda_t1(
    p = c(0.001, 0.999),
    s_t1 = s_t1,
    lambda_t = lambda_t,
    params = params
  )

  int <- integrate(f, bounds[1], bounds[2])
  int$value
}
