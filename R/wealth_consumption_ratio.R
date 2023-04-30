#' Wealth-consumption ratio
#'
#' @param pc Wealth-consumption value function
#' @inheritParams pi_t1
#'
#' @export
pc_t <- function(pc, lambda_t, pi_t, params = ghaderi_params()) {
  d <- params$delta
  g <- params$gamma
  mu_c <- params$mu_c
  s_c <- params$sigma_c
  th <- params$theta
  Phi_Z <- params$Phi_Z

  expectation <- expectation_eqn_8(
    pc = pc,
    lambda_t = lambda_t,
    pi_t = pi_t,
    params = params
  )

  1/th * (
    th * log(d) + (1 - g) * mu_c + 1/2 * (1 - g)^2 * s_c^2 +
      lambda_t * (Phi_Z(1 - g) - 1) + log(expectation)
  )
}



#' \eqn{E[(1 + \exp(pc))^\theta]}
#'
#' @inheritParams pc_t
#'
#' @export
expectation_eqn_8 <- function(pc, lambda_t, pi_t, params = ghaderi_params()) {
  cond_exp_1 <- conditional_expectation_eqn_8(
    pc = pc,
    s_t1 = 0,
    lambda_t = lambda_t,
    pi_t = pi_t,
    params = params
  )

  cond_exp_2 <- conditional_expectation_eqn_8(
    pc = pc,
    s_t1 = 1,
    lambda_t = lambda_t,
    pi_t = pi_t,
    params = params
  )

  (1 - pi_t1_t(pi_t, params)) * cond_exp_1 +
    pi_t1_t(pi_t, params) * cond_exp_2
}



#' Conditional expectation of equation 8
#'
#' @param s_t1 \eqn{s_t = 0}: normal state; \eqn{s_t = 1}: disaster state
#' @inheritParams pc_t
#'
#' @export
conditional_expectation_eqn_8 <- function(
    pc, s_t1, lambda_t, pi_t, params = ghaderi_params()
) {
  theta <- params$theta

  pdf_l_t1 <- pdf_lambda_t1(s_t1, lambda_t, params)

  f <- function(lambda_t1) {
    pi_t1 <- pi_t1(
      pi_t = pi_t,
      lambda_t1 = lambda_t1,
      lambda_t = lambda_t
    )

    pc_t1 <- pc(lambda_t1, pi_t1)
    #if (any(is.na(pc_t1))) print("NA")
    pc_t1[is.na(pc_t1)] <- 0

    (1 + exp(pc_t1))^theta *
      pdf_l_t1(lambda_t1)
  }

  bounds <- q_lambda_t1(
    p = c(0.000001, 0.999999),
    s_t1 = s_t1,
    lambda_t = lambda_t,
    params = params
  )

  int <- integrate(f, bounds[1], bounds[2], abs.tol = 1e-12, subdivisions = 1e3L)

  int$value
}
